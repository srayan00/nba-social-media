library(tidyverse)
library(rpart)
init_draft_tree <- rpart(formula = avg_game_score ~ .,
                       data = draft_data_combined, method  = "anova")
init_draft_tree

#Decision Tree
library(caret)
set.seed(2000)
caret_mlb_tree <- train(avg_game_score ~ sentiment_week + wiki_per_100 + avg_yt_hits + avg_web_hits + avg_news_hits + avg_image_hits ,
                        data = draft_data_combined, method = "rpart",
                        trControl = trainControl(method = "cv", number = 10),
                        tuneLength = 20)
ggplot(caret_mlb_tree) + theme_bw()
library(rpart.plot)
library(vip)
rpart.plot(caret_mlb_tree$finalModel)
vip(caret_mlb_tree, bar = FALSE, color = "#2151a1") + theme_bw()
plotcp(caret_mlb_tree$finalModel)

library(pdp)
partial(caret_mlb_tree, pred.var = c("avg_yt_hits", "wiki_per_100")) %>% autoplot() + theme_bw()

#Random Forest
rf_tune_grid <- 
  expand.grid(mtry = seq(1, 6, by = 1), 
              splitrule = "variance",
              min.node.size = 3)
set.seed(1917)
model_list <- list()
for (n_tree in c(40, 45, 50, 55, 60, 65, 70, 75, 80)) {
  set.seed(2000)
  caret_mlb_rf <- 
    train(avg_game_score ~ sentiment_week + wiki_per_100 + avg_yt_hits + avg_web_hits + avg_news_hits + avg_image_hits,
          data = draft_data_combined,
          method = "ranger", num.trees = n_tree,
          trControl = trainControl(method = "cv", number = 50),
          tuneGrid = rf_tune_grid,
          importance = "impurity")
  key <- toString(n_tree)
  model_list[[key]] <- caret_mlb_rf
  
}
results <- resamples(model_list)
summary(results)
for (val in c("40", "45", "50", "55", "60", "65", "70", "75", "80")) {
  print(sqrt(model_list[[val]]$finalModel$prediction.error))
}
model_list[["45"]]$finalModel
plot(model_list[["45"]])
library(vip)
vip(model_list[["45"]]$finalModel, geom = "point") + theme_bw()


#XGBoost
library(xgboost)
xgboost_tune_grid <- expand.grid(nrounds = seq(from = 20, to = 200, by = 20),
                                 eta = c(0.025, 0.05, 0.1, 0.3), gamma = 0,
                                 max_depth = c(1, 2, 3, 4), colsample_bytree = 1,
                                 min_child_weight = 1, subsample = 1)
xgboost_tune_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
set.seed(2000)
xgb_tune <- train(x = as.matrix(dplyr::select(draft_data_combined, c(sentiment_week, wiki_per_100, avg_web_hits,
                                                                     avg_image_hits, avg_news_hits, avg_yt_hits))),
                  y = draft_data_combined$avg_game_score, trControl = xgboost_tune_control,
                  tuneGrid = xgboost_tune_grid, method = "xgbTree", verbose = TRUE,
                  objective = "reg:squarederror")
xgb_tune$bestTune
min(xgb_tune$results$RMSE)
plot(xgb_tune)
xgb_fit_final <- xgboost(data = as.matrix(dplyr::select(draft_data_combined, c(sentiment_week, wiki_per_100, avg_web_hits,
                                                                               avg_image_hits, avg_news_hits, avg_yt_hits))),
                         label = draft_data_combined$avg_game_score, objective = "reg:squarederror",
                         nrounds = xgb_tune$bestTune$nrounds,
                         params = as.list(dplyr::select(xgb_tune$bestTune,
                                                        -nrounds)), 
                         verbose = 0)
xgb_fit_final$evaluation_log %>% 
  ggplot(aes(x = iter, y = train_rmse)) +
  geom_point(alpha = 0.5) +
  theme_bw()
library(DiagrammeR)
xgb.plot.tree(model = xgb_fit_final)
vip(xgb_fit_final, bar = FALSE) + theme_bw()

library(pdp)
partial(xgb_fit_final, pred.var = "sentiment_week",
        train = as.matrix(dplyr::select(draft_data_combined, c(sentiment_week, wiki_per_100, avg_web_hits,
                                                               avg_image_hits, avg_news_hits, avg_yt_hits))))  %>% autoplot() + theme_bw()
#Modeling of clusters
set.seed(2000)
train_index <- sample(1:nrow(draft_data_combined), 0.7 * nrow(draft_data_combined))
test_index <- setdiff(1:nrow(draft_data_combined), train_index)
draft_data_combined <- draft_data_combined %>% 
  mutate(boostCluster = as.numeric(player_hc_clusters) - 1)
# Build X_train, y_train, X_test, y_test
X_train <- draft_data_combined[train_index, c("numGames", "avg_minutes", "avg_pct_fg3", "avg_pct_fg2", 
                                              "win_percent", "avg_treb", "avg_pts", "avg_pf", "avg_pctFT")]
y_train <- draft_data_combined[train_index, "boostCluster"]
train_matrix = xgb.DMatrix(data = as.matrix(X_train), label = y_train)

X_test <- draft_data_combined[test_index, c("numGames", "avg_minutes", "avg_pct_fg3", "avg_pct_fg2", 
                                            "win_percent", "avg_treb", "avg_pts", "avg_pf", "avg_pctFT")]
y_test <- draft_data_combined[test_index, "boostCluster"]
test_matrix = xgb.DMatrix(data = as.matrix(X_test), label = y_test)
library(caret)
library(xgboost)

set.seed(2000)
xgb_fit <- xgboost(data = train_matrix, 
                   objective = "binary:logistic",
                   nrounds = 100,
                   eta = 0.0025,
                   eval_metric = "auc",
                   scale_pos_weight = 10.44)
vip(xgb_fit, bar = FALSE) + theme_bw()
xgb_test_pred <- predict(xgb_fit, newdata = test_matrix)
xgb_0_1 <- ifelse(xgb_test_pred > 0.1, 1, 0)
confusionMatrix(factor(xgb_0_1), factor(y_test), mode = "everything")

library(pROC)
plot(roc(y_test, xgb_test_pred))
#caret modeling
library(xgboost)
xgboost_tune_grid <- expand.grid(nrounds = seq(from = 10, to = 100, by = 10),
                                 eta = c(0.025, 0.05, 0.075, 0.1), gamma = 0,
                                 max_depth = c(1, 2, 3, 4), colsample_bytree = 1,
                                 min_child_weight = 1, subsample = 1)
xgboost_tune_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
set.seed(2000)
xgb_tune_class<- train(x = as.matrix(X_train),
                  y = y_train, trControl = xgboost_tune_control,
                  tuneGrid = xgboost_tune_grid, method = "xgbTree", verbose = TRUE,
                  objective = "binary:logistic", eval_metric = "auc", scale_pos_weight = 10)
xgb_tune_class$bestTune
set.seed(2000)
xgb_fit_final_class <- xgboost(data = as.matrix(X_train),
                         label = y_train, objective = "binary:logistic",
                         nrounds = xgb_tune_class$bestTune$nrounds,
                         params = as.list(dplyr::select(xgb_tune_class$bestTune,
                                                        -nrounds)), 
                         verbose = 0,
                         eval_metric = "auc",
                         scale_pos_weight = 10.44)
xgb_test_pred <- predict(xgb_fit_final_class, newdata = test_matrix)
plot(roc(y_test, xgb_test_pred))
roc(y_test, xgb_test_pred)
head(xgb_test_pred)
hist(xgb_test_pred)
stat.labs <-c("Avg Minutes", "Avg Points", "Avg FT%", "Avg Total Rebounds", "Avg 2 point%", 
              "Number of games", "Avg 3 point %", "Avg personal fouls", "Win %")
names(stat.labs) <- c("avg_minutes", "avg_pts", "avg_pctFT", "avg_treb", "avg_pct_fg2",
                      "numGames", "avg_pct_fg3", "avg_pf", "win_percent")
library(vip)
vip(xgb_fit_final_class, geom = "point", color = "#2151a1") + theme_bw() +
  labs(title = "Variable Importance Plot")
#+scale_x_discrete(labels = labeller(stats = stat.labs))

library(pdp)
partial(xgb_fit_final_class, pred.var = "avg_minutes",
        train = as.matrix(X_train),
        plot.engine = "ggplot2", plot = TRUE, prob = TRUE) + geom_line(color = "#eb1933", size = 1) + theme_bw() +
  labs(
    x = "Average Minutes",
    title = "Partial Dependence plot with Average Minutes"
  )

partial(xgb_fit_final_class, pred.var = "avg_pts",
        train = as.matrix(X_train),
        plot.engine = "ggplot2", plot = TRUE, prob = TRUE) + geom_line(color = "#2151a1", size = 1) + theme_bw() +
  labs(
    x = "Average Points",
    title = "Partial Dependence plot with Average Points"
  )
