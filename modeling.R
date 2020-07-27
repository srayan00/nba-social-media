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
