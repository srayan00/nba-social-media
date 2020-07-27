library(tidyverse)
library(rpart)
init_draft_tree <- rpart(formula = avg_game_score ~ .,
                       data = draft_data_combined, method  = "anova")
init_draft_tree

#Decision Tree
library(caret)
caret_mlb_tree <- train(avg_game_score ~ sentiment + wiki_per_100 + avg_yt_hits + avg_web_hits ,
                        data = draft_data_combined, method = "rpart",
                        trControl = trainControl(method = "cv", number = 10),
                        tuneLength = 20)
ggplot(caret_mlb_tree) + theme_bw()
library(rpart.plot)
rpart.plot(caret_mlb_tree$finalModel)
library(vip)
vip(caret_mlb_tree, bar = FALSE) + theme_bw()

#Random Forest
