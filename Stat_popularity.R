draft_game_data_2018_1 <- read.csv("C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\game_player_draft_data.csv")
draft_game_data_2018_clean <- draft_game_data_2018_1 %>% 
  mutate(week = cut(as.Date(dateGame), "week"), winBool = ifelse(outcomeGame == "W", 1, 0)) %>% 
  group_by(namePlayer, week) %>% 
  summarize(avg_game_score = mean(game_score),
            numGames = n(),
            avg_box_plusminus = mean(plusminus),
            avg_minutes = mean(minutes),
            avg_pct_fg3 = mean(pctFG3),
            avg_pct_fg2 = mean(pctFG2),
            win_percent = mean(winBool),
            avg_fgm = mean(fgm),
            avg_fga = mean(fga),
            avg_pctfg = mean(pctFG),
            avg_pctFT = mean(pctFT),
            avg_oreb = mean(oreb),
            avg_tov = mean(tov),
            avg_dreb = mean(dreb),
            avg_stl = mean(stl),
            avg_blk = mean(blk),
            avg_ast = mean(ast),
            avg_treb = mean(treb),
            avg_pf = mean(pf),
            avg_pts = mean(pts),
            avg_countDaysRestPlayer = mean(countDaysRestPlayer)) %>% 
  left_join(select(draft_30, namePlayer, numberRoundPick), by = "namePlayer") %>% 
  arrange(numberRoundPick) %>% 
  mutate(week = as.Date(week)) %>% 
  left_join(timeseries_draft_2018_clean, by = c("namePlayer", "week")) %>% 
  left_join(select(df_rosters_2019, namePlayer, weightLBS, heightInches, groupPosition), by = "namePlayer") %>% 
  select(-numberRoundPick.x) %>%  rename(numberRoundPick = numberRoundPick.y)
write.csv(draft_game_data_2018_clean, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\draft_2018_clean.csv")

reddit_draft_data <- read.csv("C:\\Users\\sahan\\Downloads\\combo_scored_1.csv")
reddit_draft_data <- reddit_draft_data %>% 
  mutate(sentiment_week = sentiment / 7,
         positive_week = positive/ 7,
         negative_week = negative / 7)
reddit_draft_data$week = as.Date(reddit_draft_data$week)
draft_data_combined <- reddit_draft_data %>% 
  rename(namePlayer = player_name) %>% 
  inner_join(draft_game_data_2018_clean, by = c("namePlayer", "week")) %>% 
  arrange(numberRoundPick) %>% 
  mutate(wiki_per_100 = wiki_views / 100)

write.csv(draft_data_combined, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\draft_gamereddit_clean.csv")

#Comment score and controversiality

reddit_comment <- read_xlsx("C:\\Users\\sahan\\Downloads\\final_reddit_datescorrected.xlsx") 

reddit_comment$comm_date <- as.Date(reddit_comment$comm_date)

reddit_comment <- reddit_comment %>%  
  filter((comm_date > as.Date("2018-10-16")) & ((comm_date < as.Date("2019-04-10"))) )

reddit_comment_week <- reddit_comment %>% 
  mutate(week = cut(comm_date, "week")) %>% 
  group_by(player_name, week) %>% 
  summarize(avg_commentscoreperday = sum(comment_score) / 7, 
            avg_controversial = sum(controversiality) / 7)
reddit_draft_comment_data <- left_join(reddit_comment_week, reddit_draft_data, by = c("player_name", "week"))
write.csv(reddit_draft_comment_data, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\all_reddit_data.csv")

#EDA
#Normality check
draft_game_data_2018_clean %>% 
  ggplot(aes(x = avg_box_plusminus)) +
  geom_density()

draft_game_data_2018_clean %>% 
  ggplot(aes(x = avg_minutes, y = avg_web_hits)) +
  geom_point()

draft_game_data_2018_clean %>% 
  filter(win_percent %in% c(0 , 1)) %>% 
  ggplot(aes(x = as.factor(win_percent), y = avg_web_hits)) +
  geom_boxplot()

draft_game_data_2018_clean %>% 
  ggplot(aes(x = avg_minutes, y = avg_web_hits)) +
  geom_point(alpha = 0.2)

#draft data combined
draft_data_combined %>% 
  ggplot(aes(x = avg_game_score)) +
  geom_histogram(fill = "#eb1933", color = "black") +
  theme_bw() +
  labs(
    x = "Average game score",
    title = "Distribution of Average game score"
  ) +
  theme(
    plot.title = element_text(hjust="0.5")
  )

draft_data_combined %>% 
  ggplot(aes(x = scale(sentiment), y = win_percent)) +
  geom_jitter()

draft_data_combined %>% 
  filter(sentiment < 0) %>% 
  ggplot(aes(x = positive)) +
  geom_density()

draft_data_combined %>% 
  group_by(numberRoundPick) %>% 
  summarize(gamescore = mean(avg_game_score), senti = mean(sentiment_week), webhit = mean(avg_yt_hits)) %>% 
  ggplot(aes(x = numberRoundPick, y = senti)) +
  geom_point() +
  xlab("Number Round Pick") + ylab("Average Sentiment score") +
  theme_bw()

#Hierarchial Clustering
draft_scaled_data <- draft_data_combined %>% 
  select(wiki_per_100, avg_web_hits, avg_news_hits, avg_yt_hits, positive_week, negative_week) %>% 
  scale()  %>% 
  as.data.frame()

nba_draft_hclust <- 
  hclust(dist(draft_scaled_data),
    method = "complete")

hc_player_clusters <-
  cutree(nba_draft_hclust,
         k = 2)
draft_data_combined <-draft_data_combined %>%
  mutate(player_hc_clusters = 
           as.factor(hc_player_clusters))
draft_data_combined %>% 
  mutate(player_hc_clusters = 
           as.factor(hc_player_clusters)) %>% 
  ggplot(aes(x = avg_web_hits, y = sentiment_week,
             color = player_hc_clusters)) +
  geom_point() +
  theme_bw() +
  labs(x = " Average web hits",
       y = "Sentiment score",
       color = "Cluster",
       title = "Scatter plot of Average Web hits and Sentiment Score") +
  theme(legend.position = "bottom") +  scale_color_manual(values =c("#eb1933", "#2151a1"))

# What is cluster 3. The players have high average game score and average minutes but popularity metrics doesnt capture this
# Deandre Ayton and Trae Young are only in cluster 3
# In cluster 3, the players during that week weren't very popular but people had a highly positive sentiment towards them.
# Cluster 2 : not popular, game score is low, sentiment score is low
# Cluster 1 : a bit more popular than cluster 2; sentiment is the same as cluster 2; game score is a little better than
# Cluster 4 : Most popular category, average game scores are still high but not as high as cluster 3

draft_data_combined %>%
  mutate(player_hc_clusters = 
           as.factor(hc_player_clusters)) %>% 
  ggplot(aes(x = player_hc_clusters, y = avg_game_score)) +
  geom_violin() + 
  geom_boxplot(width = 0.2, color = "#eb1933", size = 0.8 ) +
  theme_bw() +
  labs(
    x = "Clusters",
    y = "Average Game Score",
    title = "Box Plot of Average game score for the 2 clusters "
  ) +
  theme (
    plot.title = element_text(hjust = "0.5")
  )

draft_data_combined %>%
  mutate(player_hc_clusters = 
           as.factor(hc_player_clusters)) %>% 
  ggplot(aes(x = player_hc_clusters, y = avg_pts)) +
  #geom_violin() + 
  geom_boxplot(width = 0.2, color = "#2151a1", size = 0.8) +
  theme_bw() +
  labs(
    x = "Clusters",
    y = "Average Points",
    title = "Box Plot of Average points for the 2 clusters "
  ) +
  theme(
    axis.title = element_text(hjust = "0.5")
  )

#Gaussian Mixture Models
library(mclust)
draft_mclust <- Mclust(draft_scaled_data)
summary(draft_mclust)
draft_mclust$classification
draft_data_combined %>% 
  mutate(cluster = as.factor(draft_mclust$classification)) %>% 
  ggplot(aes(x = avg_web_hits, y = wiki_per_100,
             color = cluster)) +
  geom_point() +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  labs(x = " Average web hits",
       y = "Wiki views in 100s",
       color = "Cluster") +
  theme(legend.position = "bottom")

#Time series analysis
#Top 5 picks
draft_game_data_2018_clean$week <- as.Date(draft_game_data_2018_clean$week)

draft_game_data_2018_clean %>% 
  group_by(week) %>% 
  

draft_game_data_2018_clean %>% 
  filter(namePlayer == "Deandre Ayton") %>% 
  pivot_longer(c("avg_game_score", "avg_web_hits"),
               names_to = "Name",
               values_to = "value") %>% 
  ggplot (aes(x = week, y = value)) +
  geom_line()+
  facet_wrap(~Name, scales = "free_y") +
  theme_bw()

timeseries_draft_2018_clean$week <- as.Date(timeseries_draft_2018_clean$week)
timeseries_draft_2018_clean %>% 
  filter(numberRoundPick < 5) %>% 
  pivot_longer(c("wiki_views", "avg_web_hits", "avg_yt_hits"),
               names_to = "names",
               values_to = "values") %>% 
  ggplot(aes(x = week, y = values)) +
  geom_line(aes(color = namePlayer), width = 1.2) +
  facet_wrap(~names, scales = "free_y", nrow = 2) +
  theme_classic()

draft_game_data_2018_clean %>% 
  filter(numberRoundPick < 4) %>% 
  pivot_longer(c("wiki_views", "avg_web_hits", "avg_yt_hits", "avg_game_score"),
               names_to = "names",
               values_to = "values") %>% 
  ggplot(aes(x = week, y = values)) +
  geom_line(aes(color = namePlayer), width = 1.2) +
  facet_wrap(~names, scales = "free_y", nrow = 2) +
  theme_classic()

#Clustering for sentiment metrics
reddit_draft_comment_data <- read.csv("C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\all_reddit_data.csv")
draft_scaled_data_comment <- reddit_draft_comment_data %>% 
  select(sentiment_week, avg_commentscoreperday) %>% 
  filter(avg_commentscoreperday < 3000) %>% 
  scale()  %>% 
  as.data.frame() %>% 
  na.omit()

nba_draft_hclust <- 
  hclust(dist(draft_scaled_data_comment),
         method = "complete")

hc_player_clusters <-
  cutree(nba_draft_hclust,
         k = 4)
reddit_draft_comment_data %>%
  filter((!is.na(avg_commentscoreperday)) & (!is.na(sentiment_week))) %>% 
  filter(avg_commentscoreperday < 3000) %>% 
  #na.omit() %>% 
  mutate(player_hc_clusters = 
           as.factor(hc_player_clusters)) %>% 
  ggplot(aes(x = sentiment_week, y = avg_commentscoreperday,
             color = player_hc_clusters)) +
  geom_point() +
  theme_bw() +
  labs(x = "Sentiment Score",
       y = "Average Comment Score",
       color = "Cluster",
       title = "Scatterplot of Sentiment and comment score") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = "0.5")) +
  scale_color_manual(values = c("#eb1933", "#2151a1", "black", "#a9a9a9") )
  
#Cluster 1 :- Sentiment scores are low but comment scores are also quite low. This indicated that the plaer is controversial
#Cluster 2: Higher sentiment scores and relatively higher comment scores. The player is perceived more positive than Cluster 1 players
#Cluster 3: Higherst sentiment scores but comment scores are in between cluster 2 and cluster 1. The player is perceived most positively 

reddit_draft_comment_data %>%
  filter((!is.na(avg_commentscoreperday)) & (!is.na(sentiment_week))) %>% 
  filter(avg_commentscoreperday < 3000) %>% 
  rename(namePlayer = player_name) %>% 
  left_join(dplyr::select(draft_data_combined, c("week", "namePlayer", "avg_game_score")), by = c("namePlayer", "week")) %>% 
  mutate(player_hc_clusters = 
           as.factor(hc_player_clusters)) %>% 
  ggplot(aes(x = player_hc_clusters, y = avg_game_score)) +
  #geom_violin() + 
  geom_boxplot(width = 0.2, color = "#2151a1", size = 0.8) +
  labs(
    x = "Clusters", y = "Average game score",
    title = "Box plot of Average game score for the 4 clusters"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = "0.5")
  )
