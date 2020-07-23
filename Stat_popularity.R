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
            win_percent = mean(winBool)) %>% 
  left_join(select(draft_30, namePlayer, numberRoundPick), by = "namePlayer") %>% 
  arrange(numberRoundPick) %>% 
  left_join(timeseries_draft_2018_clean, by = c("namePlayer", "week")) %>% 
  left_join(select(df_rosters_2019, namePlayer, weightLBS, heightInches, groupPosition), by = "namePlayer") %>% 
  select(-numberRoundPick.x) %>%  rename(numberRoundPick = numberRoundPick.y)

write.csv(draft_game_data_2018_clean, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\draft_2018_clean.csv")
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
