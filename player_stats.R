library(tidyverse)
library("nbastatR")
salaries <- nba_insider_salaries(
  assume_player_opt_out = F,
  assume_team_doesnt_exercise = T,
  return_wide = F,
  return_message = T
)
df_rosters <- seasons_rosters(2019:2020)

#Dont use this !!!!
player_career_stats <- players_careers(players = df_rosters$namePlayer)
nba_seasons <- dataPlayerSeasonTotalsRegularSeason
nba_career <- merge(x = df_rosters, y = dataPlayerCareerTotalsRegularSeason, by = "namePlayer", all = TRUE) %>% 
  select(-c("urlTeamSeasonLogo", "urlPlayerStats", "urlPlayerThumbnail", "urlPlayerHeadshot", "urlPlayerActionPhoto", "urlPlayerPhoto")) %>% 
  mutate(possessions = fgaTotals - orebTotals + (0.4 * ftaTotals))
######


#This data set has more variables
player_stats <- bref_players_stats(seasons = 2019:2020, tables = c("advanced", "totals")) 
player_stats_bio <- inner_join(x = df_rosters, y = player_stats, by= c("namePlayer", "yearSeason"))
player_stats_clean <- player_stats_bio %>% 
  filter(yearSeason == 2020) %>% 
  select(-c("urlPlayerStats.x", "urlPlayerThumbnail.x", "urlPlayerHeadshot.x", "urlPlayerActionPhoto.x", "urlPlayerPhoto.x",
            "urlPlayerStats.y", "urlPlayerThumbnail.y", "urlPlayerHeadshot.y", "urlPlayerActionPhoto.y", "urlPlayerPhoto.y"))

#Correlation Plot
library(ggcorrplot)
basket_bar <- player_stats_bio %>% 
  dplyr::select(c(ratioPER, ratioWS, pctUSG, pct3PRate, pctTrueShooting, pctEFG, ratioBPM, ratioVORP))
corr_data <- cor(basket_bar)
colnames(corr_data) <- c("PEfficiencyRating", "Win Share" , "Usage Rate","3point%","TrueShooting%", "EFieldGoal%", "Box +/-", "Value Over ReplacementP")
rownames(corr_data) <- c("PEfficiencyRating", "Win Share" , "Usage Rate","3point%","TrueShooting%", "EFieldGoal%", "Box +/-", "Value Over ReplacementP")
ggcorrplot(corr_data)
round_cor_matrix <- 
  round(corr_data, 2)
ggcorrplot(round_cor_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("#0072B2", "white", "#D55E00")) +
  labs(
    title = "Correlation Plot with different evalaution metrics"
  ) +
  scale_color_continuous()
