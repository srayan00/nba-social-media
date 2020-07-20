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
            "urlPlayerStats.y", "urlPlayerThumbnail.y", "urlPlayerHeadshot.y", "urlPlayerActionPhoto.y", "urlPlayerPhoto.y")) %>% 
  mutate(possessions = fgaTotals -orbTotals + tovTotals + (0.4 * ftaTotals))

#Offensive rating defensive rating
other_adv_stats <- player_profiles(players = player_stats_clean$namePlayer)

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

#distribution of WS per 48
player_stats_clean %>% 
  ggplot(aes(x = ratioWSPer48)) +
  geom_histogram() +
  theme_bw()
player_stats_clean %>% 
  filter(ratioWSPer48 > -0.2) %>% 
  ggplot(aes(x = ratioWSPer48)) +
  geom_density() +
  theme_bw()

#Does WSper48min capture defensive ability of a player. Correlations with STL%, BLK%, DefReb%, DWS
library(ggcorrplot)
basket_bar <- player_stats_clean %>% 
  dplyr::select(c(pctSTL, pctBLK, pctDRB, ratioDWS, ratioWSPper48))
corr_data <- cor(basket_bar)
colnames(corr_data) <- c("Steal%", "Block%" , "DefRebound%","Def Win Share","Win Share per 48 min")
rownames(corr_data) <- c("Steal%", "Block%" , "DefRebound%","Def Win Share","Win Share per 48 min")
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

#Are counting stats like steals, blocks, defensive rebounds etc captured by win share and def win share

basket_corr <- player_stats_clean %>% 
  dplyr::select(c(stlTotals, blkTotals, drbTotals, ratioDWS, ratioWS))
corr_data_totals <- cor(basket_corr)
colnames(corr_data_totals) <- c("Steal Totals", "Block Totals" , "drbTotals","Def Win Share","Win Share")
rownames(corr_data_totals) <- c("Steal Totals", "Block Totals" , "drbTotals","Def Win Share","Win Share")
round_cor_matrix <- 
  round(corr_data_totals, 2)
ggcorrplot(round_cor_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("#0072B2", "white", "#D55E00")) +
  labs(
    title = "Correlation Plot with different Defensive metrics"
  ) +
  scale_color_continuous()

player_stats_clean %>% 
  pivot_longer(c("stlTotals", "blkTotals", "drbTotals"),
               names_to = "Variable",
               values_to = "Value") %>% 
  ggplot(aes(x = Value, y = ratioDWS)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Variable, scales = "free_x", nrow = 2) +
  geom_smooth() +
  theme_bw()

#Are counting stats like assists, turnovers, field goals, free throws, offensive rebounds captured by offensive win share
basket_corr_off <- player_stats_clean %>% 
  dplyr::select(c(astTotals, tovTotals, fgmTotals, ftmTotals, orbTotals, ratioOWS))
corr_off_data <- cor(basket_corr_off)
round_cor_matrix <- round(corr_off_data, 2)
ggcorrplot(round_cor_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("#0072B2", "white", "#D55E00")) +
  labs(
    title = "Correlation Plot with different Offensive metrics"
  ) +
  scale_color_continuous()

player_stats_clean %>% 
  pivot_longer(c("astTotals", "tovTotals", "fgmTotals", "orbTotals"),
               names_to = "stat",
               values_to = "Value") %>% 
  ggplot(aes(x = Value, y = ratioOWS)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~stat, scales = "free_x", nrow =2) +
  geom_smooth() +
  theme_bw()

#Distribution of WS, DWS, OWS, WS48
player_stats_clean %>% 
  pivot_longer(c("ratioWS", "ratioWSPer48", "ratioDWS", "ratioOWS"),
               names_to = "stat",
               values_to = "value") %>% 
  ggplot(aes(x= value)) +
  geom_histogram() +
  facet_wrap(~stat, scales = "free_x") +
  theme_bw()

#Create OWSper48 and DWSper48 and now, look at distributions. 
player_stats_clean <- player_stats_clean %>% 
  mutate(ratioOWSPer48 = (ratioOWS * 48) / minutes,
         ratioDWSPer48 = (ratioDWS * 48) / minutes)
player_stats_clean %>% 
  pivot_longer(c("ratioWSPer48", "ratioDWSPer48", "ratioOWSPer48"),
               names_to = "stat",
               values_to = "value") %>% 
  ggplot(aes(x= value)) +
  geom_histogram() +
  facet_wrap(~stat, scales = "free_x") +
  theme_bw()
#they are now normalized. May need to scale these variables.