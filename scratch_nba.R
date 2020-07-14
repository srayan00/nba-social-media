library(tidycensus)
library(tidyverse)
census_api_key("3a93d59164cea95de65705d13febc7c42f6971cf", install = TRUE)
v17 <- load_variables(2018, "acs1", cache = TRUE)

View(v17)

m90 <- get_acs(geography = "county",
               variables = c("B01001A_001", "B01001B_001", "B01001C_001", "B01001D_001", 
                             "B01001E_001", "B01001F_001", "B01001G_001", "B01001H_001",
                             "B01001I_001", "B01001_002", "B01001_026", "B17026_001",
                             "B19001_001", "B08015_001", "B01001_001"),
               year = 2018,
               survey = "acs1")
head(m90)

variable_names <- c("White", "Black", "AIAN", "Asian",
                    "NH", "other", "Multiracial", "WNHL",
                    "Latino", "Male", "Female", "IncomeToPoverty",
                    "HouseholdIncome", "Vehicles", "Total")
library(plyr)
m90$variable <- mapvalues(m90$variable, from = c("B01001A_001", "B01001B_001", "B01001C_001", "B01001D_001", 
                                                 "B01001E_001", "B01001F_001", "B01001G_001", "B01001H_001",
                                                 "B01001I_001", "B01001_002", "B01001_026", "B17026_001",
                                                 "B19001_001", "B08015_001", "B01001_001"),
                          to = variable_names)
imp_counties <- c("Maricopa County, Arizona", "San Francisco County, California", "Los Angeles County, California", 
                  "Sacramento County, California", "Denver County, Colorado", "Miami-Dade County, Florida",
                  "Orange County, Florida", "Fulton County, Georgia", "Cook County, Illinois", "Marion County, Indiana",
                  "Orleans Parish, Louisiana", "Suffolk County, Massachusetts", "Wayne County, Michigan", "Hennepin County, Minnesota",
                  "Kings County, New York", "New York County, New York", "Mecklenburg County, North Carolina",  
                  "Cuyahoga County, Ohio", "Oklahoma County, Oklahoma", "Multnomah County, Oregon",
                  "Philadelphia County, Pennsylvania", "Shelby County, Tennessee", "Dallas County, Texas", "Harris County, Texas",
                  "Bexar County, Texas", "Salt Lake County, Utah", "Milwaukee County, Wisconsin", "District of Columbia, District of Columbia")

                   
m90_clean <- m90 %>% 
  pivot_wider(names_from = variable,
              values_from = c(estimate, moe)) %>% 
  filter (NAME %in% imp_counties) 
m90_clean$team_name <- c("Phoenix Suns", "Golden State Warriors", "Los Angeles Lakers and Clippers", "Sacramento Kings",
                         "Denver Nuggets", "Miami Heat", "Orlando Magic", "Atlanta Hawks", "Chicago Bulls", 
                         "Indiana Pacers", "New Orleans Pelicans", "Boston Celtics", "Detroit Pistons",
                         "Minnesota Timberwolves", "Brooklyn Nets", "New York Knicks", "Charlotte Hornets",
                         "Cleveland Cavaliers", "Oklahoma City Thunder", "Portland Trail Blazers", "Philadelphia 76ers",
                         "Memphis Grizzlies", "Dallas Mavericks", "Houston Rockets", "San Antonio Spurs",
                         "Utah Jazz", "Milwaukee Bucks", "Washington Wizards")

m90_clean <- m90_clean %>% 
  mutate(Conference = ifelse (team_name %in% c("Phoenix Suns", "Golden State Warriors", "Los Angeles Lakers and Clippers", "Sacramento Kings",
                                               "Denver Nuggets", "Minnesota Timberwolves", "Oklahoma City Thunder", "Memphis Grizzlies",
                                               "Portland Trail Blazers", "Dallas Mavericks", "Houston Rockets", "San Antonio Spurs",
                                               "Utah Jazz", "New Orleans Pelicans"), "Western", "Eastern"),
         Division = case_when(team_name %in% c("Phoenix Suns", "Golden State Warriors", "Los Angeles Lakers and Clippers", "Sacramento Kings") ~ "Pacific",
                              team_name %in% c("Miami Heat", "Orlando Magic", "Atlanta Hawks", "Charlotte Hornets", "Washington Wizards") ~ "SouthEast",
                              team_name %in% c("Denver Nuggets", "Minnesota Timberwolves", "Oklahoma City Thunder", "Portland Trail Blazers", "Utah Jazz") ~ "NorthWest",
                              team_name %in% c("Chicago Bulls", "Indiana Pacers", "Detroit Pistons", "Cleveland Cavaliers", "Milwaukee Bucks") ~ "Central",
                              team_name %in% c("New Orleans Pelicans", "Memphis Grizzlies", "Dallas Mavericks", "Houston Rockets", "San Antonio Spurs") ~ "SouthWest",
                              team_name %in% c("Brooklyn Nets", "New York Knicks", "Philadelphia 76ers", "Boston Celtics") ~ "Atlantic")) 
m90_clean$team_abv <- c("PHX", "GSW", "LAC/L", "SAC", "DEN", "MIA",
                        "ORL", "ATL", "CHI", "IND", "NOP", "BOS", 
                        "DET", "MIN", "BKN", "NYK", "CHA", "CLE", "OKC",
                        "POR", "PHI", "MEM", "DAL", "HOU", "SAS",
                        "UTA", "MIL", "WAS")                           
library(ggthemes)
m90_clean %>% 
  mutate (ratio = (estimate_White/estimate_Total)) %>% 
  #filter(Conference %in% c("Eastern")) %>% 
  ggplot(aes(x = team_abv, y = estimate_IncomeToPoverty, fill = Division)) +
  geom_col() +
  theme_bw() +
  scale_fill_colorblind()

cali_la <- get_decennial(
  geography = "state",
  state = "California",
  city = "Los Angeles",
  year = 2000,
  table = "P012"
)

#NBA STATs. Playing around
devtools::install_github("abresler/nbastatR")
library("nbastatR")

draft_2016 <- 
  get_nba_draftnet_year_mock_draft(draft_year = 2016)

mocks_09_15 <- 
  get_nba_draftnet_years_mock_draft(draft_years = 2009:2015)
get_bref_coach_award_df("Gregg Popovich")

salaries <- nba_insider_salaries(
  assume_player_opt_out = F,
  assume_team_doesnt_exercise = T,
  return_wide = F,
  return_message = T
)


player_career_stats <- players_careers(players = c("LeBron James", "Kobe Bryant"))
dict <- nba_players() %>% 
  filter(yearSeasonLast == 2019)

#Career and Season Data for ATL Hawks roster 2017- 2018
df_rosters <- seasons_rosters(2018:2019)
roster_atl <- df_rosters %>% 
  filter ((slugTeam == "ATL") & (yearSeason == 2018))
player_career_stats <- players_careers(players = roster_atl$namePlayer)
atl_seasons <- dataPlayerSeasonTotalsRegularSeason
atl_career <- merge(x = roster_atl, y = dataPlayerCareerTotalsRegularSeason, by = "namePlayer", all = TRUE) %>% 
  select(-c("urlTeamSeasonLogo", "urlPlayerStats", "urlPlayerThumbnail", "urlPlayerHeadshot", "urlPlayerActionPhoto", "urlPlayerPhoto"))
