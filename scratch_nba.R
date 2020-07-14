library(tidycensus)
library(tidyverse)
census_api_key("3a93d59164cea95de65705d13febc7c42f6971cf", install = TRUE)
v17 <- load_variables(2018, "acs1", cache = TRUE)

View(v17)

m90 <- get_acs(geography = "county",
               variables = c("B01001A_001", "B01001B_001", "B01001C_001", "B01001D_001", 
                             "B01001E_001", "B01001F_001", "B01001G_001", "B01001H_001",
                             "B01001I_001", "B01001_002", "B01001_026"),
               year = 2018,
               survey = "acs1")
head(m90)

variable_names <- c("White", "Black", "AIAN", "Asian",
                    "NH", "other", "Multiracial", "WNHL",
                    "Latino", "Male", "Female")
library(plyr)
m90$variable <- mapvalues(m90$variable, from = c("B01001A_001", "B01001B_001", "B01001C_001", "B01001D_001", 
                                                 "B01001E_001", "B01001F_001", "B01001G_001", "B01001H_001",
                                                 "B01001I_001", "B01001_002", "B01001_026"),
                          to = variable_names)
imp_counties <- c("Maricopa County, Arizona", "San Francisco County, California", "Los Angeles County, California", 
                  "Sacramento County, California", "Denver County, Colorado", "Miami-Dade County, Florida",
                  "Orange County, Florida", "Fulton County, Georgia", "Cook County, Illinois", "Marion County, Indiana",
                  "Orleans Parish, Louisiana", "Suffolk County, Massachusetts", "Wayne County, Michigan", "Hennepin County, Minnesota",
                  "Kings County, New York", "New York County, New York", "Mecklenburg County, North Carolina", 
                  "Cuyahoga County, North Carolina", "Oklahoma County, Oklahoma", "Multnomah County, Oregon",
                  "Philadelphia County, Pennsylvania", "Shelby County, Tennessee", "Dallas County, Texas", "Harris County, Texas",
                  "Bexar County, Texas", "Salt Lake County, Utah", "Milwaukee County, Wisconsin", "District of Columbia, District of Columbia")
m90_clean <- m90 %>% 
  pivot_wider(names_from = variable,
              values_from = c(estimate, moe)) %>% 
  filter (NAME %in% imp_counties)



cali_la <- get_decennial(
  geography = "state",
  state = "California",
  city = "Los Angeles",
  year = 2000,
  table = "P012"
)

#NBA STATs
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

