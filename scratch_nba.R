library(tidycensus)
library(tidyverse)
census_api_key("3a93d59164cea95de65705d13febc7c42f6971cf", install = TRUE)
v17 <- load_variables(2018, "acs5", cache = TRUE)

View(v17)

m90 <- get_acs(geography = "state",
               variables = c("B01001C_002", "B01001C_017", "B01001D_002", "B01001D_017", 
                             "B01001B_002", "B01001B_017", "B01001I_002", "B01001I_017",
                             "B01001E_002", "B01001E_017", "B01001F_002", "B01001F_017",
                             "B01001G_002", "B01001G_017", "B01001A_002", "B01001A_017"),
               year = 2018,
               survey = "acs1")
head(m90)

m90_clean <- m90 %>% 
  pivot_wider(names_from = variable,
              values_from = c(estimate, moe))

cali_la <- get_decennial(
  geography = "state",
  state = "California",
  city = "Los Angeles",
  year = 2000,
  table = "P012"
)
