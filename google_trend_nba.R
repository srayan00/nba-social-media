install.packages("gtrendsR")
library(gtrendsR)
gtrends()
#Draft
library(nbastatR)
draft <- drafts(draft_years = 2018) 
draft_top_30 <- draft %>% 
  filter (numberRound == 1)
player_stats_clean <- player_stats_bio %>% 
  filter ((yearSeason == 2019) & (namePlayer %in% draft_top_30$namePlayer))

#redo troy brown jr. for draft_top_30

draft_30 <- draft_top_30
count <- 1
for (name in draft_30$namePlayer ) {
  example <- gtrends(name, time = "2018-10-16 2019-04-10")
  example_time <- example$interest_over_time
  sum_of_hits <- sum(example_time$hits)
  draft_30[count, "Google_hits"] <- sum_of_hits
  
  example_image <- gtrends(name ,gprop = "images", time = "2018-10-16 2019-04-10")
  example_time_image <- example_image$interest_over_time
  draft_30[count, "GImage_hits"] <- sum(example_time_image$hits)
  
  example_news <- gtrends(name,gprop = "news", time = "2018-10-16 2019-04-10")
  example_time_news<- example_news$interest_over_time
  draft_30[count, "GNews_hits"] <- sum(example_time_news$hits)
  
  example_yt <- gtrends(name,gprop = "youtube", time = "2018-10-16 2019-04-10")
  example_time_yt<- example_yt$interest_over_time
  draft_30[count, "YT_hits"] <- sum(example_time_yt$hits)
  
  count <- count + 1
  
}

#Some rows need to be changed because of the names 
name <- "Mohamed Bamba"
count<- 6
example <- gtrends(name, time = "2018-10-16 2019-04-10")
example_time <- example$interest_over_time
draft_30[count, "Google_hits"] <- sum(example_time$hits)

example_image <- gtrends(name ,gprop = "images", time = "2018-10-16 2019-04-10")
example_time_image <- example_image$interest_over_time
draft_30[count, "GImage_hits"] <- sum(example_time_image$hits)

example_news <- gtrends(name,gprop = "news", time = "2018-10-16 2019-04-10")
example_time_news<- example_news$interest_over_time
draft_30[count, "GNews_hits"] <- sum(example_time_news$hits)

example_yt <- gtrends(name,gprop = "youtube", time = "2018-10-16 2019-04-10")
example_time_yt<- example_yt$interest_over_time
draft_30[count, "YT_hits"] <- sum(example_time_yt$hits)

name <- "Kevin Knox"
count<- 9
example <- gtrends(name, time = "2018-10-16 2019-04-10")
example_time <- example$interest_over_time
draft_30[count, "Google_hits"] <- sum(example_time$hits)

example_image <- gtrends(name ,gprop = "images", time = "2018-10-16 2019-04-10")
example_time_image <- example_image$interest_over_time
draft_30[count, "GImage_hits"] <- sum(example_time_image$hits)

example_news <- gtrends(name,gprop = "news", time = "2018-10-16 2019-04-10")
example_time_news<- example_news$interest_over_time
draft_30[count, "GNews_hits"] <- sum(example_time_news$hits)

example_yt <- gtrends(name,gprop = "youtube", time = "2018-10-16 2019-04-10")
example_time_yt<- example_yt$interest_over_time
draft_30[count, "YT_hits"] <- sum(example_time_yt$hits)

write.csv(draft_30, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\draft_player_2018_gtrend.csv")


#Creating a time series data set
count <- 1
time_draft_2018 <- data.frame()
for (name in draft_30$namePlayer) {
  sample_web <- gtrends(name, time = "2018-10-16 2019-04-10")
  sample_web_time <- sample_web$interest_over_time
  sample_time <- sample_web_time %>% 
    dplyr::select(-c(geo, time, gprop, category)) %>% 
    rename(web_hits = hits)
  print(count)
  
  sample_image <- gtrends(name ,gprop = "images", time = "2018-10-16 2019-04-10")
  sample_time_image <- sample_image$interest_over_time
  if (is.null(sample_time_image)) {
    sample_time_image <- c(rep(NULL, 177))
  } else {
    sample_time_image <- sample_time_image %>% 
      dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
      rename(image_hits = hits)
  }
  sample_time <- dplyr::bind_cols(sample_time, sample_time_image)
  print(count)
  
  sample_news <- gtrends(name,gprop = "news", time = "2018-10-16 2019-04-10")
  sample_time_news<- sample_news$interest_over_time
  if (is.null(sample_time_news)) {
    sample_time_news <- c(rep(NULL, 177))
  } else {
    sample_time_news <- sample_time_news %>% 
      dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
      rename(news_hits = hits)
  }
  sample_time <- dplyr::bind_cols(sample_time, sample_time_news)
  print(count)
  
  sample_yt <- gtrends(name,gprop = "youtube", time = "2018-10-16 2019-04-10")
  sample_time_yt<- sample_yt$interest_over_time
  if (is.null(sample_time_yt)) {
    sample_time_yt <- c(rep(NULL, 177))
  } else {
    sample_time_yt <- sample_time_yt %>% 
      dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
      rename(yt_hits = hits)
  }
  sample_time <- dplyr::bind_cols(sample_time, sample_time_yt)
  time_draft_2018 <- dplyr::bind_rows(time_draft_2018, sample_time)
  print(count)
  count <- count + 1
}



left_overs <- data.frame()
name <- "Robert Williams III" 
sample_web <- gtrends(name, time = "2018-10-16 2019-04-10")
sample_web_time <- sample_web$interest_over_time
sample_time <- sample_web_time %>% 
  dplyr::select(-c(geo, time, gprop, category)) %>% 
  rename(web_hits = hits)

sample_image <- gtrends(name ,gprop = "images", time = "2018-10-16 2019-04-10")
sample_time_image <- sample_image$interest_over_time
sample_time_image <- sample_time_image %>% 
    dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
    rename(image_hits = hits)

sample_time <- dplyr::bind_cols(sample_time, sample_time_image)

sample_news <- gtrends(name,gprop = "news", time = "2018-10-16 2019-04-10")
sample_time_news<- sample_news$interest_over_time
sample_time_news <- sample_time_news %>% 
    dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
    rename(news_hits = hits)
sample_time <- dplyr::bind_cols(sample_time, sample_time_news)

sample_yt <- gtrends(name,gprop = "youtube", time = "2018-10-16 2019-04-10")
sample_time_yt<- sample_yt$interest_over_time
sample_time_yt <- sample_time_yt %>% 
    dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
    rename(yt_hits = hits)
sample_time <- dplyr::bind_cols(sample_time, sample_time_yt)
left_overs <- dplyr::bind_rows(left_overs, sample_time)

robert_williams_news <- read.csv("C:\\Users\\sahan\\Downloads\\multiTimeline (3).csv")
robert_williams_news <- robert_williams_news[-1,]

roberts_data <- time_draft_2018 %>% 
  filter(keyword == "Robert Williams III")
roberts_data$news_hits <- as.integer(robert_williams_news)
jaren_jack_1 <- read.csv("C:\\Users\\sahan\\Downloads\\multiTimeline (2).csv")
jaren_jack_news <- read.csv("C:\\Users\\sahan\\Downloads\\multiTimeline (1).csv")
jaren_jack_data <- time_draft_2018 %>% 
  filter(keyword == "Jaren Jackson Jr.")
jaren_jack_data$news_hits <- as.integer(jaren_jack_news[-1,])
jaren_jack_data$image_hits <- as.integer(jaren_jack_1[-1,])

time_draft_2018_wo <- time_draft_2018 %>% 
  filter(!(keyword %in% c("Marvin Bagley III", "Mo Bamba", "Kevin Knox II", "Troy Brown Jr.")))
time_draft_2018_wo <- time_draft_2018_wo %>% 
  filter(!(keyword %in% c("Robert Williams III", "Jaren Jackson Jr.")))
left_overs$date <- as.character(left_overs$date)
time_draft_2018 <- bind_rows(time_draft_2018_wo, left_overs, roberts_data, jaren_jack_data)

time_draft_2018_test <- time_draft_2018 %>% 
  select(-X)
time_draft_2018_test$keyword[time_draft_2018_test$keyword == "Troy Brown"] <- "Troy Brown Jr."
time_draft_2018_test$keyword[time_draft_2018_test$keyword == "Mohamed Bamba"] <- "Mo Bamba"
time_draft_2018_test$keyword[time_draft_2018_test$keyword == "Kevin Knox"] <- "Kevin Knox II"
time_draft_2018_test <- time_draft_2018_test %>% 
  left_join(select(draft_30, namePlayer, numberRoundPick), by = "namePlayer")
time_draft_2018_test <- time_draft_2018_test %>% 
  arrange(numberRoundPick)

write.csv(time_draft_2018, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\time_series_draft_2018.csv")
write.csv(time_draft_2018, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\time_series_draft_2018_2.csv")

time_draft_2018 <- read.csv("C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\time_series_draft_2018.csv")
#Figure out why there are so many Null values and Change kevin knox and mo bamba

wiki_data_draft <- read.csv("C:\\Users\\sahan\\Downloads\\Wikipedia.csv")
timeseries_draft_2018 <- bind_cols(wiki_data_draft, time_draft_2018_test)
timeseries_draft_2018 <- timeseries_draft_2018 %>% 
  select(-c(date...5, X, PlayerName)) %>% 
  rename(date = date...6 )
timeseries_draft_2018$date <- as.Date(timeseries_draft_2018$date)
timeseries_draft_2018_clean <- timeseries_draft_2018 %>% 
  mutate(week = cut(date, "week")) %>% 
  group_by(namePlayer, week) %>% 
  summarize(wiki_views = mean(views), 
            avg_web_hits = mean(web_hits),
            avg_image_hits = mean(image_hits),
            avg_news_hits = mean(news_hits),
            avg_yt_hits = mean(yt_hits)) %>% 
  left_join(select(draft_30, namePlayer, numberRoundPick), by = "namePlayer") %>% 
  arrange(numberRoundPick)
write.csv(timeseries_draft_2018_clean, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\time_series_draft_2018_clean.csv")
