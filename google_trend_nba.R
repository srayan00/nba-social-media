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


count <- 1
time_draft_2018 <- data.frame()
#Creating a time series data set
for (name in draft_30$namePlayer) {
  sample_web <- gtrends(name, time = "2018-10-16 2019-04-10")
  sample_web_time <- sample_web$interest_over_time
  sample_time <- sample_web_time %>% 
    dplyr::select(-c(geo, time, gprop, category)) %>% 
    rename(web_hits = hits)
  print(count)
  
  sample_image <- gtrends(name ,gprop = "images", time = "2018-10-16 2019-04-10")
  sample_time_image <- sample_image$interest_over_time
  sample_time_image <- sample_time_image %>% 
    dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
    rename(image_hits = hits)
  sample_time <- dplyr::bind_cols(sample_time, sample_time_image)
  print(count)
  
  sample_news <- gtrends(name,gprop = "news", time = "2018-10-16 2019-04-10")
  sample_time_news<- sample_news$interest_over_time
  if (sample_time_news == NULL) {
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
  sample_time_yt <- sample_time_yt %>% 
    dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
    rename(yt_hits = hits)
  sample_time <- dplyr::bind_cols(sample_time, sample_time_yt)
  time_draft_2018 <- dplyr::bind_rows(time_draft_2018, sample_time)
  print(count)
  count <- count + 1
}




sample_web <- gtrends(name, time = "2018-10-16 2019-04-10")
sample_web_time <- sample_web$interest_over_time
sample_time <- sample_web_time %>% 
  dplyr::select(-c(geo, time, gprop, category)) %>% 
  rename(web_hits = hits)
print(count)

sample_image <- gtrends(name ,gprop = "images", time = "2018-10-16 2019-04-10")
sample_time_image <- sample_image$interest_over_time
sample_time_image <- sample_time_image %>% 
  dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
  rename(image_hits = hits)
sample_time <- dplyr::bind_cols(sample_time, sample_time_image)
print(count)

sample_news <- gtrends("Marvin Bagley III",gprop = "news", time = "2018-10-16 2019-04-10")
sample_time_news<- sample_news$interest_over_time
sample_time_news <- sample_time_news %>% 
  dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
  rename(news_hits = hits)
sample_time <- dplyr::bind_cols(sample_time, sample_time_news)
print(count)

sample_yt <- gtrends(name,gprop = "youtube", time = "2018-10-16 2019-04-10")
sample_time_yt<- sample_yt$interest_over_time
sample_time_yt <- sample_time_yt %>% 
  dplyr::select(-c(geo, time, gprop, category, keyword, date)) %>% 
  rename(yt_hits = hits)
sample_time <- dplyr::bind_cols(sample_time, sample_time_yt)
time_draft_2018 <- dplyr::bind_rows(time_draft_2018, sample_time)


