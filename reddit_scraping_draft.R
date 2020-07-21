install.packages("RedditExtractoR")
library(RedditExtractoR)
reddit_data = get_reddit(search_terms = "Grayson Allen",subreddit = "nba",cn_threshold=10)
reddit_data_clean <- reddit_data %>% 
  filter(str_detect(comment, "Grayson") | str_detect(comment, "grayson"))
grayson_allen <- reddit_data

