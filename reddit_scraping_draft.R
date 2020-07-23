install.packages("RedditExtractoR")
library(RedditExtractoR)
library(tidyverse)
library(stringr)
reddit_data = get_reddit(search_terms = "Grayson Allen",subreddit = "nba",cn_threshold=10, page_threshold = 4)
reddit_data_clean <- reddit_data %>% 
  filter(str_detect(comment, "Grayson") | str_detect(comment, "grayson") | (str_detect(comment, "GRAYSON"))) %>% 
grayson_allen <- reddit_data

grayson <- grayson_allen %>% 
  filter(str_detect(comment, regex("Grayson", ignore_case = TRUE)) | 
           str_detect(comment, regex("Allen", ignore_case = TRUE))) %>% 
  mutate(player_name = "Grayson Allen")

#Chandler Hutchison
chandler_hutchison <- get_reddit(search_terms = "Chandler Hutchison",subreddit = "nba",cn_threshold=10, page_threshold = 4)
chandler <- chandler_hutchison %>% 
  filter(str_detect(comment, regex("Chandler", ignore_case = TRUE)) | 
           str_detect(comment, regex("Hutchison", ignore_case = TRUE))) %>% 
  mutate(player_name = "Chandler Hutchison")
  

#Aaron Holiday
aaron_holiday <- get_reddit(search_terms = "Aaron Holiday", subreddit = "nba", cn_threshold = 10, page_threshold = 4)
aaron <- aaron_holiday %>% 
  filter(str_detect(comment, regex("Aaron", ignore_case = TRUE)) | 
           str_detect(comment, regex("Holiday", ignore_case = TRUE))) %>% 
  mutate(player_name = "Aaron Holiday")

#Anfernee Simons
anfernee_simons <- get_reddit(search_terms = "Anfernee Simons", subreddit = "nba", cn_threshold = 10, page_threshold = 4)
anfernee <- anfernee_simons %>% 
  filter(str_detect(comment, regex("anfernee", ignore_case = TRUE)) | 
           str_detect(comment, regex("simons", ignore_case = TRUE))) %>% 
  mutate(player_name = "Anfernee Simons")

#Moritz Wagner
moritz_wagner <- get_reddit(search_terms = "Moritz Wagner", subreddit = "nba", cn_threshold = 10, page_threshold = 4)
moritz <- moritz_wagner %>% 
  filter(str_detect(comment, regex("moritz", ignore_case = TRUE)) | 
           str_detect(comment, regex("wagner", ignore_case = TRUE))) %>% 
  mutate(player_name = "Moritz Wagner")

#Landry Shamet
landry_shamet <- get_reddit(search_terms = "Landry Shamet", subreddit = "nba", cn_threshold = 10, page_threshold = 4)
landry <- landry_shamet %>% 
  filter(str_detect(comment, regex("landry", ignore_case = TRUE)) | 
           str_detect(comment, regex("shamet", ignore_case = TRUE))) %>% 
  mutate(player_name = "Landry Shamet")

#Robert Williams III
robert_williams <- get_reddit(search_terms = "Robert Williams III", subreddit = "nba", cn_threshold = 10, page_threshold = 4)
robert <- robert_williams %>% 
  filter(str_detect(comment, regex("robert", ignore_case = TRUE)) | 
           str_detect(comment, regex("williams", ignore_case = TRUE))) %>% 
  mutate(player_name = "Robert Williams III")

#Jacob Evans
jacob_evans <- get_reddit(search_terms = "Jacob Evans", subreddit = "nba", cn_threshold = 10, page_threshold = 4)
jacob <- jacob_evans %>% 
  filter(str_detect(comment, regex("jacob", ignore_case = TRUE)) | 
           str_detect(comment, regex("evans", ignore_case = TRUE))) %>% 
  mutate(player_name = "Jacob Evans")

#Dzanan Musa
dzanan_musa <- get_reddit(search_terms = "Dzanan Musa", subreddit = "nba", cn_threshold = 10, page_threshold = 4)
dzanan <- dzanan_musa %>% 
  filter(str_detect(comment, regex("dzanan", ignore_case = TRUE)) | 
           str_detect(comment, regex("musa", ignore_case = TRUE))) %>% 
  mutate(player_name = "Dzanan Musa")

#Omari Spellman
omari_spellman <- get_reddit(search_terms = "Omari Spellman", subreddit = "nba", cn_threshold = 10, page_threshold = 4)
omari <- omari_spellman %>% 
  filter(str_detect(comment, regex("omari", ignore_case = TRUE)) | 
           str_detect(comment, regex("spellman", ignore_case = TRUE))) %>% 
  mutate(player_name = "Omari Spellman")

reddit_data_bottom10 <- bind_rows(grayson, moritz, dzanan, robert, omari, 
                                  jacob, landry, anfernee, aaron, chandler )
require(devtools)
install_github("Displayr/flipTime")
reddit_data_bottom10$comm_date <- as.Date(reddit_data_bottom10$comm_date, format= "%d-%m-%y")
reddit_data_bottom10 <- reddit_data_bottom10 %>%
  filter(comm_date <= "2019-04-30", comm_date >= "2018-10-01") %>% 
  select(c(player_name,post_date, comm_date, num_comments, post_score, 
           comment_score, controversiality, comment, title, post_text))

write_csv(reddit_data_bottom10, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\reddit_bottom_10.csv")
write.xlsx(reddit_data_bottom10, "C:\\Users\\sahan\\OneDrive\\Documents\\Projects\\CMSAC2020\\reddit_bottom_10.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
#Sentiment analysis
install.packages("tidytext")
library(tidytext)

unnested_bot10 <- reddit_data_bottom10 %>%
  unnest_tokens(word, comment)

my_stop_words <- tibble(
  word = c(
    "https",
    "t.co"),
  lexicon = "twitter"
)

all_stop_words <- stop_words %>%
  bind_rows(my_stop_words)

no_numbers <- unnested_bot10 %>%
  filter(is.na(as.numeric(word)))

no_stop_words_bot10<- no_numbers %>%
  anti_join(all_stop_words, by = "word")

nrc <- get_sentiments("nrc")

get_nrc_sentiment("I am happy", language = "english")



nrc_words <- no_stop_words_bot10 %>%
  inner_join(nrc, by="word")

view(nrc_words)

nrc_words %>%
  group_by(player_name) %>%
  add_count(sentiment) %>%
  ggplot(aes(x = sentiment, y = n)) +
  facet_wrap(~ player_name) +
  geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  labs(title = "Sentiment Analysis: First Round Picks 21-30" ,
       y = "Number of Words",
       x = "Sentiment")

install.packages("syuzhet")
library(syuzhet)
senti <- get_sentiment(reddit_data_bottom10$comment, method = "afinn")


