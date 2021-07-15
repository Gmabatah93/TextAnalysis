library(tidyverse)
library(tidytext)
theme_set(theme_minimal())

# Twitter: Data ----
twitter_data <- read_rds("Data/twitter_data.rds")
# EDA
twitter_data %>% 
  filter(complaint_label == "Complaint")

twitter_data %>% 
  group_by(complaint_label) %>% 
  summarize(
    avg_followers = mean(usr_followers_count),
    min_followers = min(usr_followers_count),
    max_followers = max(usr_followers_count)
  )

twitter_data %>% 
  filter(complaint_label == "Complaint") %>% 
  count(usr_verified)

twitter_data %>% 
  group_by(usr_verified) %>% 
  summarize(
    avg_followers = mean(usr_followers_count),
    n = n())
# Twitter: Tokenization ----
tidy_twitter <- twitter_data %>% 
  unnest_tokens(word, tweet_text) %>% 
  anti_join(stop_words)

tidy_twitter %>% 
  count(word) %>% 
  arrange(desc(n))

tidy_twitter %>% 
  filter(complaint_label == "Complaint") %>% 
  count(word) %>% 
  arrange(desc(n))
# - Word Counts
tidy_twitter %>% 
  filter(complaint_label == "Complaint") %>% 
  count(word) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Complaint Word Counts")

tidy_twitter %>% 
  filter(complaint_label == "Non-Complaint") %>% 
  count(word) %>% 
  filter(n > 150) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Non-Complaint Word Counts")

# - Stop Words
custom_stop_words <- tibble(
  word = c("http","win","t.co"),
  lexicon = "CUSTOM"
)
stop_words_2 <- stop_words %>% 
  bind_rows(custom_stop_words)

tidy_twitter %>% 
  filter(complaint_label == "Non-Complaint") %>% 
  count(word) %>% 
  filter(n > 100) %>% 
  mutate(word2 = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word2, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Non-Complaint Word Counts")

tidy_twitter %>%
  count(word, complaint_label) %>%
  group_by(complaint_label) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word2, y = n, fill = complaint_label)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ complaint_label, scales = "free_y") +
  coord_flip() +
  ggtitle("Twitter Word Counts")

# WordClouds
library(wordcloud)

word_counts <- tidy_twitter %>% 
  anti_join(stop_words_2) %>% 
  count(word)
wordcloud(
  words = word_counts$word, 
  freq = word_counts$n, 
  max.words = 30
)

word_counts <- tidy_twitter %>%
  anti_join(stop_words_2) %>% 
  filter(complaint_label == "Complaint") %>% 
  count(word)

wordcloud(
  words = word_counts$word, 
  freq = word_counts$n, 
  max.words = 50,
  colors = "red"
)

#
# Twitter: Sentiment Analysis ----
tidy_twitter <- tidy_twitter %>% 
  anti_join(stop_words_2)

sentiment_twitter <- tidy_twitter %>% 
  inner_join(get_sentiments("nrc"))
sentiment_twitter %>% 
  count(sentiment) %>% 
  arrange(desc(n))

word_counts <- tidy_twitter %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("positive", "fear", "trust")) %>%
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word2 = fct_reorder(word, n))
word_counts %>% 
  ggplot(aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Sentiment Word Counts",
    x = "Words"
  )

tidy_twitter %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(complaint_label, sentiment) %>% 
  spread(sentiment, n)

tidy_twitter %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(complaint_label, usr_verified) %>% 
  summarize(aggregate_value = sum(value)) %>% 
  spread(complaint_label, aggregate_value) %>% 
  mutate(overall_sentiment = Complaint + `Non-Complaint`)

tidy_twitter %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(complaint_label, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(overall_sentiment = positive - negative) %>% 
  ggplot(aes(x = complaint_label, y = overall_sentiment, fill = as.factor(complaint_label))) +
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  labs(
    title = "Overall Sentiment by Complaint Label",
    subtitle = "Airline Twitter Data"
  )
#
# Roomba ----
