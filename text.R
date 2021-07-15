library(tidyverse)
theme_set(theme_minimal())
library(tidytext)
library(tm)
library(qdap)
library(wordcloud)

#
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

tidy_twitter <- tidy_twitter %>% 
  anti_join(stop_words_2)

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
# Twitter: Topic Model ----
library(topicmodels)

# - dtm
dtm_twitter <- tidy_twitter %>% 
  count(word, tweet_id) %>% 
  cast_dtm(tweet_id, word, n)
# - LDA
lda_out <- LDA(
  dtm_twitter,
  k = 2,
  method = "Gibbs",
  control = list(seed = 42)
)

lda_out %>% glimpse()
lda_topics <- lda_out %>% tidy(matrix = "beta")
lda_topics %>% arrange(desc(beta))
# Roomba ----

# Drinks: Data ----
# - Coffee
coffee <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/27a2a8587eff17add54f4ba288e770e235ea3325/coffee.csv")
coffee_tweets <- coffee$text
# - Chardonnay
chardonnay <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/13ae5c66c3990397032b6428e50cc41ac6bc1ca7/chardonnay.csv")
chardonnay_tweets <- chardonnay$text

# Drinks: Tokenization ----

# Coffee
# - source
coffee_source <- VectorSource(coffee_tweets)
# - corpus
coffee_corpus <- VCorpus(coffee_source)
coffee_corpus[[15]][1]
# - stop words
stopwords("en")
new_stops <- c("coffee", "bean", stopwords("en"))
# - clean corpus Function
clean_corpus_Coffee <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "coffee", "mug"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
# - Apply
coffee_corpus_clean <- clean_corpus(coffee_corpus)
coffee_corpus[[227]] %>% content()       # Before
coffee_corpus_clean[[227]] %>% content() # After
# - Document Term Matrix
coffee_dtm <- DocumentTermMatrix(coffee_corpus_clean)
coffee_dtm_matrix <- as.matrix(coffee_dtm)
coffee_dtm_matrix %>% dim
coffee_dtm_matrix[25:35, c("star", "starbucks")]
# - Term Document Matrix
coffee_tdm <- TermDocumentMatrix(coffee_corpus_clean)
coffee_tdm_matrix <- as.matrix(coffee_tdm)
coffee_tdm_matrix[c("star", "starbucks"), 25:35]



# Chardonnay
# - source
chard_source <- VectorSource(chardonnay_tweets)
# - corpus
chard_corpus <- VCorpus(chard_source)
chard_corpus[[24]] %>% content()
# - clean corpus
stop_chard <- c(stopwords(kind = "en"), "chardonnay")

clean_corpus_Chardonnay <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = stop_chard)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

chard_corpus_Clean <- clean_corpus_Chardonnay(chard_corpus)
chard_corpus_Clean[[24]] %>% content()
# - TermDocument Matrix
chard_tdm <- TermDocumentMatrix(chard_corpus_Clean)
chard_tdm_Matrix <- as.matrix(chard_tdm)

#
# Drinks: Visuals ----

# Coffee
# - Bag of Words
coffee_tdm_frequency <- freq_terms(
  text.var = coffee$text,
  top = 100,
  at.least = 3,
  stopwords = c(stopwords("english"),"coffee","mug")
)

# - Barplot
coffee_tdm_frequency %>% plot()

# - WordCloud
wordcloud(words = coffee_tdm_frequency$WORD,
          freq = coffee_tdm_frequency$FREQ,
          max.words = 50, 
          colors = "red")


# Chardonnay
# - Bag of Word
chard_tdm_frequency <- freq_terms(
  text.var = chardonnay$text,
  top = 100,
  at.least = 3,
  stopwords = c(stopwords("english"),"chardonnay")
)
# - WordCloud
wordcloud(words = chard_tdm_frequency$WORD,
          freq = chard_tdm_frequency$FREQ,
          max.words = 100,
          colors = c("grey80", "darkgoldenrod1", "tomato"))

# BOTH
all_coffee <- paste(coffee$text, collapse = " ")
all_chardonnay <- paste(chardonnay$text, collapse = " ")
all_tweets <- c(all_coffee, all_chardonnay)
# - source
all_drinks <- VectorSource(all_tweets) 
all_drinks_corpus <- VCorpus(all_drinks)
# - clean corpus
clean_corpus_ALL <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "amp", "glass", "chardonnay", "coffee"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

drinks_corpus_Clean <- clean_corpus_ALL(all_drinks_corpus)
# - TermDocument Matrix
drinks_tdm <- TermDocumentMatrix(drinks_corpus_Clean)
drinks_tdm_Matrix <- as.matrix(drinks_tdm)
# - VISUAL: Commanlity Cloud
commonality.cloud(term.matrix = drinks_tdm_Matrix,
                  max.words = 100,
                  colors = "steelblue1")
# - VISUAL: Comparison Cloud
comparison.cloud(term.matrix = drinks_tdm_Matrix, 
                 colors = c("orange", "blue"), 
                 max.words = 50)
