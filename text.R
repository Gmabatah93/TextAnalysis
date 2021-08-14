library(tidyverse)
library(forcats)
library(lubridate)
theme_set(theme_minimal())
library(tidytext)
library(tm)
library(qdap)
library(SnowballC)
library(topicmodels)
library(wordcloud)
library(plotrix)
library(gutenbergr)

#
# Twitter: Data ----

# Data
twitter_data <- read_rds("Data/twitter_data.rds")

# EDA
# - Counts:
twitter_data %>% count(complaint_label) 
twitter_data %>% count(usr_verified)
twitter_data %>% summarise(avg = mean(usr_followers_count)) # 4,189
# - Summary
twitter_data %>% 
  ggplot(aes(complaint_label, fill = usr_verified)) +
  geom_bar(position = "fill") +
  labs(title = "% of Users Verified",
       subtitle = "Complaint: (1650/26) | Non-Complaint: (5277/91)") +
  theme(axis.title = element_blank())
# - Summary: Followers x ( Complaint & Verified )
twitter_data %>% 
  ggplot(aes(complaint_label, usr_followers_count, fill = usr_verified)) +
  geom_col(position = "dodge") +
  labs(title = "User Follower Count",
       subtitle = "Compaint ( 111,268/ 1,532 ) | Non-Complaint ( 140,301 / 2,145 )") +
  theme(axis.title = element_blank())
#
# Twitter: QDAP ----
# - Corpus
twitter_Corpus <- twitter_data$tweet_text %>% 
  VectorSource() %>% 
  VCorpus()

clean_Corpus_twitter <- function(corpus) {
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, words = c(stopwords("en"),
                                                 "united","klm","americanair","delta",
                                                 "southwestair","usairways","britishairways",
                                                 "jetblue","ryanair",
                                                 "just","can","get","will"))
  corpus = tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

twitter_Corpus_clean <- clean_Corpus_twitter(twitter_Corpus)

# - Term Document Matrix
twitter_tdm <- TermDocumentMatrix(twitter_Corpus_clean)
twitter_tdm_M <- as.matrix(twitter_tdm)
twitter_tf <- tibble(
  word = rownames(twitter_tdm_M),
  freq = rowSums(twitter_tdm_M)
) %>% arrange(-freq)

twitter_tf %>%
  top_n(15, freq) %>% 
  ggplot(aes(freq, fct_reorder(word, freq))) +
  geom_col()

# - TfIdf
clean_Corpus_twitter_noStops <- function(corpus) {
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, stripWhitespace)
  
  return(corpus)
}
twitter_Corpus_nStops <- clean_Corpus_twitter_noStops(twitter_Corpus)
twitter_tf_idf <- TermDocumentMatrix(twitter_Corpus_nStops,
                                     control = list(weighting = weightTfIdf))
twitter_tf_idf_M <- as.matrix(twitter_tf_idf)
twitter_tf_idf <- tibble(
  word = rownames(twitter_tf_idf_M),
  freq = rowSums(twitter_tf_idf_M)
) %>% arrange(-freq)

#
# Twitter: tidytext ----

# Bag of Words
twitter_tidy <- twitter_data %>% 
  unnest_tokens(word, tweet_text) %>% 
  anti_join(
    bind_rows(stop_words,
              tibble(word = c("http","win","t.co", "flight",
                              "united","klm","americanair","delta","southwestair",
                              "usairways","british_airways","britishairways","jetblue","ryanair",
                              "aircanada","alaskaair","virginamerica",
                              "de","en","la","el","es","se",
                              1:9),
                     lexicon = "CUSTOM"))
    )

# Term Frequecy 
twitter_tf <- twitter_tidy %>% 
  count(word) %>% 
  arrange(-n)

# - Visual: Complaints
twitter_tidy %>% 
  group_by(complaint_label) %>% 
  count(word) %>%
  arrange(-n) %>% 
  top_n(25) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = complaint_label)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ complaint_label, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw()
# - Visual: User-Verified
twitter_tidy %>% 
  group_by(usr_verified) %>% 
  count(word) %>%
  arrange(-n) %>% 
  top_n(25) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = usr_verified)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ usr_verified, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw()

# - Wordcloud
wordcloud(
  words = twitter_tf$word,
  freq = twitter_tf$n,
  max.words = 30
)

# Twitter: Sentiment Analysis ----

# BING
twitter_bing <- twitter_tidy %>% inner_join(get_sentiments("bing")) 
# - Summary:
twitter_bing %>% count(sentiment) %>% 
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col() +
  labs(title = "Negative = 2888 | Positive = 1871")
# - Summary: (Complaint Label)
twitter_bing %>% 
  ggplot(aes(complaint_label, fill = sentiment)) +
  geom_bar(position = "fill") +
  labs(
    title = "Complaint",
    subtitle = "Complaint: ( 1456 / 394 ) | Non-Complaint: ( 1431 / 1477 )",
    x = NULL, y = NULL
  ) 
# - Summary: (User Verified)
twitter_bing %>% 
  ggplot(aes(usr_verified, fill = sentiment)) +
  geom_bar(position = "fill") +
  labs(
    title = "User Verified: Positive Negative",
    subtitle = "Not-Verified: ( 2852 / 1844 ) | Verified: ( 36 / 27 )",
    x = NULL, y = NULL
  ) 
# - Summary: (Complaint Label x User Verified)
twitter_bing %>% 
  ggplot(aes(complaint_label, fill = sentiment)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ usr_verified, scales = "free") +
  labs(
    title = "Complaint x User Verified",
    x = NULL, y = NULL
  ) + theme_bw()

# - Sentiment: Avg User Follower Count
twitter_bing %>% 
  group_by(sentiment) %>% 
  summarise(Avg_Usr_Follower_Count = mean(usr_followers_count)) %>% 
  ggplot(aes(sentiment, Avg_Usr_Follower_Count)) +
  geom_col() +
  labs(title = "Sentiment: Avg User Follower Count",
       x= NULL, y = NULL)
twitter_bing %>% 
  group_by(sentiment, complaint_label, usr_verified) %>% 
  summarise(Avg_Usr_Follower_Count = mean(usr_followers_count)) %>% 
  ggplot(aes(complaint_label, Avg_Usr_Follower_Count, fill = sentiment)) +
  geom_col(position = "dodge") +
  facet_wrap(~ usr_verified, scales = "free") +
  labs(
    title = "Sentiment: Avg Follower Count by (Complaint & Verified)",
    x = NULL, y = NULL
  ) + theme_bw()
  
# - Words: Top Words
twitter_bing %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw()
# - Words: Top Words (Complaint) 
twitter_bing %>% 
  count(complaint_label, sentiment, word) %>% 
  group_by(sentiment, complaint_label) %>% 
  top_n(10, n) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_grid(sentiment ~ complaint_label, 
             scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw()
# - Words: Top Words (User Verified) 
twitter_bing %>% 
  filter(usr_verified == "TRUE") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, 
             scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw()





# NRC
twitter_nrc <- twitter_tidy %>% inner_join(get_sentiments("nrc"))

# - Summary: Counts
twitter_nrc %>% count(sentiment) %>% 
  ggplot(aes(sentiment, n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(
    title = "Counts",
    x = NULL, y = NULL
  )
# - Summary: Counts (Complaint Label)
twitter_nrc %>% 
  ggplot(aes(sentiment, fill = complaint_label)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~ complaint_label, scales = "free") +
  labs(
    title = "Counts by Complaint Label"
  ) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

twitter_nrc %>% 
  count(complaint_label, sentiment) %>% 
  spread(sentiment,n)

# - Summary: Counts (User Verified)
twitter_nrc %>% 
  ggplot(aes(sentiment, fill = usr_verified)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~ usr_verified, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

twitter_nrc %>% 
  count(usr_verified, sentiment) %>% 
  spread(sentiment,n)

# - Words: Top Words
twitter_nrc %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  theme_bw()



# AFINN
twitter_afinn <- twitter_tidy %>% inner_join(get_sentiments("afinn"))
# - Summary: Count
twitter_afinn %>% count(value)
# - Summary: Count (Complaint Label)
twitter_afinn %>% 
  group_by(complaint_label) %>% 
  summarise(agg_value = sum(value)) %>% 
  spread(complaint_label, agg_value) %>% 
  mutate(Overall = Complaint + `Non-Complaint`)
# - Summary: Count (User Verified)
twitter_afinn %>% 
  group_by(usr_verified) %>% 
  summarise(agg_value = sum(value)) %>% 
  spread(usr_verified, agg_value) %>% 
  mutate(Overall = `FALSE` + `TRUE`)
# - Summary: Count (Complaint Label & User Verified)
twitter_afinn %>% 
  group_by(complaint_label, usr_verified) %>% 
  summarise(agg_value = sum(value)) %>% 
  spread(complaint_label, agg_value) %>% 
  mutate(overall = Complaint + `Non-Complaint`)


# Twitter: Topic Modeling ----
# - document term matrix
twitter_dtm <- twitter_tidy %>% 
  count(word, tweet_id) %>% 
  cast_dtm(document = tweet_id,
           term = word,
           value = n)
twitter_dtm_M <- twitter_dtm %>% as.matrix()
# - LDA 2
twitter_LDA_2 <- LDA(
  x = twitter_dtm, k = 2,
  method = "Gibbs",
  control = list(seed = 42)
)
twitter_Beta_2 <- twitter_LDA_2 %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  mutate(term = fct_reorder(term, beta))
twitter_Beta_2 %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col() +
  facet_wrap(~ topic, scales = "free")
# - LDA 3
twitter_LDA_3 <- LDA(
  x = twitter_dtm, k = 3,
  method = "Gibbs",
  control = list(seed = 42)
)
twitter_Beta_3 <- twitter_LDA_3 %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  mutate(term = fct_reorder(term, beta))
twitter_Beta_3 %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  theme_bw()
#
# Roomba: Data ----

# Data
roomba_reviews <- read_csv("Data/Roomba Reviews.csv")

# EDA
# - product: count
roomba_reviews %>% count(Product)
roomba_reviews %>% 
  group_by(Product) %>% 
  summarise(Avg = mean(Stars))

# Roomba: QDAP ----
# Roomba: tidytext ----

# Bag of Words
roomba_tidy <- roomba_reviews %>% 
  mutate(id = row_number()) %>% 
  unnest_tokens(word, Review) %>% 
  anti_join(
    bind_rows(stop_words,
              tibble(word = c("roomba",2),
                     lexicon = "CUSTOM"))
    ) %>% 
  select(id, everything())

# Term Frequency
roomba_tf <- roomba_tidy %>% 
  count(word) %>% 
  arrange(-n)
roomba_tf_Product <- roomba_tidy %>% 
  count(word, Product) %>% 
  arrange(-n)
# - Visual
roomba_tf %>%
  filter(n > 300) %>% 
  ggplot(aes(n, fct_reorder(word, n))) +
  geom_col()
# - Visual: Count by Product
roomba_tf_Product %>% 
  group_by(Product) %>% top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word,n)) %>% 
  ggplot(aes(n, word, fill = Product)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Product, scales = "free") +
  theme_bw()
# - WordCloud
wordcloud(
  words = roomba_tf$word,
  freq = roomba_tf$n,
  max.words = 30
)

# Senitment Analysis
# - loughran
roomba_loughran <- roomba_tidy %>% 
  inner_join(get_sentiments("loughran"))
roomba_loughran %>% count(sentiment)
roomba_loughran %>% count(word, sentiment) %>% 
  arrange(-n)

roomba_loughran %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  theme_bw()
roomba_loughran %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  theme_bw()

# - bing
roomba_bing <- roomba_tidy %>% 
  inner_join(get_sentiments("bing"))
roomba_bing_Stars <- roomba_bing %>% 
  count(Stars, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(overall = positive - negative,
         Stars = as.factor(Stars)) %>% 
  mutate(Stars = fct_reorder(Stars, overall))
roomba_bing_Stars %>% 
  ggplot(aes(overall, Stars, fill = Stars)) +
  geom_col(show.legend = FALSE)  +
  ggtitle("Overall Sentiment by Stars")


# Topic Modeling
# - document term matrix
roomba_dtm <- roomba_tidy %>% 
  cast_dtm(document = id,
           term = word, 
           value = n)
roomba_dtm
# - LDA 2
roomba_LDA_2 <- LDA(
  x = roomba_dtm, k = 2,
  method = "Gibbs",
  control = list(seed = 42)
)
# - LDA 2: word probs
roomba_Beta_2 <- roomba_LDA_2 %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  mutate(term = fct_reorder(term, beta))
roomba_Beta_2 %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_bw()


# - LDA 3
roomba_LDA_3 <- LDA(
  x = roomba_dtm, k = 3,
  method = "Gibbs",
  control = list(seed = 42)
)
# - LDA 3: word probs
roomba_Beta_3 <- roomba_LDA_3 %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  mutate(term = fct_reorder(term, beta))
roomba_Beta_3 %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_bw()
#
# Drinks: Data ----
# Coffee
coffee <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/27a2a8587eff17add54f4ba288e770e235ea3325/coffee.csv")
coffee_tweets <- coffee$text
# Chardonnay
chardonnay <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/13ae5c66c3990397032b6428e50cc41ac6bc1ca7/chardonnay.csv")
chardonnay_tweets <- chardonnay$text

# Drinks: QDAP ----

# Coffee
# - corpus
coffee_Corpus <- coffee_tweets %>% 
  VectorSource() %>% 
  VCorpus()
coffee_Corpus[[227]] %>% content()
# - clean corpus
clean_corpus_Coffee <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), 
                                                  "coffee", "bean", "mug"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
coffee_Corpus_Clean <- clean_corpus_Coffee(coffee_Corpus)
coffee_Corpus_Clean[[227]] %>% content()
# - Term Document Matix
coffee_tdm <- TermDocumentMatrix(coffee_Corpus_Clean)
coffee_tdm_M <- as.matrix(coffee_tdm)
coffee_tdm_M[c("star", "starbucks"), 25:35]

coffee_tdm %>% tidy() %>% 
  count(term) %>% arrange(-n) # tidytext

# - Term Document Matix: TfIdf
coffee_tf_idf <- TermDocumentMatrix(coffee_Corpus, 
                                    control = list(weighting = weightTfIdf))
coffee_tf_idf_M <- as.matrix(coffee_tf_idf)
coffee_tf_idf_M[c("coffee","espresso","latte"), 1:5]

# - Visual: Term Frequecy
coffee_tf <- tibble(
  Word = rownames(coffee_tdm_M),
  Freq = rowSums(coffee_tdm_M)
) %>% arrange(-Freq)

coffee_tf %>%
  top_n(10, Freq) %>% 
  ggplot(aes(Freq, fct_reorder(Word, Freq))) +
  geom_col()

# - Visual: Word Cloud
wordcloud(
  words = coffee_tf$Word,
  freq = coffee_tf$Freq,
  max.words = 50,
  colors = c("grey80", "darkgoldenrod1", "tomato")
)



# Chardonnay
# - corpus
chardonnay_Corpus <- chardonnay_tweets %>% 
  VectorSource() %>% 
  VCorpus()
chardonnay_Corpus[[24]] %>% content()
# - clean corpus
clean_corpus_Chardonnay <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   words = c( stopwords("en"), 
                              "chardonnay", "amp", "wine", "glass"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
chardonnay_corpus_Clean <- clean_corpus_Chardonnay(chardonnay_Corpus)
chardonnay_corpus_Clean[[24]] %>% content()
# - TermDocument Matrix
chardonnay_tdm <- TermDocumentMatrix(chardonnay_corpus_Clean)
chardonnay_tdm_M <- as.matrix(chardonnay_tdm)

# - Visual: Term Frequency
chardonnay_tf <- tibble(
  Word = rownames(chardonnay_tdm_M),
  Freq = rowSums(chardonnay_tdm_M)
) %>% arrange(-Freq)

chardonnay_tf %>%
  top_n(10, Freq) %>% 
  ggplot(aes(Freq, fct_reorder(Word, Freq))) +
  geom_col()

# - Visual: Word Cloud
wordcloud(
  words = chardonnay_tf$Word,
  freq = chardonnay_tf$Freq,
  max.words = 50,
  colors = c("grey80", "darkgoldenrod1", "tomato")
)





# BOTH
# - Data
all_coffee <- paste(coffee$text, collapse = " ")
all_chardonnay <- paste(chardonnay$text, collapse = " ")
all_drinks <- c(all_coffee, all_chardonnay)
# - Corpus
drinks_Corpus <- all_drinks %>% 
  VectorSource() %>% 
  VCorpus()

clean_corpus_Drinks <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), 
                                                  "coffee", "bean", "mug", "chardonnay", "amp", "wine", "glass"))
  return(corpus)
}
drinks_Corpus_Clean <- clean_corpus_Drinks(drinks_Corpus)

# - TermDocumentMatrix
drinks_tdm <- TermDocumentMatrix(drinks_Corpus_Clean)
colnames(drinks_tdm) <- c("Coffee", "Chardonnay")
drinks_tdm_M <- as.matrix(drinks_tdm)
# - VISUAL: Commonality CLoud
commonality.cloud(term.matrix = drinks_tdm_M,
                  max.words = 100,
                  colors = "steelblue1")
# - VISUAL: Comparision CLoud
comparison.cloud(term.matrix = drinks_tdm_M,
                 max.words = 100,
                 colors = c("orange", "blue"))
# - VISUAL: Pyramid Plot
drinks_tf_top25 <- drinks_tdm_M %>% 
  as_data_frame(rownames = "word") %>% 
  mutate(difference = Chardonnay - Coffee) %>% 
  top_n(n = 25, wt = difference) %>% 
  arrange(-difference)

plotrix::pyramid.plot(
  drinks_tf_top25$Chardonnay, 
  drinks_tf_top25$Coffee, 
  labels = drinks_tf_top25$word, 
  top.labels = c("Chardonnay", "Words", "Coffee"), 
  main = "Words in Common", 
  unit = NULL,
  gap = 8,
)

# Drinks: tidytext ----
# Tech Giants: Data ----
amazon <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/92c0a61dc0ad77799c8cd46bd6e56d9429eb5ea4/500_amzn.csv")
amazon_pros <- amazon$pros
amazon_cons <- amazon$cons

google <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/c050b2c388dfe7e9a0478aa3f67dd0ba3c529d3e/500_goog.csv")
google_pros <- google$pros
google_cons <- google$cons

# Tech Giants: QDAP ----
# - clean text Function
qdap_clean <- function(x){
  x <- replace_abbreviation(x)
  x <- replace_contraction(x)
  x <- replace_number(x)
  x <- replace_ordinal(x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  return(x)
}
# - clean corpus Function
tm_clean <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "Google", "Amazon", "company"))
  return(corpus)
}

# AMAZON
# - clean text
amazon_pros_Clean <- qdap_clean(amazon_pros)
amazon_cons_Clean <- qdap_clean(amazon_cons)
# - Corpus
amazon_pros_Corpus <- amazon_pros_Clean %>% 
  VectorSource() %>% 
  VCorpus()
amazon_cons_Corpus <- amazon_cons_Clean %>% 
  VectorSource() %>% 
  VCorpus()
amazon_pros_Corpus_Clean <- tm_clean(amazon_pros_Corpus)
amazon_cons_Corpus_Clean <- tm_clean(amazon_cons_Corpus)
# - token
amazon_pros_tdm <- TermDocumentMatrix(amazon_pros_Corpus_Clean,
                                      control = list(tokenize = tokenizer))

# GOOGLE
# - clean text
google_pros_Clean <- qdap_clean(google_pros)
google_cons_Clean <- qdap_clean(google_cons)
# - make corpus
google_pros_Corpus <- google_pros_Clean %>% 
  VectorSource() %>% 
  VCorpus()
google_cons_Corpus <- google_cons_Clean %>% 
  VectorSource() %>% 
  VCorpus()
# - clean corpus
google_pros_Corpus_Clean <- tm_clean(google_pros_Corpus)
google_cons_Corpus_Clean <- tm_clean(google_cons_Corpus)


# (Topic Modeling) Sherlock Holmes ----
# EXAMPLE: Hotel Reviews ----

# Data
reviews <- read_csv("Data/deceptive-opinion.csv")

# Tokenization
reviews_Corpus <- VectorSource(reviews$text) %>% Corpus()
reviews_DTM <- reviews_Corpus %>% DocumentTermMatrix()
reviews_DTM_tidy <- tidy(reviews_DTM)
# - clean
custom_stop_words <- tibble(word = c("hotel", "room"))
reviews_DTM_tidy_clean <- reviews_DTM_tidy %>%
  # stop words
  anti_join(stop_words, by = c("term" = "word")) %>% 
  anti_join(custom_stop_words, by = c("term" = "word")) %>% 
  # stemming
  mutate(stem = wordStem(term))

cleaned_documents <- reviews_DTM_tidy_clean %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()


reviews_Corpus_clean <- VectorSource(cleaned_documents$terms) %>% Corpus()
reviews_DTM_clean <- reviews_Corpus_clean %>% DocumentTermMatrix()

# LDA
mod_LDA_2 <- LDA(x = reviews_DTM_clean, k = 2, 
                 control = list(seed = 1234))
# topic-words
lda_topics_2 <- mod_LDA_2 %>% tidy(matrix = "beta")
# - top 10 words
lda_2_words10 <- lda_topics_2 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)
lda_2_words10 %>% 
  mutate(term = reorder(term, beta),
         topic = as.factor(topic)) %>% 
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  labs(x = NULL, y = "beta") +
  coord_flip()


# TF-IDF (Deveptive ?)
# - bow
reviews_Words <- reviews %>% 
  unnest_tokens(word, text) %>% 
  count(deceptive, word) %>% 
  ungroup()
reviews_Words_total <- reviews_Words %>% 
  group_by(deceptive) %>% 
  summarise(total = sum(n))
reviews_Words <- left_join(reviews_Words, reviews_Words_total)

# - tfidf 
reviews_TFIDF <- reviews_Words %>% 
  bind_tf_idf(term = word, 
              document = deceptive,
              n = n) %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))))

reviews_TFIDF %>% 
  group_by(deceptive) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = as.factor(deceptive))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~deceptive, scales = "free") +
  labs(x = NULL, y = "tf_idf") +
  coord_flip()



# TF-IDF (Polarity)
# - bow
reviews_Words <- reviews %>% 
  unnest_tokens(word, text) %>% 
  count(polarity, word) %>% 
  ungroup()
reviews_Words_total <- reviews_Words %>% 
  group_by(polarity) %>% 
  summarise(total = sum(n))
reviews_Words <- left_join(reviews_Words, reviews_Words_total)

# - tfidf 
reviews_TFIDF <- reviews_Words %>% 
  bind_tf_idf(term = word, 
              document = polarity,
              n = n) %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))))

reviews_TFIDF %>% 
  group_by(polarity) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = as.factor(polarity))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~polarity, scales = "free") +
  labs(x = NULL, y = "tf_idf") +
  coord_flip()



# TF-IDF (Hotels)
# - bow
reviews_Words <- reviews %>% 
  unnest_tokens(word, text) %>% 
  count(hotel, word) %>% 
  ungroup()
reviews_Words_total <- reviews_Words %>% 
  group_by(hotel) %>% 
  summarise(total = sum(n))
reviews_Words <- left_join(reviews_Words, reviews_Words_total)

# - tfidf 
reviews_TFIDF <- reviews_Words %>% 
  bind_tf_idf(term = word, 
              document = hotel,
              n = n) %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))))

reviews_TFIDF %>% 
  group_by(hotel) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = hotel)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~hotel, ncol = 4, scales = "free") +
  labs(x = NULL, y = "tf_idf") +
  coord_flip()

# EXAMPLE: Simpsons ----

# Data 
scripts <- read_csv("Data/simpsons_script_lines.csv")
characters <- read_csv("Data/simpsons_characters.csv")
scripts_characters <- left_join(scripts, characters, by = c("character_id" = "id")) %>% 
  filter(!is.na(name))


# EDA
# - 20 most active characters
top20_Characters <- scripts_characters %>% 
  group_by(name) %>% 
  tally(sort = TRUE) %>% 
  top_n(20)
# - Plot: most active characters
top20_Characters %>% 
  ggplot(aes(reorder(name,n), n)) +
  geom_col() + 
  coord_flip() +
  labs(title = "Top 20 Active Characeters",
       x = NULL, y = NULL)
# - top words used
sc_words <- scripts_characters %>% 
  select(id, name, normalized_text)
# - Plot: top words used
sc_words %>% 
  unnest_tokens(word, normalized_text) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  labs(title = "Most Common Words",
       x = "Word", y = NULL) +
  coord_flip()



# Parts of Speech
# - adjectives
sc_words %>% 
  unnest_tokens(word, normalized_text) %>% 
  filter(!word %in% stop_words$word) %>% 
  left_join(parts_of_speech) %>% 
  filter(pos == "Adjective") %>% 
  count(word, sort = TRUE) %>% 
  ungroup()
# - verb (transitive)
sc_words %>% 
  unnest_tokens(word, normalized_text) %>% 
  filter(!word %in% stop_words$word) %>% 
  left_join(parts_of_speech) %>% 
  filter(pos == "Verb (transitive)") %>% 
  count(word, sort = TRUE) %>% 
  ungroup()
# - verb (intransitive)
sc_words %>% 
  unnest_tokens(word, normalized_text) %>% 
  filter(!word %in% stop_words$word) %>% 
  left_join(parts_of_speech) %>% 
  filter(pos == "Verb (intransitive)") %>% 
  count(word, sort = TRUE) %>% 
  ungroup()
# - lines with baby
sc_words %>% 
  filter(str_detect(normalized_text, "baby")) %>% 
  head(10)



# TF-IDF
# - top characters
top6_characters_names <- head(top20_Characters) %>% pull(name)
# - bag of words
sc_BOW <- sc_words %>% 
  unnest_tokens(word, normalized_text) %>% 
  count(name, word, sort = TRUE)
sc_words_Total <- sc_BOW %>%  
  group_by(name) %>% 
  summarize(total = sum(n))
sc_BOW <- left_join(sc_BOW, sc_words_Total)
# - tf_idf
sc_TFIDF <- sc_BOW %>% 
  filter(name %in% top6_characters_names) %>% 
  bind_tf_idf(word, name, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))))

sc_TFIDF %>% 
  top_n(20) %>% 
  ggplot(aes(word, tf_idf, fill = name)) + 
  geom_col() +
  labs(x = NULL, y = "tf_idf") +
  coord_flip()


# Important Words: Marge (homie)
sc_Marge <- scripts_characters %>% 
  filter(name == "Marge Simpson") %>% 
  filter(str_detect(normalized_text, "homie"))

addressesTo_Marge <- data.frame(Name = character(),
                                Text = character())

for (i  in 1:5) {
  
  nextSentenceAfterHomie = scripts_characters %>% 
    filter( id > sc_Marge[i,]$id - 1 ) %>% 
    filter( id < sc_Marge[i,]$id + 2 ) %>% 
    select(name, raw_text)
  
  addressesTo_Marge = rbind(addressesTo_Marge, nextSentenceAfterHomie)
}

# Important Words: Moe (Midge)
sc_Moe <- scripts_characters %>% 
  filter(name == "Moe Szyslak") %>% 
  filter(str_detect(normalized_text, "midge"))

addressesTo_Moe <- data.frame(Name = character(),
                              Text = character())

for (i  in 1:5) {
  
  nextSentenceAfterMidge = scripts_characters %>% 
    filter( id > sc_Moe[i,]$id - 1 ) %>% 
    filter( id < sc_Moe[i,]$id + 2 ) %>% 
    select(name, raw_text)
  
  addressesTo_Moe = rbind(addressesTo_Moe, nextSentenceAfterMidge)
}




# Bigrams: Relationships among words
library(igraph)
library(ggraph)
# - bigram
sc_bigram <- sc_words %>% 
  unnest_tokens(bigram, normalized_text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) 
sc_bigram %>% count(word1, word2, sort = TRUE)
# - Plot: bigram
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

sc_bigram %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 50) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# - bigram (don't)
sc_word1_dont <- sc_words %>% 
  unnest_tokens(bigram, normalized_text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(word1 == "dont")

sc_word2_dont <- sc_words %>% 
  unnest_tokens(bigram, normalized_text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(word2 == "dont")
# - Plot: bigram (dont)
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
rbind(sc_word1_dont, sc_word2_dont) %>% 
  filter(n > 20) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



# Sentiment Analsysis: Top 20 Characters
visualize_sentiments <- function(SCWords) {
  
  SCWords_sentiments <- SCWords %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(name) %>%
    summarize(score = sum(score * n) / sum(n)) %>%
    arrange(desc(score))
  
  SCWords_sentiments %>%
    mutate(name = reorder(name, score)) %>%
    ggplot(aes(name, score, fill = score > 0)) +
    geom_col(show.legend = TRUE) +
    coord_flip() +
    ylab("Average sentiment score") + theme_bw()
 
}
# - top 20 characters
top20_Characters_names <- head(top20_Characters, 20) %>% pull(name)
# - Bag of Words: top 20 
sc_words_Top20 <- sc_words %>% 
  unnest_tokens(word, normalized_text) %>% 
  filter(name != "NA") %>% 
  filter(name %in% top20_Characters_names) %>% 
  count(name, word, sort = TRUE) %>% 
  ungroup()
# - senitment: AFINN
sc_words_Top20_AFINN <- sc_words_Top20 %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(name) %>% 
  summarise(score = sum( value * n ) / sum(n)) %>% 
  arrange(-score)
# - Plot: AFINN
sc_words_Top20_AFINN %>% 
  mutate(name = reorder(name, score)) %>% 
  ggplot(aes(name, score, fill = score > 0)) +
  geom_col() +
  coord_flip() +
  labs(y = "Avg sentiment score")

# Sentiment Analsysis: Words
sc_contributions <- sc_words %>% 
  unnest_tokens(word, normalized_text) %>% 
  filter(name != "NA") %>% 
  count(name, word, sort = TRUE) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(word) %>% 
  summarise(occurences = n(),
            contribution = sum(value))
# - Plot:
sc_contributions %>%
  top_n(20, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

sc_words %>%
  unnest_tokens(word, normalized_text) %>%
  filter(name != "NA") %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id,word) %>%
  summarize(sentiment = mean(value),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5) 


# Topic Modeling (tidytext)
# - word cloud
sc_word_freq <- sc_words %>% 
  unnest_tokens(input = normalized_text,
                output = word,
                drop = TRUE) %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(-n)

wordcloud(words = sc_word_freq$word,
          freq = sc_word_freq$n,
          min.freq = 1,
          max.words = 10,
          random.order = FALSE,
          random.color = FALSE,
          colors=c("DarkOrange", "Blue"))

# - word frequency
sc_freq <- sc_words %>% 
  unnest_tokens(input = normalized_text,
                output = word,
                drop = TRUE) %>% 
  anti_join(stop_words) %>% 
  count(name,word) %>% 
  arrange(-n)
# - document term matrix
sc_dtm <- sc_freq %>% 
  cast_dtm(document = name,
           term = word, 
           value = n)
# - LDA
mod_LDA <- LDA(x = sc_dtm, k = 2, method = "Gibbs",
               control = list(alpha = 1, delta = 0.1, seed = 10005))

terms(mod_LDA, k = 15)
posterior(mod_LDA)$topics
tidy(mod_LDA, matrix = "beta")
tidy(mod_LDA, matrix = "gamma") %>% spread(topic, gamma)
tidy(mod_LDA, matrix = "gamma") %>% 
  filter(document %in% top20_Characters_names) %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(aes(document, gamma, fill = topic)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))




# Topic Modeling 
# - corpus
sc_corpus <- VectorSource(sc_words$normalized_text) %>% Corpus()
# - text preprocess
sc_corpus <- tm_map(sc_corpus, tolower)
sc_corpus <- tm_map(sc_corpus, removePunctuation)
sc_corpus <- tm_map(sc_corpus, removeWords, stopwords("english"))
sc_corpus <- tm_map(sc_corpus, stemDocument)
# - documentTermMatix
sc_dtm <- sc_corpus %>% DocumentTermMatrix()
sc_dtm <- sc_dtm %>% removeSparseTerms(0.997)

labeled_Terms <- as.data.frame(as.matrix(sc_dtm))
labeled_Terms <- labeled_Terms[ rowSums(abs(labeled_Terms)) != 0, ]

# - LDA
mod_LDA <- LDA(labeled_Terms, k = 2, control = list(seed = 13))
# - LDA: topic-word
lda_topics <- tidy(mod_LDA, matrix = "beta")
lda_top_terms <- lda_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)
lda_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# EXAMPLE: Fake News ----
news <- read_csv("Data/fake.csv")

set.seed(1234)
idx <- sample(x = 1:nrow(news),
              size = 1600,
              replace = TRUE)
news_sample <- news[idx,]

# Topic Modeling: Unsupervised

# EXAMPLE Airbnb: EDA ----

# Data
airbnb_reviews <- read_rds("Data/bos_reviews.rds")
airbnb_polarity <- read_rds("Data/bos_pol.rds")

# EDA: polarity
# - per id
airbnb_polarity$all$polarity %>% summary()
airbnb_polarity$all %>% 
  ggplot(aes(polarity, ..density..)) +
  geom_histogram(binwidth = 0.25, fill = "#bada55", colour = "grey60") +
  geom_density(size = 0.75)

# EXAMPLE Airbnb: Toeknization (QDAP) ----

# Tokenization: QDAP
# - positve polarity comments
airbnb_positive <- airbnb_reviews %>% 
  mutate(polarity = airbnb_polarity$all$polarity) %>% 
  filter(polarity > 0) %>% 
  pull(comments) %>% 
  paste(collapse = " ")
# - negative polarity comments
airbnb_negative <- airbnb_reviews %>% 
  mutate(polarity = airbnb_polarity$all$polarity) %>% 
  filter(polarity < 0) %>% 
  pull(comments) %>% 
  paste(collapse = " ")
# - corpus
airbnb_Corpus <- 
  c(airbnb_positive, airbnb_negative) %>% 
  VectorSource() %>% 
  VCorpus()
airbnb_Corpus[[1]] %>% content() # Positive
airbnb_Corpus[[2]] %>% content() # Negative
# - clean corpus
clean_Corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
airbnb_Corpus_clean <- airbnb_Corpus %>% clean_Corpus()
airbnb_Corpus_clean[[1]] %>% content() # Positive
airbnb_Corpus_clean[[2]] %>% content() # Negative

# - term document matrix
airbnb_TFIDF <- 
  TermDocumentMatrix(x = airbnb_Corpus_clean,
                     control = list(weighting = weightTfIdf))

airbnb_TFIDF %>% tidy()

# - document term matrix
airbnb_DTM <- airbnb_Corpus_clean %>% DocumentTermMatrix()
airbnb_DTM %>% tidy()
airbnb_DTM_M <- as.matrix(airbnb_DTM)
airbnb_DTM_M %>% dim()

# WordCloud
# - data prep
airbnb_TFIDF_M <- airbnb_TFIDF %>% as.matrix()
colnames(airbnb_TFIDF_M) <- c("positive", "negative")
# - top positive words
pos_idx <- order(airbnb_TFIDF_M[, 1], decreasing = TRUE)
airbnb_TFIDF_M[pos_idx, ] %>% head(10)
# - top negative words
neg_idx <- order(airbnb_TFIDF_M[, 2], decreasing = TRUE)
airbnb_TFIDF_M[neg_idx, ] %>% head(10)
# - visual
comparison.cloud(
  airbnb_TFIDF_M,
  max.words = 30,
  colors = c("darkgreen","darkred")
)

# Scaled WordlCloud
# - data prep
airbnb_positive_scaled <- airbnb_reviews %>% 
  mutate(scaled_polarity = scale(airbnb_polarity$all$polarity)) %>% 
  filter(scaled_polarity > 0) %>% 
  pull(comments) %>% 
  paste(collapse = " ")
airbnb_negative_scaled <- airbnb_reviews %>% 
  mutate(scaled_polarity = scale(airbnb_polarity$all$polarity)) %>% 
  filter(scaled_polarity < 0) %>% 
  pull(comments) %>% 
  paste(collapse = " ")
# - corpus
airbnb_Corpus_polarity_scaled <- 
  c(airbnb_positive_scaled, airbnb_negative_scaled) %>% 
  VectorSource() %>% 
  VCorpus()
airbnb_Corpus_polarity_scaled_Clean <- airbnb_Corpus_polarity_scaled %>% clean_Corpus()
# - TFIDF
airbnb_TFIDF_scaled <- TermDocumentMatrix(
  airbnb_Corpus_polarity_scaled_Clean,
  control = list(weighting = weightTfIdf))
airbnb_TFIDF_scaled_M <- airbnb_TFIDF_scaled %>% as.matrix()
colnames(airbnb_TFIDF_scaled_M) <- c("positive","negative")
# - visual
comparison.cloud(
  airbnb_TFIDF_scaled_M,
  max.words = 50,
  colors = c("darkgreen","darkred")
)

#
# EXAMPLE Airbnb: Tokenization (tidytext) ----

# Bag of Words 
airbnb_tidy <- airbnb_reviews %>% 
  unnest_tokens(word, comments) %>% 
  group_by(id) %>% 
  anti_join(stop_words) 
# Term Frequency
airbnb_freq <- airbnb_tidy %>% 
  count(word)

# Document Term Matrix
airbnb_freq %>% 
  group_by(id) %>% 
  cast_dtm(document = id, # note: not showing 2 documents 
           term = word,
           value = n) 


# Sentiment Analysis: bing
# - BING
get_sentiments("bing") %>% count(sentiment)
# - Airbnb: bing
airbnb_bing <- airbnb_tidy %>% 
  inner_join(get_sentiments("bing"))
airbnb_bing %>% 
  count(sentiment) %>% 
  ggplot(aes(factor(sentiment), n)) +
  geom_col() + 
  labs(x = NULL, y = NULL) + 
  ggtitle("Bing: Count")
# - calculate polarity for each review
airbnb_bing_polarity_byID <- airbnb_freq %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative)
airbnb_bing_polarity_byID %>% 
  ggplot(aes(id, polarity)) +
  geom_smooth(se = FALSE) +
  ggtitle("Polarity by id (review time ?)")

airbnb_bing_polarity_byWord <- airbnb_freq %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, wt = n) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative)

airbnb_bing_polarity_byWord %>% 
  filter(abs(polarity) >= 3) %>% 
  mutate(pos_neg = ifelse(polarity > 0, "positive", "negative")) %>% 
  ggplot(aes(forcats::fct_reorder(word, polarity), polarity, fill = pos_neg)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Sentiment Word Frequency")

# - assesing Author Effort (number of words used vs polarity)
airbnb_tidy %>% 
  count(id) %>% 
  inner_join(airbnb_bing_polarity) %>% 
  mutate(pol = ifelse(polarity >= 0, "positive","negative")) %>% 
  ggplot(aes(polarity, n, color = pol)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Relationship between word effort & polarity")

# Sentiment Analysis: afinn
# - Afinn 
get_sentiments("afinn") %>% count(value)
# - Airbnb: afinn
airbnb_afinn <- airbnb_tidy %>% inner_join(get_sentiments("afinn"))
airbnb_afinn %>%
  ungroup() %>% 
  count(value) %>% 
  ggplot(aes(factor(value), n)) +
  geom_col() +
  labs(x = NULL, y = NULL) + 
  ggtitle("Afinn: Count")
airbnb_afinn %>% 
  count(value, id) %>% 
  summarise(total_value = sum(value * n)) %>%
  ggplot(aes(id, total_value)) +
  geom_smooth(se = FALSE) +
  ggtitle("Afinn: Total Value by id")

airbnb_freq %>% 
  inner_join(get_sentiments("afinn")) %>% 
  ggplot(aes(value)) +
  geom_density()

# Sentiment Analysis: nrc
# - NRC
get_sentiments("nrc") %>% count(sentiment)
# - Airbnb: nrc
airbnb_nrc <- airbnb_tidy %>% inner_join(get_sentiments("nrc"))
airbnb_nrc %>% 
  ungroup() %>% 
  count(sentiment) %>% 
  ggplot(aes(sentiment, n)) +
  geom_col() +
  labs(x = NULL, y = NULL) + 
  ggtitle("NRC: Count")

airbnb_freq %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(!grepl("positive|negative", sentiment)) %>% 
  count(sentiment, word) %>% 
  spread(sentiment, n, fill = 0)
comparison.cloud()

airbnb_freq %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(!grepl("positive|negative", sentiment)) %>% 
  ungroup() %>% 
  count(sentiment) 
  

# EXAMPLE: Music lyrics polairty ----