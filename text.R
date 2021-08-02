library(tidyverse)
theme_set(theme_minimal())
library(tidytext)
library(tm)
library(qdap)
library(SnowballC)
library(quanteda)
library(topicmodels)
library(wordcloud)
library(plotrix)
library(gutenbergr)

#
# (Tidytext) Twitter: Data ----
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
# (Tidytext) Twitter: Tokenization ----
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
# (Tidytext) Twitter: Sentiment Analysis ----
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
# (Tidytext) Twitter: Topic Model ----
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
# (Tidytext) Roomba ----

# (QDAP) Drinks: Data ----
# - Coffee
coffee <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/27a2a8587eff17add54f4ba288e770e235ea3325/coffee.csv")
coffee_tweets <- coffee$text
# - Chardonnay
chardonnay <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/13ae5c66c3990397032b6428e50cc41ac6bc1ca7/chardonnay.csv")
chardonnay_tweets <- chardonnay$text

# (QDAP) Drinks: Tokenization ----

# Coffee
# - source
coffee_source <- VectorSource(coffee_tweets)
# - corpus
coffee_corpus <- VCorpus(coffee_source)
coffee_corpus[[227]] %>% content()
# - clean corpus
stops_Coffee <- c("coffee", "bean", "mug", stopwords("en"))

clean_corpus_Coffee <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = stops_Coffee)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

coffee_corpus_clean <- clean_corpus_Coffee(coffee_corpus)
coffee_corpus_clean[[227]] %>% content()
# - Document Term Matrix
coffee_dtm <- DocumentTermMatrix(coffee_corpus_clean)
coffee_dtm_matrix <- as.matrix(coffee_dtm)
coffee_dtm_matrix[25:35, c("star", "starbucks")]
# -- 2-gram
coffee_tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

coffee_dtm_bigram <- DocumentTermMatrix(
  coffee_corpus_clean, 
  control = list(tokenize = coffee_tokenizer)
)
coffee_dtm_bigram_Matrix <- as.matrix(coffee_dtm_bigram)
coffee_BOW_bigram <- coffee_dtm_bigram_Matrix %>% 
  colSums() %>% as_data_frame(rownames = "word") %>% 
  arrange(desc(value))

# - Term Document Matrix
coffee_tdm <- TermDocumentMatrix(coffee_corpus_clean)
coffee_tdm_matrix <- as.matrix(coffee_tdm)
coffee_tdm_matrix[c("star", "starbucks"), 25:35]
coffee_tdm_matrix[c("coffee", "espresso", "latte"), 161:166]

# - TFIDF
stops_Coffee <- c("coffee", "bean", "mug", stopwords("en"))

clean_corpus_Coffee_tfidf <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = stopwords)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

coffee_corpus_clean_tfidf <- clean_corpus_Coffee_tfidf(coffee_corpus)
coffee_corpus_clean[[227]] %>% content()


# - Bag of Words
coffee_Matrix <- coffee_tdm_matrix %>% rowSums() %>% as.data.frame()
# -- manual
coffee_BOW_Manual <- tibble(
  Word = rownames(coffee_Matrix),
  Freq = coffee_Matrix[,1]
) %>% arrange(desc(Freq))
# -- qdap
coffee_BOW_QDAP <- freq_terms(text.var = coffee$text,
                              top = 30,
                              at.least = 3,
                              stopwords = stops_Coffee)
coffee_BOW_QDAP %>% plot()
# - VISUAL: Frequency Plot
coffee_BOW_Manual %>% 
  filter(Freq > 30) %>% 
  mutate(Word = fct_reorder(Word, Freq)) %>% 
  ggplot(aes(Freq, Word)) +
  geom_col() +
  ggtitle(label = "Coffee: Bag of Words")
# - VISUAL: WordCloud
wordcloud(words = coffee_BOW_bigram$word,
          freq = coffee_BOW_bigram$value,
          max.words = 15, 
          colors = c("grey80", "darkgoldenrod1", "tomato"))




# Chardonnay
# - source
chardonnay_source <- VectorSource(chardonnay_tweets)
# - corpus
chardonnay_corpus <- VCorpus(chardonnay_source)
chardonnay_corpus[[24]] %>% content()
# - clean corpus
stop_chardonnay <- c("chardonnay", "amp", "wine", "glass", stopwords("en"))

clean_corpus_Chardonnay <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = stop_chardonnay)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

chardonnay_corpus_Clean <- clean_corpus_Chardonnay(chardonnay_corpus)
chardonnay_corpus_Clean[[24]] %>% content()
# - TermDocument Matrix
chardonnay_tdm <- TermDocumentMatrix(chardonnay_corpus_Clean)
chardonnay_tdm_Matrix <- as.matrix(chardonnay_tdm)
# -- 2-gram
bigram_tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

chardonnay_dtm_bigram <- DocumentTermMatrix(
  chardonnay_corpus_Clean, 
  control = list(tokenize = bigram_tokenizer)
)
chardonnay_dtm_bigram_Matrix <- as.matrix(chardonnay_dtm_bigram)
chardonnay_BOW_bigram <- chardonnay_dtm_bigram_Matrix %>% 
  colSums() %>% as_data_frame(rownames = "word") %>% 
  arrange(desc(value))

# - Bag of Words
chardonnay_Matrix <- rowSums(chardonnay_tdm_Matrix) %>% as.data.frame()
# -- manual
chardonnay_BOW_Manual <- tibble(
  Word = rownames(chardonnay_Matrix),
  Freq = chardonnay_Matrix[,1]
) %>% arrange(desc(Freq))
# - VISUAL: Frequency Plot
chardonnay_BOW_Manual %>% 
  filter(Freq > 20) %>% 
  mutate(Word = fct_reorder(Word, Freq)) %>% 
  ggplot(aes(Freq, Word)) +
  geom_col() +
  ggtitle(label = "Chardonnay: Bag of Words")
# - VISUAL: WordCloud
wordcloud(words = chardonnay_BOW_Manual$Word,
          freq = chardonnay_BOW_Manual$Freq,
          max.words = 100, 
          colors = c("grey80", "darkgoldenrod1", "tomato"))
# -- bigram
wordcloud(words = chardonnay_BOW_bigram$word,
          freq = chardonnay_BOW_bigram$value,
          max.words = 15, 
          colors = c("grey80", "darkgoldenrod1", "tomato"))


# BOTH
all_coffee <- paste(coffee$text, collapse = " ")
all_chardonnay <- paste(chardonnay$text, collapse = " ")
all_drinks <- c(all_coffee, all_chardonnay)
# - Tokenization
drinks_source <- VectorSource(all_drinks)
drinks_corpus <- VCorpus(drinks_source)

stops_Drinks <- c("coffee", "bean", "mug", "chardonnay", "amp", "wine", "glass", stopwords("en"))

clean_corpus_Drinks <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = stops_Drinks)
  return(corpus)
}

drinks_corpus_Clean <- clean_corpus_Drinks(drinks_corpus)
# - TermDocumentMatrix
drinks_tdm <- TermDocumentMatrix(drinks_corpus_Clean)
colnames(drinks_tdm) <- c("Coffee", "Chardonnay")
drinks_tdm_Matrix <- as.matrix(drinks_tdm)
# - VISUAL: Commonality CLoud
commonality.cloud(term.matrix = drinks_tdm_Matrix,
                  max.words = 100,
                  colors = "steelblue1")
# - VISUAL: Comparision CLoud
comparison.cloud(term.matrix = drinks_tdm_Matrix,
                 max.words = 100,
                 colors = c("orange", "blue"))
# - VISUAL: Pyramid Plot
drinks_BOW_top25 <- drinks_tdm_Matrix %>% 
  as_data_frame(rownames = "word") %>% 
  filter(. > 0) %>% 
  mutate(difference = Chardonnay - Coffee) %>% 
  top_n(n = 25, wt = difference) %>% 
  arrange(desc(difference))

pyramid.plot(
  drinks_BOW_top25$Chardonnay, 
  drinks_BOW_top25$Coffee, 
  labels = drinks_BOW_top25$word, 
  top.labels = c("Chardonnay", "Words", "Coffee"), 
  main = "Words in Common", 
  unit = NULL,
  gap = 8,
)

# (QDAP) Tech Giants: Data ----
amazon <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/92c0a61dc0ad77799c8cd46bd6e56d9429eb5ea4/500_amzn.csv")
amazon_pros <- amazon$pros
amazon_cons <- amazon$cons

google <- read_csv("https://assets.datacamp.com/production/repositories/19/datasets/c050b2c388dfe7e9a0478aa3f67dd0ba3c529d3e/500_goog.csv")
google_pros <- google$pros
google_cons <- google$cons

# (QDAP) Tech Giants: Tokenization ----
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
# - tokenizer
tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))


# AMAZON
# - clean text
amazon_pros_clean <- qdap_clean(amazon_pros)
amazon_cons_clean <- qdap_clean(amazon_cons)
# - make corpus
amazon_pros_Source <- VectorSource(amazon_pros_clean)
amazon_cons_Source <- VectorSource(amazon_cons_clean)
amazon_pros_Corpus <- VCorpus(amazon_pros_Source)
amazon_cons_Corpus <- VCorpus(amazon_cons_Source)
# - clean corpus
amazon_pros_Corpus_Clean <- tech_tm_amazon_clean(amazon_pros_Corpus)
amazon_cons_Corpus_Clean <- tech_tm_amazon_clean(amazon_cons_Corpus)
# - token
amazon_pros_tdm <- TermDocumentMatrix(amazon_pros_Corpus_Clean,
                                      control = list(tokenize = tokenizer))

# GOOGLE
# - clean text
google_pros_clean <- qdap_clean(google_pros)
google_cons_clean <- qdap_clean(google_cons)
# - make corpus
google_pros_Source <- VectorSource(google_pros_clean)
google_cons_Source <- VectorSource(google_cons_clean)
google_pros_Corpus <- VCorpus(google_pros_Source)
google_cons_Corpus <- VCorpus(google_cons_Source)
# - clean corpus
google_pros_Corpus_Clean <- tech_tm_amazon_clean(google_pros_Corpus)
google_cons_Corpus_Clean <- tech_tm_amazon_clean(google_cons_Corpus)


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
