library(tidyverse)
library(forcats)
library(magrittr)
theme_set(theme_minimal())
library(tidytext)
library(tm)
library(qdap)

#
# Rick & Morty: Data ----
rm_scripts <- read_csv("Data/RickAndMortyScripts.csv",
                       col.names = c("index","season_num","episode_num","episode_name","name","line"))
# Counts
rm_scripts %>% count(season_num)
rm_scripts %>% count(season_num, episode_num)
rm_scripts %>% count(name) %>% arrange(-n) %>% tibble()

# EDA
rm_scripts %>% count(name) %>%
  top_n(15, n) %>% 
  ggplot(aes(n, fct_reorder(name,n))) +
  geom_col() +
  labs(
    title = "Who talks the most",
    x = NULL, y = NULL
  )
# Rick & Morty: QDAP ----

# - Corpus
clean_Corpus_RM <- function(corpus) {
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}

rm_Corpus <- rm_scripts$line %>% 
  VectorSource() %>% 
  VCorpus()

rm_Corpus_clean <- clean_Corpus_RM(rm_Corpus)

# - Tidy
rm_Corpus_clean %>% tidy() %>% 
  unnest_tokens(word, text)

# - Term Document Matrix
stops_rm <- bind_rows(stop_words,
                      tibble(
                        word = "youre",
                        lexicon = "CUSTOM")
            )
rm_tdm <- TermDocumentMatrix(rm_Corpus_clean)
rm_tdm_M <- as.matrix(rm_tdm)
rm_tf <- tibble(
  word = rownames(rm_tdm_M),
  freq = rowSums(rm_tdm_M)) %>% 
  arrange(-freq) %>% 
  anti_join(stops_rm)

# Rick & Morty: tidytext ----

# Bag of Words
stops_rm <- bind_rows(stop_words, tibble(word = "youre",
                                         lexicon = "CUSTOM"))
rm_tidy <- rm_scripts %>% 
  unnest_tokens(word, line) %>% 
  anti_join(stops_rm) %>% tibble()
# - term frequency
rm_tf <- rm_tidy %>% 
  count(word) %>% 
  arrange(-n)

# Bigrams
rm_bigram <- rm_scripts %>% 
  unnest_tokens(bigram, line,
                token = "ngrams",
                n = 2) %>% tibble()

rm_bigram_clean <- rm_bigram %>% 
  separate(bigram, c("word1","word2"), sep = " ") %>% 
  filter(!word1 %in% stops_rm$word) %>% 
  filter(!word2 %in% stops_rm$word)

rm_bigram_united <- rm_bigram_clean %>% 
  unite(bigram, word1, word2, sep = " ")

rm_bigram_united %>% count(bigram) %>% arrange(-n)

# Trigram
rm_trigram <- rm_scripts %>% 
  unnest_tokens(trigram, line,
                token = "ngrams",
                n = 3) %>% tibble()

rm_trigram_clean <- rm_trigram %>% 
  separate(trigram, c("word1","word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stops_rm$word) %>% 
  filter(!word2 %in% stops_rm$word) %>% 
  filter(!word3 %in% stops_rm$word)

rm_trigram_united <- rm_trigram_clean %>% 
  unite(trigram, word1, word2, word3, sep = " ")

rm_trigram_united %>% count(trigram) %>% arrange(-n)


# Rick & Morty: Sentiment Analysis ----
rm_characters <- c("Rick","Morty","Beth","Jerry","Summer","Pickle Rick")
# Polarity
rm_polarity <- rm_scripts %$% 
  polarity(line, name)

rm_polarity %>% counts() %>% view()
rm_polarity %>% plot()


# Bing: 
rm_bing <- rm_tidy %>% inner_join(get_sentiments("bing"))
rm_bing %>% count(sentiment)  

# - Seasons
rm_bing %>% 
  ggplot(aes(sentiment)) +
  geom_bar() +
  facet_wrap(~season_num) +
  theme_bw()
rm_bing %>% 
  count(season_num, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(overall = positive - negative)
# - Seasons: Top Words
rm_bing %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(25, n) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  labs(title = "Top 25 Words",
       x = NULL, y = NULL)
rm_bing %>% 
  count(season_num, word, sentiment) %>% 
  group_by(season_num, sentiment) %>% 
  top_n(10, n) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment ~ season_num, scales = "free") +
  theme_bw() +
  labs(
    title = "Top 25 Words by Season",
    x = NULL, y = NULL
  )
  
# - Characters
rm_bing %>% 
  filter(name %in% rm_characters) %>% 
  ggplot(aes(sentiment, fill = name)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~name) +
  theme_bw()
rm_bing %>% 
  filter(name %in% rm_characters) %>%
  count(name, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(overall = positive - negative)

rm_bing %>% 
  filter(name %in% rm_characters) %>%
  count(season_num, name, sentiment) %>% 
  ggplot(aes(name, n, fill = sentiment)) +
  geom_col(position = "fill", show.legend = FALSE) +
  facet_wrap(~ season_num) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# - Characters: Top Words
rm_bing %>% 
  filter(name %in% rm_characters) %>% 
  count(name, word, sentiment) %>% 
  group_by(name) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Top Words by Character",
       x = NULL, y = NULL)

rm_bing %>% 
  filter(name %in% rm_characters,
         season_num == 1) %>% 
  count(name, word, sentiment) %>% 
  group_by(name) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Season 1: Top Words by Character",
       x = NULL, y = NULL)
rm_bing %>% 
  filter(name %in% rm_characters,
         season_num == 2) %>% 
  count(name, word, sentiment) %>% 
  group_by(name) %>% 
  top_n(3, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Season 2: Top Words by Character",
       x = NULL, y = NULL)
rm_bing %>% 
  filter(name %in% rm_characters,
         season_num == 3) %>% 
  count(name, word, sentiment) %>% 
  group_by(name) %>% 
  top_n(3, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Season 3: Top Words by Character",
       x = NULL, y = NULL)


# NRC
rm_nrc <- rm_tidy %>% inner_join(get_sentiments("nrc"))
rm_nrc %>% count(sentiment)  %>% arrange(-n)

# - Season
rm_nrc %>% 
  ggplot(aes(sentiment, fill = sentiment)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~season_num) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
rm_nrc %>% 
  count(season_num, sentiment) %>% 
  spread(sentiment, n)
# - Seasons: Top Words
rm_nrc %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free", nrow = 2) +
  labs(title = "Top 25 Words",
       x = NULL, y = NULL) +
  theme_bw()

# - Characters
rm_nrc %>% 
  filter(name %in% rm_characters) %>% 
  count(name, word, sentiment) %>% 
  group_by(name) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Top Words by Character",
       x = NULL, y = NULL)

rm_nrc %>% 
  filter(name %in% rm_characters,
         season_num == 1) %>% 
  count(name, word, sentiment) %>% 
  group_by(name) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Season 1: Top Words by Character",
       x = NULL, y = NULL)
rm_bing %>% 
  filter(name %in% rm_characters,
         season_num == 2) %>% 
  count(name, word, sentiment) %>% 
  group_by(name) %>% 
  top_n(3, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Season 2: Top Words by Character",
       x = NULL, y = NULL)
rm_bing %>% 
  filter(name %in% rm_characters,
         season_num == 3) %>% 
  count(name, word, sentiment) %>% 
  group_by(name) %>% 
  top_n(3, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Season 3: Top Words by Character",
       x = NULL, y = NULL)
