library(tidyverse)
library(forcats)
theme_set(theme_minimal())
library(tidytext)
library(tm)

# Rick & Morty: Data ----
rm_scripts <- read.csv("Data/RickAndMortyScripts.csv",
                       col.names = c("index","season_num","episode_num","episode_name","name","line"))
# Counts
rm_scripts %>% count(season_num)
rm_scripts %>% count(season_num, episode_num)
rm_scripts %>% count(name) %>% arrange(-n)

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

# Rick & Morty: Sentiment Analysis
rm_scripts %>% 
  unnest_tokens(word, line)
