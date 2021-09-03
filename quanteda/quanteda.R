library(tidyverse)
theme_set(theme_minimal())
library(lubridate)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.corpora)
library(newsmap)
library(seededlda)

# Word clouds
data_corpus_inaugural %>% view
data_corpus_inaugural %>% summary()

inaug_tokens <- data_corpus_inaugural %>% 
  corpus_subset(Year <= 1826) %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_remove(pattern = stopwords("en"))

inaug_dfm <- inaug_tokens %>% 
  dfm() %>% 
  dfm_trim(min_termfreq = 10)

inaug_dfm %>% textplot_wordcloud()

data_corpus_inaugural %>% 
  corpus_subset(President %in% c("Washington", "Jefferson", "Madison")) %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  dfm() %>%
  dfm_group(groups = President) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
  textplot_wordcloud(comparison = TRUE)

# Lexical Dispersion Plot
data_corpus_inaugural %>% 
  corpus_subset(Year > 1949) %>% 
  tokens() %>% 
  kwic(pattern = "american") %>% 
  textplot_xray()

# Frequnecy Plots
