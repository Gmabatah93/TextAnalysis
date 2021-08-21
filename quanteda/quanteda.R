library(tidyverse)
theme_set(theme_minimal())
library(quanteda)

# 
data <- data_char_ukimmig2010

# Corpus
my_corpus <- corpus(x = data)
docvars(my_corpus, "Party") <- names(data)
docvars(my_corpus, "Year") <- 2010
meta(my_corpus, "language") <- "english"
meta(my_corpus, "docsource")  <- paste("data_char_ukimmig2010", 1:ndoc(my_corpus), sep = "_")
my_corpus %>% 
  summary(showmeta = TRUE)

data_corpus_inaugural %>% 
  summary() %>% 
  ggplot(aes(Year, Tokens)) +
  geom_line() + geom_point() +
  scale_x_continuous(labels = c(seq(1789, 2021, 14)), breaks = c(seq(1789, 2021, 14)))

# - extract text
texts(data_corpus_inaugural)[2]

# Adding two corpus objects together
my_corpus_1 <- corpus(data_corpus_inaugural[1:5])
my_corpus_2 <- corpus(data_corpus_inaugural[53:58])
my_corpus_3 <- my_corpus_1 + my_corpus_2

my_corpus_3 %>% summary()

# Subsetting Corpus Objects
corpus_subset(data_corpus_inaugural, Year > 1990) %>% summary()
corpus_subset(data_corpus_inaugural, President == "Adams") %>% summary()

# Exploring Corpus Texts
kwic(data_corpus_inaugural, "terror")
kwic(data_corpus_inaugural, "terror", valuetype = "regex")
kwic(data_corpus_inaugural, "communist*")
docvars(data_corpus_inaugural) %>% head()
meta(data_corpus_inaugural)

# Extracting Features from a Corpus
txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!", 
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")

tokens(txt, remove_numbers = TRUE, remove_punct = TRUE)
tokens(txt, remove_numbers = FALSE, remove_punct = TRUE)
tokens(txt, remove_numbers = TRUE, remove_punct = FALSE)
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE)
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE, remove_separators = FALSE)

tokens("Great website: http://textasdata.com?page=123.",
       what = "character")
tokens("Great website: http://textasdata.com?page=123.",
       what = "character", remove_separators = FALSE)
tokens(c("Kurt Vongeut said; only assholes use semi-colons.", 
         "Today is Thursday in Canberra:  It is yesterday in London.", 
         "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"), 
       what = "sentence")

# Contructing a document-feature matrix
my_corpus_1990 <- corpus_subset(data_corpus_inaugural, Year > 1990)
# - basic
my_dfm <- my_corpus_1990 %>% dfm()
# - stops & stemming
my_dfm_2 <- my_corpus_1990 %>% 
  dfm(remove = stopwords("english"),
      stem = TRUE,
      remove_punct = TRUE)

# Viewing the document-feature matrix
my_dfm_2 %>% topfeatures(20)

# Grouping documents by document variable
data_corpus_inaugural_dfm <- data_corpus_inaugural %>% 
  dfm(remove = stopwords("english"),
      stem = TRUE,
      remove_punct = TRUE)

data_corpus_inaugural_dfm_Party <- data_corpus_inaugural_dfm %>% 
  dfm_group(groups = "Party")
