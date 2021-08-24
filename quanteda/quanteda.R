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

# Data Import ----

# Basic Operations ----
path_data <- system.file("extdata/", package = "readtext")
dat_inaug <- read_csv(paste0(path_data, "/csv/inaugCorpus.csv"))
# - corpus
corp_inaug <- corpus(x = dat_inaug, text_field = "texts")
summary(corp_inaug)
# - add doc ids: 1
corp_inaug <- corpus(x = dat_inaug, text_field = "texts",
                     docid_field = "Year")
summary(corp_inaug)
# - add doc ids: 2
doc_id <- paste(dat_inaug$Year,
                dat_inaug$FirstName,
                dat_inaug$President,
                sep = " ")
docnames(corp_inaug) <- doc_id
summary(corp_inaug)
# - extracting document-variables
corp_inaug %>% docvars(field = "FirstName")
corp_inaug$FirstName
# - assinging document-level variables
corp_inaug$Country <- "USA"
summary(corp_inaug)


# Subset Corpus
data_corpus_inaugural %>% ndoc()
data_corpus_inaugural %>% corpus_subset(Year >= 1990)
data_corpus_inaugural %>% corpus_subset(President %in% c("Obama", "Clinton", "Carter"))


# Change units of text
data_char_ukimmig2010 %>% view()
corp_ukimmig <- corpus(data_char_ukimmig2010)
corp_ukimmig %>% ndoc()

corp_ukimmig_sent <- corp_ukimmig %>% corpus_reshape(to = "sentences")
corp_ukimmig_sent %>% ndoc()

# Extracting Tags from Texts
corp_tagged <- corpus(c("##INTRO This is the introduction.
                         ##DOC1 This is the first document.  Second sentence in Doc 1.
                         ##DOC3 Third document starts here.  End of third document.",
                        "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
corp_sect <- corp_tagged %>% corpus_segment(pattern = "##")
cbind(docvars(corp_sect), text = as.character(corp_sect))

corp_speeches <- corpus("Mr. Smith: Text.
                        Mrs. Jones: More text.
                        Mr. Smith: I'm speaking, again.")
corp_speakers <- corpus_segment(corp_speeches, pattern = "\\b[A-Z].+\\s[A-Z][a-z]+:", valuetype = "regex")
cbind(docvars(corp_speakers), text = as.character(corp_speakers))

# Contruct a Tokens Object
corp_ukimmig %>% view()
toks_ukimmig <- tokens(corp_ukimmig)
toks_ukimmig_nopunct <- tokens(corp_ukimmig, remove_punct = TRUE) 

# Keywords in Contexts
toks_ukimmig %>% kwic(pattern = "immig*")
toks_ukimmig %>% kwic(pattern = c("immig*","migra*"))
toks_ukimmig %>% kwic(pattern = c("immig*", "migra*"), window = 7)
toks_ukimmig %>% kwic(pattern = phrase("asylum seeker*")) %>% view()

# Select Tokens
toks_ukimmig %>% tokens_select(pattern = stopwords("en"), selection = "remove")
toks_ukimmig %>% tokens_select(pattern = stopwords("en"))
toks_ukimmig %>% tokens_select(pattern = stopwords("en"), padding = TRUE)
toks_ukimmig %>% tokens_select(pattern = c("immag*","migra*"), padding = TRUE)
toks_ukimmig %>% tokens_select(pattern = c("immig*", "migra*"), padding = TRUE, window = 5)

# Compound Tokens
toks_ukimmig %>% kwic(pattern = phrase(c("asylum seeker*", "british citizen*")))
toks_ukimmig %>% 
  tokens_compound(pattern = phrase(c("asylum seeker*", "british citizen*"))) %>% 
  kwic(pattern = c("asylum_seeker*", "british_citizen*"))

# Look up Dictionary
dict <- dictionary(list(refugee = c("refugee*","asylum"),
                        worker = c("worker","employee")))

toks_ukimmig %>% 
  tokens_lookup(dictionary = dict) %>% 
  dfm()

# Generate ngrams
toks_ukimmig %>% tokens(remove_punct = TRUE)

toks_ukimmig_ngram <- toks_ukimmig %>% 
  tokens_ngrams(n = 2:4)
toks_ukimmig_ngram[[1]] %>% head()
toks_ukimmig_ngram[[1]] %>% tail()

toks_ukimmig_neg_bigram <- toks_ukimmig %>% 
  tokens_compound(pattern = phrase("not *"))
toks_ukimmig_neg_bigram_select <- toks_ukimmig_neg_bigram %>% 
  tokens_select(pattern = phrase("not_*"))
toks_ukimmig_neg_bigram_select[[1]] %>% head(30)

# Document-Feature Matrix
toks_innag <- tokens(data_corpus_inaugural, remove_punct = TRUE)

dfmat_innaug <- toks_innag %>% dfm()
dfmat_innaug %>% docnames()
dfmat_innaug %>% featnames()
dfmat_innaug %>% rowSums()
dfmat_innaug %>% colSums()
dfmat_innaug %>% topfeatures()

dfmat_innaug_prop <- dfmat_innaug %>% dfm_weight(scheme = "prop")
dfmat_innaug_tfidf <- dfmat_innaug %>% dfm_tfidf()

# Select Features
dfmat_innaug %>% dfm_select(pattern = stopwords("en"), selection = "remove")
dfmat_innaug %>% dfm_remove(pattern = stopwords("en"))
dfmat_innaug %>% dfm_keep(min_nchar = 5) %>% 
  topfeatures()
dfmat_innaug %>% dfm_trim(min_termfreq = 10)
dfmat_innaug %>% dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

# Group Documents
dfmat_innaug %>% dfm_group(groups = Party)

# Feature Co-Occurence Matrix
corp_news <- download("data_corpus_guardian")
toks_news <- corp_news %>% tokens(remove_punct = TRUE)
dfmat_news <- toks_news %>% dfm()
dfmat_news <- toks_news %>% dfm()
dfmat_news <- dfmat_news %>% dfm_remove(pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"))
dfmat_news <- dfmat_news %>% dfm_trim(min_termfreq = 100)
dfmat_news %>% topfeatures()

fcmat_news <- dfmat_news %>% fcm() 
feats_news <- names(topfeatures(fcmat_news, 50))
fcmat_news_select <- fcmat_news %>% fcm_select(pattern = feats_news, selection = "keep")

set.seed(144)
size <- log(colSums(dfm_select(dfmat_news, feats_news, selection = "keep")))
textplot_network(fcmat_news_select, min_freq = 0.8, vertex_size = size / max(size) * 3)

# Statistical Analysis ----

# Simple Frequency Analysis
corp_tweets <- download(url = "https://www.dropbox.com/s/846skn1i5elbnd2/data_corpus_sampletweets.rds?dl=1")
toks_tweets <- corp_tweets %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_keep(pattern = "#*")
dfmat_tweets <- toks_tweets %>% dfm()
tstat_freq <- dfmat_tweets %>% 
  textstat_frequency(n = 5, groups = lang)

dfmat_tweets %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(reorder(feature, frequency), frequency)) +
  geom_point() + coord_flip() +
  labs(x = NULL)

# Lexical Diversity
tstat_lexdiv <- dfmat_innaug %>% textstat_lexdiv()

# Document Feature Similarity
tstat_dist <- as.dist(textstat_dist(dfmat_innaug))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance")

# Relative Frequency Analysis
tstat_key <- textstat_keyness(dfmat_news, target = year(dfmat_news$date) >= 2016)
tstat_key %>% textplot_keyness()

# Advance Operations ----
# Scaling and Classification ----
