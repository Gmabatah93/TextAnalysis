# quanteda

> Reference
> - [tutorial](https://tutorials.quanteda.io/)
> - [quanteda.io](https://quanteda.io/index.html)
## Corpus
Function | Description | Parameters
--- | --- | ---
corpus() | creates a corpus
corpus_reshape() | recast the document units of a corpus
corpus_sample() | ramdomly sample documents from a corpus
corpus_segment() | segment texts into component elements
corpus_subset() | extract a subset of a corpus
corpus_trim() | remove sentences based on their token lengths or a pattern match

## Tokens
Function | Description | Parameters
--- | --- | ---
tokens() | creates a tokenized text object from a corpus
tokens_compound() | convert token sequences into compound tokens
tokens_lookup() | apply a dictionary to a token object
tokens_select(), tokens_remove() | select or remove tokens from a tokens object
tokens_ngrams(), tokens_skipgrams() | create ngrams and skipgrams from tokens
tokens_tolower(), tokens_toupper() | convert the case of tokens
tokens_wordstem() | stem the terms of an object

## Document-Feature Matrix
Function | Description | Parameters
--- | --- | ---
dfm(), fcm() | creates a document-feature matrix or feature co-occurence matrix
dfm_group() | recombine a dfm by a grouping variable
dfm_lookup() | apply a dictionary to a dfm
dfm_select(), dfm_remove() | select features from a dfm
dfm_trim() | trim a dfm using frequency threshold-based feature selection
dfm_weight() | weight a dfm, including full SMART scheme, tf-idf, etc, in a dfm
dfm_wordstem() | stem the features in a dfm

## Statistical Analytic Functions
Function | Description | Parameters
--- | --- | ---
textstat_collacations() | calculate collocation statistics
textstat_dist() | distance computation between documents or features
textstat_keyness() | calculate keyness statistic
textstat_lexdiv() | calculate lexical diversity
textstat_readability() | calculate readability
textstat_simil() | similarity computation between documents of features

## Machine Learning Functions
Function | Description | Parameters
--- | --- | ---
textmodel_ca() | Correspondence Analysis of a document-feature matrix
textmodel_lsa() | Latent Sementic Analysis of a document-feature matrix
textmodel_nb() | Naive Bayes classifier for texts
textmodel_svm() | Support Vector Machine
textmodel_lr() | Penalized Logistic Regression for text
textmodel_wordfish() | Slapin and Prokosh (2008) text scaling model
textmodel_wordscores() | Laver, Benoit and Garry (2003) text scaling
textmodel_affinity() | Perry and Benoit (2017) class affinity scaling

## Visualization Functions
Function | Description | Parameters
--- | --- | ---
textplot_scale1d() | Fitted scaling textmodel
textplot_wordcloud() | wordcloud
textplot_xray() | dispersion of key word(s)
textplot_keyness() | association of words with target v. reference set

## Other
Function | Description | Parameters
--- | --- | ---
dictionary() | create or import a word lexicon
ndoc(), ntoken(), ... | R-familiar utility functions
kwic() | locate keywords-in-context

## Sentiment

## Tidy
