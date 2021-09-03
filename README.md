<img src="Images/TextMining.PNG" width="350">
<img src="Images/TextMining2.PNG" width="350">

# Natural Language Understanding

## Word Vectors
> Turney & Pantel: "If units of text have similar vectors in a text frequency matrix, then they tend to have similar meanings"

<img src="Images/Stanford/WordVector/Overview.PNG" width="500">

### Matrix Designs

#### Word x Word
<img src="Images/Stanford/WordVector/WordxWord.PNG" width="500">

#### Word x Document
<img src="Images/Stanford/WordVector/WordxDoc.PNG" width="500">

#### Word x Discourse Context
<img src="Images/Stanford/WordVector/WordxDiscourseContext.PNG" width="500">

### Co-occurence
<img src="Images/Stanford/WordVector/Cooccurence.PNG" width="500">

### Vector Comparison
<img src="Images/Stanford/WordVector/Euclidean.PNG" width="500">
<img src="Images/Stanford/WordVector/norm.PNG" width="500">
<img src="Images/Stanford/WordVector/norm2.PNG" width="500">
<img src="Images/Stanford/WordVector/cosine.PNG" width="500">

  - used most often because most NLP problems we care about how words are alike

<img src="Images/Stanford/WordVector/matching.PNG" width="500">
<img src="Images/Stanford/WordVector/KL.PNG" width="500">
<img src="Images/Stanford/WordVector/DistanceMetric.PNG" width="500">
<img src="Images/Stanford/WordVector/DistanceSummary.PNG" width="500">

### Reweighting
<img src="Images/Stanford/WordVector/GOALSReweighting.PNG" width="500">
<img src="Images/Stanford/WordVector/ObsExp.PNG" width="500">
<img src="Images/Stanford/WordVector/PMI.PNG" width="500">
<img src="Images/Stanford/WordVector/TFIDF.PNG" width="500">
<img src="Images/Stanford/WordVector/ttest.PNG" width="500">
<img src="Images/Stanford/WordVector/PLOTweights.PNG" width="500">
<img src="Images/Stanford/WordVector/WeightSummary.PNG" width="500">

### Subword Information
<img src="Images/Stanford/WordVector/SubwordMotivation.PNG" width="500">

### Dimensionality Reduction
<img src="Images/Stanford/WordVector/LSA.PNG" width="500">
<img src="Images/Stanford/WordVector/LSA2.PNG" width="500">
<img src="Images/Stanford/WordVector/LSAEx.PNG" width="500">
<img src="Images/Stanford/WordVector/Autoencoders.PNG" width="500">
<img src="Images/Stanford/WordVector/AutoencodersIntuition.PNG" width="500">
<img src="Images/Stanford/WordVector/Glove.PNG" width="500">
<img src="Images/Stanford/WordVector/GloveMethod.PNG" width="500">
<img src="Images/Stanford/WordVector/GloveWeight.PNG" width="500">
<img src="Images/Stanford/WordVector/GloveHyperparameters.PNG" width="500">
<img src="Images/Stanford/WordVector/GloveEx.PNG" width="500">
<img src="Images/Stanford/WordVector/GloveComparisons.PNG" width="500">
<img src="Images/Stanford/WordVector/Word2Vec.PNG" width="500">
<img src="Images/Stanford/WordVector/Word2Vec2.PNG" width="500">
<img src="Images/Stanford/WordVector/Word2VecSkipGram.PNG" width="500">
<img src="Images/Stanford/WordVector/Word2VecSkipGram2.PNG" width="500">

### Retrofitting
<img src="Images/Stanford/WordVector/RetrofittingGOALS.PNG" width="500">
<img src="Images/Stanford/WordVector/RetrofittingProsCons.PNG" width="500">
<img src="Images/Stanford/WordVector/RetrofittingModel.PNG" width="500">
<img src="Images/Stanford/WordVector/RetrofittingEx.PNG" width="500">
<img src="Images/Stanford/WordVector/RetrofittingEx2.PNG" width="500">
<img src="Images/Stanford/WordVector/RetrofittingEx3.PNG" width="500">

## Sentiment Analysis


# Regular Expressions

# Pipeline

# Text Preprocessing
> **Corpus:** collection of text documents. _[ Corpus / Documents / Paragraphs / Sentences / Tokens ]_.\
**Tokens:** smaller units of text. _[ words / phrases / ngrams]_.\
**Ngrams:** combination of N words

## TOKENIZATION

**White Space Tokenizer _Bag of Words_**
- Sentence: "I went to NY to play football"
- Tokens: "I", "went", "to", "NY", "to", "play", "football"

**Regular Expression Tokenizer**

**Normalization**
> **Morpheme:** base form of a word.
> **Normalization:** process of converting a token into its base form

## STEMMING
<img src="Images/stemming.PNG" width="500">

_may generate non-meaningful terms_

## LEMMATIZATION
<img src="Images/lemmatization.PNG" width="500">

_makes use of vocabulary, word structure, part of speech tagging and grammer relations_

## Part of Speech Tagging
> Defines the syntactic context and role of words in the Sentence. _[ Nouns | Verbs | Adjectives | Adverbs ]_

## Term - Frequency Inverse-Document-Frequency
<img src="Images/tfidf.PNG" width="500">

# Latent Dirichlet Allocation

<img src="Images/LDA3_Graphic.PNG" width="800">
<img src="Images/LDA3_Formula.PNG" width="800">
<img src="Images/LDA3_Formula_FINAL.PNG" width="800">

## Dirichlet Distribution
<img src="Images/LDA3_Dirichlet1.PNG" width="800">
<img src="Images/LDA3_Dirichlet2.PNG" width="800">

**Document-Topics:** associates documents with their corresponding topics
<img src="Images/LDA3_Topics1.PNG" width="800">
<img src="Images/LDA3_Topics2.PNG" width="300">
<img src="Images/LDA3_Formula_DocumentTopic.PNG" width="800">

**Topic-Words:** associates topics with their corresponding words
<img src="Images/LDA3_Words.PNG" width="800">
<img src="Images/LDA3_Formula_TopicWord.PNG" width="800">

## Gibbs Sampling

**Properties**
1. Documents are as monochromatic as possible
2. Words are as monochromatic as possible

<img src="Images/LDA3_Gibbs1.PNG" width="800">
<img src="Images/LDA3_Gibbs2.PNG" width="800">

## Determining the number of k
- **Topic Coherence:** examine the words in topics, decide if they make sense
- **Log-Likelihood:** how plausible model parameters are given the data
- **Perplexity:** a measure of model "surprise" at the data
