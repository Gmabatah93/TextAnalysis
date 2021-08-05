# Regular Expressions

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

# Latent Dirichlet Allocation: Explanation 1
<img src="Images/LDA_Algorithm.PNG" width="500">

1. Initialize parameters
<img src="Images/LDA_Algorithm.PNG" width="500">

2. Initialize topic assignments randomly
<img src="Images/LDA_topic.PNG" width="500">

3. Iterate for each word in each document:
  - Resample topic for word, given all other words and their current topic assignments
<img src="Images/LDA_Iterate.PNG" width="500">
<img src="Images/LDA_Resample.PNG" width="500">

4. Get results
5. Evaluate Model

# Latent Dirichlet Allocation: Explanation 2

## Generative Model
> 1. Choose a distribution _(Dirichlet)_ over topics
> 2. For each word draw a colored _"coin"_ from the distribution
> 3. Look the distribution _(over terms associated with that coin)_ and draw the word from that distribution
> 4. Repeat for each document

<img src="Images/LDA1.PNG" width="800">

- Each **Topic** is a distribution over terms in the _fixed_ vocabulary
- Different **Topics** has different words with different probabilities

## Graphical Model
<img src="Images/Graphic.PNG" width="500">
<img src="Images/Graphic_LDA.PNG" width="800">

- Topics:
  + Each **topic** is a distribution _(Dirichlet)_ over terms

- Topic Assignment 'colored coin"
  + there's a z "colored coin" for every word.

## Dirichlet Distribution

- The parameter **a** controls the mean shape and sparsity of Thetha  


# Latent Dirichlet Allocation: Explanation 3

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
