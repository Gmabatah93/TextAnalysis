# Natural Language Understanding
> [Reference](https://www.youtube.com/playlist?list=PLoROMvodv4rObpMCir6rNNUlFAn56Js20)

- [WORD VECTORS](https://github.com/Gmabatah93/TextAnalysis#word-vectors)
  + [Matrix Designs](https://github.com/Gmabatah93/TextAnalysis#matrix-designs)
  + [Co-occurence](https://github.com/Gmabatah93/TextAnalysis#co-occurence)
  + [Vector Comparison](https://github.com/Gmabatah93/TextAnalysis#vector-comparison)
  + [Reweighting](https://github.com/Gmabatah93/TextAnalysis#reweighting)
  + [Subword Information](https://github.com/Gmabatah93/TextAnalysis#subword-information)
  + [Dimensionality Reduction](https://github.com/Gmabatah93/TextAnalysis#dimensionality-reduction)
  + [Retrofitting](hhttps://github.com/Gmabatah93/TextAnalysis#relation-extractionttps://github.com/Gmabatah93/TextAnalysis#retrofitting)
- [SENTIMENT ANALYSIS](https://github.com/Gmabatah93/TextAnalysis#sentiment-analysis)
  + [Text Preprocessing](https://github.com/Gmabatah93/TextAnalysis#text-preprocessing)
  + [Stanford Sentiment Treebank](https://github.com/Gmabatah93/TextAnalysis#stanford-sentiment-treebank)
  + [Hyperparameter & Classifier Comparison](https://github.com/Gmabatah93/TextAnalysis#hyperparameters--classifier-comparison)
  + [Feature Representation](https://github.com/Gmabatah93/TextAnalysis#feature-representation)
  + [Neural Networks](https://github.com/Gmabatah93/TextAnalysis#neural-networks)
- [RELATION EXTRACTION](https://github.com/Gmabatah93/TextAnalysis#relation-extraction)
  + [Tasks](https://github.com/Gmabatah93/TextAnalysis#tasks)
  + [Pipeline](https://github.com/Gmabatah93/TextAnalysis#pipeline)
  + [Enhancements](https://github.com/Gmabatah93/TextAnalysis#enhancements)

## WORD VECTORS
> Turney & Pantel: "If units of text have similar vectors in a text frequency matrix, then they tend to have similar meanings"

<img src="Images/WordVector/Overview.PNG" width="500">

### Matrix Designs

#### Word x Word
<img src="Images/WordVector/WordxWord.PNG" width="500">

#### Word x Document
<img src="Images/WordVector/WordxDoc.PNG" width="500">

#### Word x Discourse Context
<img src="Images/WordVector/WordxDiscourseContext.PNG" width="500">

### Co-occurence
<img src="Images/WordVector/Cooccurence.PNG" width="500">

### Vector Comparison
<img src="Images/WordVector/Euclidean.PNG" width="500">
<img src="Images/WordVector/norm.PNG" width="500">
<img src="Images/WordVector/norm2.PNG" width="500">
<img src="Images/WordVector/cosine.PNG" width="500">

  - used most often because most NLP problems we care about how words are alike

<img src="Images/WordVector/matching.PNG" width="500">
<img src="Images/WordVector/KL.PNG" width="500">
<img src="Images/WordVector/DistanceMetric.PNG" width="500">
<img src="Images/WordVector/DistanceSummary.PNG" width="500">

### Reweighting
<img src="Images/WordVector/GOALSReweighting.PNG" width="500">
<img src="Images/WordVector/ObsExp.PNG" width="500">
<img src="Images/WordVector/PMI.PNG" width="500">
<img src="Images/WordVector/TFIDF.PNG" width="500">
<img src="Images/WordVector/ttest.PNG" width="500">
<img src="Images/WordVector/PLOTweights.PNG" width="500">
<img src="Images/WordVector/WeightSummary.PNG" width="500">

### Subword Information
<img src="Images/WordVector/SubwordMotivation.PNG" width="500">

### Dimensionality Reduction
<img src="Images/WordVector/LSA.PNG" width="500">
<img src="Images/WordVector/LSA2.PNG" width="500">
<img src="Images/WordVector/LSAEx.PNG" width="500">
<img src="Images/WordVector/tsne.PNG" width="500">
<img src="Images/WordVector/Autoencoders.PNG" width="500">
<img src="Images/WordVector/AutoencodersIntuition.PNG" width="500">
<img src="Images/WordVector/Glove.PNG" width="500">
<img src="Images/WordVector/GloveMethod.PNG" width="500">
<img src="Images/WordVector/GloveWeight.PNG" width="500">
<img src="Images/WordVector/GloveHyperparameters.PNG" width="500">
<img src="Images/WordVector/GloveEx.PNG" width="500">
<img src="Images/WordVector/GloveComparisons.PNG" width="500">
<img src="Images/WordVector/Word2Vec.PNG" width="500">
<img src="Images/WordVector/Word2Vec2.PNG" width="500">
<img src="Images/WordVector/Word2VecSkipGram.PNG" width="500">
<img src="Images/WordVector/Word2VecSkipGram2.PNG" width="500">

### Retrofitting
<img src="Images/WordVector/RetrofittingGOALS.PNG" width="500">
<img src="Images/WordVector/RetrofittingProsCons.PNG" width="500">
<img src="Images/WordVector/RetrofittingModel.PNG" width="500">
<img src="Images/WordVector/RetrofittingEx.PNG" width="500">
<img src="Images/WordVector/RetrofittingEx2.PNG" width="500">
<img src="Images/WordVector/RetrofittingEx3.PNG" width="500">

---

## SUPERVISED: SENTIMENT ANALYSIS
<img src="Images/Sentiment/Papers.PNG" width="500">

### Text Preprocessing
<img src="Images/Sentiment/Token.PNG" width="500">
<img src="Images/Sentiment/TokenTreeBank.PNG" width="500">
<img src="Images/Sentiment/TokenAware.PNG" width="500">
<img src="Images/Sentiment/TokenAware2.PNG" width="500">
<img src="Images/Sentiment/Stemming.PNG" width="500">
<img src="Images/Sentiment/StemmingPorter.PNG" width="500">
<img src="Images/Sentiment/StemmingLancaster.PNG" width="500">
<img src="Images/Sentiment/StemmingWordnet.PNG" width="500">
<img src="Images/Sentiment/TokenPOS.PNG" width="500">
<img src="Images/Sentiment/Negation.PNG" width="500">
<img src="Images/Sentiment/Negation2.PNG" width="500">

### Stanford Sentiment Treebank
<img src="Images/Sentiment/Treebank.PNG" width="500">
<img src="Images/Sentiment/Treebank2.PNG" width="500">
<img src="Images/Sentiment/Root.PNG" width="500">
<img src="Images/Sentiment/Root2.PNG" width="500">

### Hyperparameters & Classifier Comparison
<img src="Images/Sentiment/Hyperparameter.PNG" width="500">
<img src="Images/Sentiment/ClassifierComparison.PNG" width="500">

### Feature Representation
<img src="Images/Sentiment/Bigrams.PNG" width="500">
<img src="Images/Sentiment/HandbuiltNegation.PNG" width="500">
<img src="Images/Sentiment/Scope.PNG" width="500">
<img src="Images/Sentiment/HandbuiltOther.PNG" width="500">
<img src="Images/Sentiment/Assessment.PNG" width="500">
<img src="Images/Sentiment/DistributedRep.PNG" width="500">

### Neural Networks
<img src="Images/Sentiment/RNNModel.PNG" width="500">
<img src="Images/Sentiment/RNNPrep.PNG" width="500">
<img src="Images/Sentiment/LSTM.PNG" width="500">

**TreeNN**

<img src="Images/Sentiment/TreeNN.PNG" width="500">
<img src="Images/Sentiment/TreeNN2.PNG" width="500">
<img src="Images/Sentiment/TreeNN3.PNG" width="500">

## Relation Extraction
> [Reference](https://github.com/cgpotts/cs224u/blob/master/rel_ext_01_task.ipynb)

> **TASK:** extracting from natural language text relational triples. If we can accumulate a large knowledge base of relational triples, we can use it to power _question answering_ and other applications  \
> **Distance Supervision:**

### Applications
<img src="Images/RelationExtraction/Task.PNG" width="500">
<img src="Images/RelationExtraction/Task2.PNG" width="500">
<img src="Images/RelationExtraction/Task3.PNG" width="500">


### Tasks
<img src="Images/RelationExtraction/TaskDef.PNG" width="500">
<img src="Images/RelationExtraction/HandBuilt.PNG" width="500">
<img src="Images/RelationExtraction/Supervised.PNG" width="500">
<img src="Images/RelationExtraction/DistanceSupervision.PNG" width="500">
<img src="Images/RelationExtraction/DistanceSupervision2.PNG" width="500">

### Pipeline

**_INPUT_**
1. **Corpus:** we need to be able to identify entities in the text and connect them to a knowledge base of relations between entities. So, we need a corpus in which entity mentions are annotated with entity resolutions which map them to unique, unambiguous identifiers. Entity resolution serves two purposes:
  - It ensures that if an entity mention could refer to two different entities, it is properly disambiguated. For example, "New York" could refer to the city or the state.
  - It ensures that if two different entity mentions refer to the same entity, they are properly identified. For example, both "New York City" and "The Big Apple" refer to New York City
2. **The Knowledge Base:** a collection of relational triples, each consisting of a relation, a subject, and an object.
    - <img src="Images/RelationExtraction/KBEx.PNG" width="500">
    - High-Level Characteristics of the KB
      - How many relations are there ?
      - How big is each relation ?
      - Examples of each relation.
      - How many unique entities does the KB include ?
3. **Problem Formulation:**
  - What is the input to the prediction ?
    - Is it a specific pair of entity mentions in a specific document ?
    - Or is it a pair of entities, apart from any specific mentions ?
  - What is the output of the prediction ?
    - Do we need to predict at most one relation label ?
    - Or can we predict multiple relation labels ?
4. **Joining the corpus and the KB:**
  - Use the KB to generate labels for the corpus.
  - Use the corpus to generate features for entity pairs.
5. **Negative Examples:**

**_OUTPUT_**
1. **Multi-Label Classification**
2. **Build dataset**
3. **Evaluation**
4. **Baseline Model**

### Enhancements
<img src="Images/RelationExtraction/EnhanceFeatures.PNG" width="500">
<img src="Images/RelationExtraction/EnhanceModels.PNG" width="500">
