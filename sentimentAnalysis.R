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


# BING: 
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

rm_bing %>% 
  count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative) %>% 
  mutate(pos_neg = ifelse(polarity > 0, "positive","negative")) %>% 
  filter(abs(polarity) >= 10) %>% 
  ggplot(aes(fct_reorder(word, polarity), polarity, fill = pos_neg)) +
  geom_col() +
  coord_flip()

rm_bing %>% 
  filter(name == "Rick") %>% 
  count(index, season_num, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative,
         season_num = factor(season_num)) %>% 
  ggplot(aes(index, polarity, color = season_num)) +
  geom_smooth(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red")



# NRC
rm_nrc <- rm_tidy %>% inner_join(get_sentiments("nrc"))
rm_nrc %>% count(sentiment)  %>% arrange(-n)

rm_nrc %>% 
  filter(!grepl("positive|negative", sentiment)) %>% 
  count(sentiment, word) %>% 
  spread(sentiment, n, fill = 0) %>% 
  data.frame(row.names = "word") %>% 
  wordcloud::comparison.cloud(max.words = 50,
                              title.size = 1.5)


rm_nrc %>% 
  filter(!grepl("positive|negative", sentiment)) %>% 
  filter(name %in% rm_characters) %>% 
  count(name, sentiment) %>% 
  spread(name, n, fill = 0) 
  
rm_nrc %>% 
  filter(name %in% rm_characters) %>% 
  filter(!grepl("positive|negative", sentiment)) %>%
  count(name, sentiment) %>% 
  group_by(name) %>% 
  mutate(percent_positive = 100 * n / sum(n)) %>% 
  ggplot(aes(name, percent_positive, fill = sentiment)) +
  geom_col()
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





# AFINN
rm_afinn <- rm_tidy %>% inner_join(get_sentiments("afinn"))
rm_afinn %>% 
  count(season_num, index, value) %>% 
  group_by(season_num, index) %>% 
  summarise(total_value = sum(value)) %>%
  ungroup() %>% 
  ggplot(aes(index, total_value, color = factor(season_num))) +
  geom_smooth(show.legend = FALSE)

rm_afinn %>% 
  filter(name %in% rm_characters) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = 0.09)

# Family Guy: Data ----
family_raw <- read_csv("Data/Family_guy_dialog.csv")
family <- family_raw %>% 
  mutate(index = row_number()) %>% 
  select(index, season = seasons, character, dialog) %>% 
  mutate(season = str_replace(season, "season", "") %>% as.integer %>% as.factor) %>% 
  filter(character != "Maids and Butlers M&B")

# Counts
family %>% count(character) %>% arrange(-n)
family %>% count(season)

# EDA
family %>% 
  count(season, character) %>% 
  ggplot(aes(fct_reorder(character, n), n, fill = season)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~season, scales = "free_x", nrow = 3) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank())

#
# Family Guy: tidytext ----

# Bag of Words
family_tidy <- family %>% 
  unnest_tokens(word, dialog) %>% 
  anti_join(stop_words)

# - BOW: Popular Words (Character)
family_tidy %>% 
  group_by(character) %>%
  count(word) %>% 
  arrange(-n) %>% 
  top_n(3, n) %>% 
  ggplot(aes(n, fct_reorder(word,n), fill = character)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~character, scales = "free")
# - BOW: Popular Words (Character per Season)
gg_tf_Character_Season <- function(season_num) {
  
  family_tidy %>% 
    filter(season == season_num) %>% 
    group_by(character) %>%
    count(word) %>% 
    arrange(-n) %>% 
    top_n(3, n) %>% 
    ggplot(aes(n, fct_reorder(word,n), fill = character)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~character, scales = "free") +
    theme_bw() +
    labs(title = paste0("Season: ", season_num))

}

gg_tf_Character_Season(12)


# Term Frequency
family_tf <- family_tidy %>% 
  count(word) %>% 
  arrange(-n)

# TfIdf
family_tf_idf <- family_tidy %>% 
  count(season, word) %>% 
  bind_tf_idf(term = word,document = season,n = n)

# Bigram
family_bigram <- family %>% 
  unnest_tokens(bigram, dialog,
                token = "ngrams",
                n = 2)
family_bigram_clean_seperate <- family_bigram %>% 
  separate(bigram, c("word1","word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

family_bigram_clean_unite <- 
  family_bigram_clean_seperate %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  filter(bigram != "NA NA")

family_bigram_clean_unite %>% count(bigram) %>% arrange(-n)
# - Bigram: Popular Words (Character)
family_bigram_clean_unite %>% 
  group_by(character) %>%
  count(bigram) %>% 
  arrange(-n) %>% 
  top_n(3, n) %>% 
  ggplot(aes(n, fct_reorder(bigram,n), fill = character)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~character, scales = "free")
# - Bigram: Popular Words (Character per Season)
gg_bigram_Character_Season <- function(season_num) {
  
  family_bigram_clean_unite %>% 
    filter(season == season_num) %>% 
    group_by(character) %>%
    count(bigram) %>% 
    arrange(-n) %>% 
    top_n(3, n) %>% 
    ggplot(aes(n, fct_reorder(bigram,n), fill = character)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~character, scales = "free") +
    theme_bw() +
    labs(title = paste0("Season: ", season_num))
  
}

gg_bigram_Character_Season(3)

#
# Family Guy: Sentiment Analysis ----

# BING
family_bing <- family_tidy %>% inner_join(get_sentiments("bing"))
family_bing %>% count(sentiment)

family_bing %>% 
  count(index, season, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative) %>% 
  ggplot(aes(index, polarity)) +
  geom_smooth(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Polarity of Entire Series")

family_bing %>% 
  count(word, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative) %>% 
  mutate(pos_neg = ifelse(polarity > 0, "positive","negative")) %>% 
  filter(abs(polarity) >= 5) %>% 
  ggplot(aes(fct_reorder(word, polarity), polarity, fill = pos_neg)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Polarity of Words",
       x = NULL)

family_bing %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  labs(title = "Polarity: Top 10 Words",
       x = NULL, y = NULL) + theme_bw()


# - Seasons
family_bing %>% 
  count(index, season, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative) %>% 
  ggplot(aes(index, polarity)) +
  geom_smooth(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~season, scales = "free_x", nrow = 2) +
  labs(title = "Polarity of Entire Series",
       subtitle = "by Season") +
  theme_bw()

family_bing %>% 
  ggplot(aes(sentiment, fill = sentiment)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~season, nrow = 2) +
  theme_bw() +
  labs(title = "Polarity by Seasons",
       x = NULL, y = NULL)
family_bing %>% 
  count(season, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(overall = positive - negative)
# - Seasons: Top Words
gg_topWords_Season <- function(season_num) {
  
  family_bing %>% 
    filter(season == season_num) %>% 
    group_by(season, sentiment) %>%
    count(word, sort = TRUE) %>%
    top_n(3, n) %>% 
    ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free") +
    theme_bw() +
    labs(
      title = "Top Words",
      subtitle = paste0("Season: ", season_num),
      x = NULL, y = NULL
    )
  
}
gg_topWords_Season(6)

# - Characters
gg_polarity_Season_Character <- function(name) {
    
  family_bing %>%
    filter(character == name) %>% 
    count(index, season, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(polarity = positive - negative) %>% 
    ggplot(aes(index, polarity)) +
    geom_smooth(show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "red") +
    facet_wrap(~season, scales = "free_x", nrow = 2) +
    labs(title = "Polarity of Entire Series by Season",
         subtitle = paste0("Name: ", name)) +
    theme_bw()
}
gg_polarity_Season_Character(name = "Chris")

family_bing %>% 
  count(character, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(overall = positive - negative)
family_bing %>% 
  ggplot(aes(sentiment, fill = sentiment)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~character) +
  theme_bw() +
  labs(title = "Polarity by Character",
       x = NULL, y = NULL)
family_bing %>% 
  count(season, character, sentiment) %>% 
  ggplot(aes(character, n, fill = sentiment)) +
  geom_col(position = "fill", show.legend = FALSE) +
  facet_wrap(~ season, nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "% Polarity of Characters by Season",
       x = NULL, y = NULL)

# - Character: Top Words
family_bing %>%
  filter(sentiment == "negative") %>% 
  count(character, word) %>% 
  group_by(character) %>% 
  top_n(3, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = character)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ character, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Top Words by Character",
       subtitle = "negative",
       x = NULL, y = NULL)
family_bing %>%
  filter(sentiment == "positive") %>% 
  count(character, word) %>% 
  group_by(character) %>% 
  top_n(3, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = character)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ character, scales = "free", nrow = 2) +
  theme_bw() +
  labs(title = "Top Words by Character",
       subtitle = "positive",
       x = NULL, y = NULL)

gg_topWords_Season_Character <- function(season_num) {
  
  family_bing %>% 
    filter(season == season_num) %>% 
    count(character, word, sentiment) %>% 
    group_by(character) %>% 
    top_n(5, n) %>% 
    ungroup() %>% 
    ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ character, scales = "free", nrow = 2) +
    theme_bw() +
    labs(title = paste0("Season ", season_num, ": Top Words by Character"),
         x = NULL, y = NULL)
  
}
gg_topWords_Season_Character(12)

