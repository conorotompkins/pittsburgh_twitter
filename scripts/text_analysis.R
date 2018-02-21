library(tidyverse)
library(tidytext)
library(ggraph)
df_bill <- read_csv("data/bill_peduto_tweets.tweets.csv")

df_bill %>% 
  filter(is_quote == FALSE, is_retweet == FALSE) %>% 
  select(status_id, text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) -> df_bigrams

df_bigrams %>% 
  count(bigram, sort = TRUE)

df_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) -> df_bigrams_separate

df_bigrams_separate
df_bigrams_separate %>% 
  count(word1, word2, sort = TRUE) -> df_bigrams_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph