library(tidyverse)
library(tidytext)
library(ggraph)
library(igraph)
library(widyr)
library(SnowballC)

theme_set(theme_void())

df_bill <- read_csv("data/bill_peduto_tweets.tweets.csv")

set.seed(2017)

df_bill %>% 
  filter(is_quote == FALSE, is_retweet == FALSE) %>% 
  select(status_id, text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) -> df_bigrams

df_bigrams %>% 
  count(bigram, sort = TRUE)

bill_stop_words <- c("t.co", "https", "amp")

#need to manually stem words. "lane", "lanes" etc
df_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1 %in% bill_stop_words) %>% 
  filter(!word2 %in% bill_stop_words) -> df_bigrams_separate

df_bigrams_separate
df_bigrams_separate %>% 
  count(word1, word2, sort = TRUE) -> df_bigrams_counts

df_bigrams_counts

bigram_graph <- df_bigrams_counts %>%
  filter(n > 2) %>%
  graph_from_data_frame()

bigram_graph

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



#pairwise correlation
bill_tweet_words <- df_bill %>%
  filter(is_quote == FALSE, is_retweet == FALSE) %>% 
  select(status_id, text) %>% 
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         !word %in% bill_stop_words) %>% 
  mutate(word = str_replace(word, "'", ""),
         word = str_replace(word, "’", ""),
         word = wordStem(word))

bill_tweet_words

word_pairs <- bill_tweet_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 == "pittsburgh")

word_cors <- bill_tweet_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors
word_cors %>%
  filter(item1 == "amazon")

word_cors %>%
  filter(item1 %in% c("pittsburgh", "amazon")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  theme_bw()

word_cors %>%
  filter(correlation > .05) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_label(aes(label = name), size = 5, repel = FALSE) +
  scale_edge_alpha_continuous(range = c(.1, .5)) +
  theme_void()

?geom_node_label
?geom_edge_link
#citations
#https://www.tidytextmining.com/ngrams.html
#https://stackoverflow.com/questions/43344108/word-substitution-within-tidy-text-format