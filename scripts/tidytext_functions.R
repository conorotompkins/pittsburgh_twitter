library(tidyverse)
library(tidytext)
library(ggraph)
library(igraph)
library(widyr)
library(SnowballC)

count_bigrams <- function(dataset, custom_stopwords, replacer) {
  dataset %>%
    filter(is_quote == FALSE, is_retweet == FALSE) %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    mutate(word1 = str_replace(word1, replacer, ""),
           word2 = str_replace(word2, replacer, "")) %>% 
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word1 %in% custom_stopwords,
           !word2 %in% custom_stopwords) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams, minimum, title, subtitle, caption) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", 
                   length = unit(.1, "inches"))
  
  bigrams %>%
    filter(n >= minimum) %>% 
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), size = 6, vjust = 1, hjust = 1) +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = TRUE, arrow = a, end_cap = circle(.25, 'inches')) +
    scale_edge_width_continuous("Count", range = c(.1, .9)) +
    scale_edge_alpha_continuous("Count", range = c(.3, .9)) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_void(base_size = 18)
}