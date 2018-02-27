library(tidyverse)
library(tidytext)
library(lubridate)
library(rtweet)
library(scales)

#need to do network analysis for each senator
source("scripts/tidytext_functions.R")

set.seed(1234)
df_casey <- read_csv("data/tweets_casey.tweets.csv")

df_toomey <- read_csv("data/tweets_toomey.tweets.csv")

df_casey

casey_stopwords <- c("0085")
casey_replacers <- c("'s")
#add casey replacer for "'s"
#need to get rid of "itâ€™s"
tweets_casey <- count_twitter_bigrams(df_casey, custom_stopwords = casey_stopwords) %>% 
  mutate(senator = "Casey")
tweets_casey

visualize_bigrams(tweets_casey, 15,
                  title = "@SenBobCasey tweets",
                  subtitle = "Bigram network",
                  caption = "@conor_tompkins")

#need to get rid of "amp" connected to "senbobcasey
toomey_stopwords <- c("0085")
tweets_toomey <- count_twitter_bigrams(df_toomey, custom_stopwords = toomey_stopwords) %>% 
  mutate(senator = "Toomey")
tweets_toomey

visualize_bigrams(tweets_toomey, 15,
                  title = "@SenToomey tweets",
                  subtitle = "Bigram network",
                  caption = "@conor_tompkins")

#trying to create df of combined tweets so I can facet network graph by senator
df_combined <- bind_rows(df_casey, df_toomey)
df_combined
combined_stopwords <- c("0085")

bind_rows(tweets_casey, tweets_toomey) -> tweets_combined
tweets_combined

a <- grid::arrow(type = "closed", 
                 length = unit(.1, "inches"))
tweets_combined %>%
  filter(n >= 15) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), size = 6, vjust = 1, hjust = 1) +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = TRUE, arrow = a, end_cap = circle(.25, 'inches')) +
  facet_edges(~senator) + #facet_edges works, but not facet_nodes
  scale_edge_width_continuous("Count", range = c(.1, 2)) +
  scale_edge_alpha_continuous("Count", range = c(.3, .7)) +
  labs(title = "title",
       subtitle = "subtitle",
       caption = "caption") +
  theme_void(base_size = 18) +
  th_foreground(foreground = 'grey80', border = TRUE)

#need to do correlation plots