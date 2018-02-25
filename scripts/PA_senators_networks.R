library(tidyverse)
library(tidytext)
library(lubridate)
library(rtweet)
library(scales)

#need to do network analysis for each senator
source("scripts/tidytext_functions.R")

df_casey <- read_csv("data/tweets_casey.tweets.csv")

df_toomey <- read_csv("data/tweets_toomey.tweets.csv")

count_bigrams
df_casey

casey_stopwords <- c("t.co", "https", "0085", "&amp")
#add casey replacer for "'s"
#need to get rid of "itâ€™s"
tweets_casey <- count_bigrams_basic(df_casey, custom_stopwords = casey_stopwords)
tweets_casey

visualize_bigrams(tweets_casey, 10,
                  title = "@SenBobCasey tweets",
                  subtitle = "Bigram network",
                  caption = "@conor_tompkins")

df_casey %>% 
  select(text) %>% 
  filter(str_detect(text, "time"))

#need to get rid of "amp" connected to "senbobcasey
toomey_stopwords <- c("t.co", "https", "http", "&amp")
tweets_toomey <- count_bigrams_basic(df_toomey, custom_stopwords = toomey_stopwords)
tweets_toomey

visualize_bigrams(tweets_toomey, 10,
                  title = "@SenToomey tweets",
                  subtitle = "Bigram network",
                  caption = "@conor_tompkins")

df_toomey %>% 
  select(text) %>% 
  filter(str_detect(text, "public"),
         str_detect(text, "safety"))
