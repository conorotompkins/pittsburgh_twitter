library(tidyverse)
library(httpuv)
library(openssl)
library(httr)
library(rtweet)
library(lubridate)

theme_set(theme_bw(base_size = 18))

#this script should open a popoup window where I authenticate with the API
rt <- search_tweets(
  "#rstats", n = 18, include_rts = FALSE
)

## get user IDs of accounts followed by CNN
tmls <- get_timelines(c("cnn", "BBCWorld", "foxnews"), n = 3200)

## plot the frequency of tweets for each user over time
tmls %>%
  dplyr::filter(created_at > "2017-10-29") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

df_bill <- get_timelines("billpeduto", n = 3200)
#save_as_csv(df_bill, "data/bill_peduto_tweets.csv")

df_bill %>% 
  filter(str_detect(text, "snow|ice|pothole")) %>% 
  select(status_id, text)

df_bill %>% 
  filter(str_detect(text, "snow|ice|pothole")) %>% 
  mutate(date = ymd(str_sub(created_at, 1, 10)),
         snow = str_detect(text, " snow "),
         ice = str_detect(text, " ice "),
         sleet = str_detect(text, " sleet "),
         rain = str_detect(text, " rain "),
         pothole = str_detect(text, " pothole ")) %>% 
  select(status_id, date, text, snow, ice, pothole) %>% 
  gather(key = topic, value = status, -c(status_id, date, text)) %>% 
  filter(status == TRUE) -> df_tweets_text

df_tweets_text %>% 
  count(date, topic) -> df_tweets


df_tweets %>% 
  ggplot(aes(date, n, color = topic)) +
  geom_point()

df_tweets %>% 
  ggplot(aes(date, topic, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_x_date(expand = c(0,0)) +
  theme(panel.grid = element_blank())

###Citations
#https://github.com/mkearney/rtweet
#http://rtweet.info/
#http://rtweet.info/articles/intro.html
#https://stackoverflow.com/questions/47681690/no-twitter-authorization-prompt-when-using-rtweet-package/48275078#48275078
#https://stackoverflow.com/questions/47910979/setting-up-rtweet-for-r-in-aws-ubuntu-server
#https://github.com/r-lib/httr/issues/156
#https://github.com/mkearney/rtweet/issues/75
#https://github.com/geoffjentry/twitteR/issues/65
#https://github.com/r-lib/httr/blob/master/demo/oauth1-twitter.r



