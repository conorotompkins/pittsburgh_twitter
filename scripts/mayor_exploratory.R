library(tidyverse)
library(lubridate)
library(viridis)
library(scales)

theme_set(theme_bw(base_family = 18))

title <- "Mayor @billpeduto tweets"
caption <- "@conor_tompkins"

df <- read_csv("data/bill_peduto_tweets.tweets.csv")

df %>% 
  arrange(created_at) %>% 
  select(status_id, text, created_at, source, is_quote, is_retweet, display_text_width, favorite_count, reply_to_screen_name) %>% 
  mutate(created_at = with_tz(created_at, "US/Eastern"),
         date = ymd(str_sub(created_at, 1, 10)),
         year = year(date),
         month = month(date, label = TRUE),
         week = week(date),
         wday = wday(date, label = TRUE),
         hour = hour(created_at),
         month_year = str_c(month, year, sep = "-"),
         month_year = factor(month_year, levels = unique(month_year)),
         wday = factor(wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) -> df_bill
df_bill

df_bill %>% 
  count(date) %>%
  ggplot(aes(date, n)) +
  geom_jitter(alpha = .5) +
  geom_smooth(size = 2) +
  labs(title = title, 
       x = "",
       y = "Number of tweets",
       caption = caption)


df_bill %>% 
  select(date, month_year, month, week, is_retweet, is_quote) %>% 
  mutate(tweet_type = case_when(is_retweet == FALSE & is_quote == FALSE ~ "Regular tweet",
                                   is_retweet == TRUE ~ "Retweet",
                                   is_quote == TRUE ~ "Quote")) -> df_bill_tweet_types

df_bill_tweet_types

df_bill_tweet_types %>% 
  count(date, tweet_type) %>% 
  ggplot(aes(date, n, color = tweet_type)) +
  geom_jitter(alpha = .5) +
  geom_smooth(size = 2)

df_bill_tweet_types %>% 
  count(month_year, tweet_type) -> df_bill_tweet_types_month_year

df_bill_tweet_types_month_year

df_bill_tweet_types_month_year %>%  
  ggplot(aes(month_year, n, fill = tweet_type, group = tweet_type)) +
  geom_area(position = "fill") +
  scale_fill_viridis(name = "Tweet type", discrete = TRUE) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  labs(title = "Types of @BillPeduto tweets",
       x = "",
       y = "Percentage of tweets",
       caption = caption)
  
#df_bill %>% 
#  count(date, is_quote) %>% 
#  ggplot(aes(date, n, color = is_quote)) +
#  geom_jitter(alpha = .5) +
#  geom_smooth() +
#  labs(x = "",
#       y = "Number of tweets") +
#  scale_color_discrete(name = "", labels = c("Not a quote tweet", "Quote tweet"))

#df_bill %>% 
#  count(date, is_retweet) %>% 
#  ggplot(aes(date, n, color = is_retweet)) +
#  geom_jitter(alpha = .5) +
#  geom_smooth() +
#  labs(x = "",
#       y = "Number of tweets") +
#  scale_color_discrete(name = "", labels = c("Not a retweet", "Retweet"))
  
df_bill %>% 
  ggplot(aes(hour)) +
  geom_freqpoly(bins = 20, size = 2) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  coord_cartesian(xlim = c(0, 23)) +
  labs(title = title,
       x = "Hour",
       y = "Number of tweets",
       caption = caption)

#df_bill %>% 
#  ggplot(aes(hour)) +
#  geom_density() +
#  labs(x = "Hour",
#       y = "Density of tweets")

df_bill %>% 
  ggplot(aes(wday, group = 1)) +
  #geom_freqpoly(stat = "count", size = 2) +
  geom_density(stat = "count", fill = "black") +
  labs(x = "",
       y = "Number of tweets") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(title = title,
       x = "", 
       y = "Number of tweets",
       caption = caption)

df_bill %>% 
  count(wday, hour) %>% 
  complete(wday, hour = 0:23) %>% 
  replace_na(list(n = 0)) %>% 
  ggplot(aes(wday, hour, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_y_reverse(expand = c(0,0),
                  breaks = seq(0, 24, by = 3)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_viridis(option = 3) +
  labs(x = "",
       y = "Hour") +
  guides(fill = guide_colorbar("Number of tweets"))
  


#location
df %>% 
  select(bbox_coords) %>% 
  filter(!is.na(bbox_coords))
df %>% 
  select(geo_coords) %>% 
  filter(!is.na(geo_coords))

df %>% 
  select(geo_coords) %>% 
  filter(!is.na(geo_coords)) %>% 
  separate(geo_coords, into = c("long", "lat"), sep = " ") %>% 
  ggplot(aes(long, lat)) +
  geom_point()


#when does the mayor relpy to tweets?
df_bill %>% 
  filter(!is.na(reply_to_screen_name), is_quote == FALSE, is_retweet == FALSE) %>% 
  count(wday, hour) %>% 
  complete(wday, hour = 0:23) %>% 
  replace_na(list(n = 0)) %>% 
  ggplot(aes(wday, hour, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_y_reverse(expand = c(0,0),
                  breaks = seq(0, 24, by = 3)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_viridis(option = 3) +
  labs(x = "",
       y = "Hour") +
  guides(fill = guide_colorbar("Number of tweets"))

df_bill %>% 
  filter(!is.na(reply_to_screen_name), is_quote == FALSE, is_retweet == FALSE) %>% 
  count(reply_to_screen_name, sort = TRUE)
