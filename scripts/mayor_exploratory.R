library(tidyverse)
library(lubridate)
library(viridis)

theme_set(theme_bw(base_family = 18))

df <- read_csv("data/bill_peduto_tweets.tweets.csv")

df %>% 
  select(status_id, text, created_at, source, is_quote, is_retweet, display_text_width, favorite_count) %>% 
  mutate(created_at = with_tz(created_at, "US/Eastern"),
         date = ymd(str_sub(created_at, 1, 10)),
         hour = hour(created_at),
         wday = wday(date, label = TRUE)) -> df_bill
df_bill

df_bill %>% 
  count(date) %>%
  ggplot(aes(date, n)) +
  geom_point() +
  geom_smooth()

df_bill %>% 
  count(date, is_quote) %>% 
  ggplot(aes(date, n, color = is_quote)) +
  geom_point(alpha = .2) +
  geom_smooth()

df_bill %>% 
  count(date, is_retweet) %>% 
  ggplot(aes(date, n, color = is_retweet)) +
  geom_point(alpha = .2) +
  geom_smooth()
  
df_bill %>% 
  ggplot(aes(hour)) +
  geom_freqpoly(bins = 20) +
  coord_cartesian(xlim = c(0, 23))

df_bill %>% 
  ggplot(aes(hour)) +
  geom_density()

df_bill %>% 
  ggplot(aes(wday)) +
  geom_bar()

df_bill %>% 
  count(wday, hour) %>% 
  complete(wday, hour) %>% 
  replace_na(list(n = 0)) %>% 
  ggplot(aes(wday, hour, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_y_reverse(expand = c(0,0),
                  breaks = seq(0, 24, by = 3)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_viridis(option = 3)
  


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
  