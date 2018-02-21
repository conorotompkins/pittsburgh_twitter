
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
