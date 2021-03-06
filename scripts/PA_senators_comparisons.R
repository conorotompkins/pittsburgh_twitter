library(tidyverse)
library(tidytext)
library(lubridate)
library(rtweet)
library(scales)

theme_set(theme_bw(base_size = 18))
title <- "@SenBobCasey and @SenToomey tweets"
caption <- "@conor_tompkins"

#tweets_casey <- get_timelines("SenBobCasey", n = 3200) %>% 
#  mutate(senator = "Casey")

#save_as_csv(tweets_casey, "data/tweets_casey.csv")
tweets_casey <- read_csv("data/tweets_casey.tweets.csv") %>% 
  mutate(senator = "SenBobCasey")

#tweets_toomey <- get_timelines("SenToomey", n = 3200) %>% 
#  mutate(senator = "Toomey")

#save_as_csv(tweets_toomey, "data/tweets_toomey.csv")
tweets_toomey <- read_csv("data/tweets_toomey.tweets.csv") %>% 
  mutate(senator = "SenToomey")

tweets <- bind_rows(tweets_casey, tweets_toomey)

tweets %>% 
  count(senator)

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https|'s|'"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tweets %>% 
  select(senator, status_id, text, is_quote, is_retweet) %>% 
  filter(is_quote == FALSE, is_retweet == FALSE) %>% 
  mutate(text = str_replace_all(text, replace_reg, ""),
         senator = factor(senator, levels = c("SenToomey", "SenBobCasey"))) %>% 
  count(senator)

tidy_tweets <- tweets %>% 
  select(senator, status_id, text, is_quote, is_retweet) %>% 
  mutate(text = str_replace_all(text, replace_reg, ""),
         senator = factor(senator, levels = c("SenBobCasey", "SenToomey"))) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         !word %in% c("009f", "00a6", "f0"),
         str_detect(word, "[a-z]"))

tidy_tweets
sum(is.na(tidy_tweets$senator))
tidy_tweets %>% 
  filter(is.na(senator))

frequency <- tidy_tweets %>% 
  group_by(senator) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(senator) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)
frequency

frequency <- frequency %>% 
  select(senator, word, freq) %>% 
  spread(senator, freq) %>%
  arrange(desc(SenBobCasey), desc(SenToomey))
frequency

ggplot(frequency, aes(SenBobCasey, SenToomey)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  labs(title = title,
       x = "Used more by @SenBobCasey",
       y = "Used more by @SenToomey",
       caption = caption)

#test plot
ggplot(frequency, aes(SenBobCasey, SenToomey)) +
  stat_density2d(aes(fill = ..density..^.005), geom = "tile", contour = FALSE, n = 500) +
  geom_point(alpha = 0.1, shape = 20) +
  scale_fill_continuous(low = "white", high = "dodgerblue4") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal()

word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, senator) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(senator, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(SenBobCasey / SenToomey)) %>%
  arrange(desc(logratio))

word_ratios %>% 
  arrange(desc(abs(logratio)))

#need to switch colors around
word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(alpha = .8) +
  coord_flip() +
  labs(x = "",
       y = "Log odds ratio (Casey/Toomey)") +
  scale_fill_manual(name = "", 
                    values = c("blue", "red"),
                    breaks = c(FALSE, TRUE), 
                    labels = c("@SenBobCasey", "@SenToomey")) +
  theme(panel.grid.major.y = element_blank())
