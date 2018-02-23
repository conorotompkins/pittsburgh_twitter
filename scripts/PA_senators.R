library(tidyverse)
library(rtweet)
library(lubridate)
library(tidytext)
library(scales)

theme_set(theme_bw(base_size = 18))

tweets_casey <- get_timelines("SenBobCasey", n = 3200) %>% 
  mutate(senator = "Casey")

tweets_toomey <- get_timelines("SenToomey", n = 3200) %>% 
  mutate(senator = "Toomey")

tweets <- bind_rows(tweets_casey, tweets_toomey)

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  select(senator, status_id, text) %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

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
  arrange(Casey, Toomey)
frequency

ggplot(frequency, aes(Casey, Toomey)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, senator) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(senator, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Casey / Toomey)) %>%
  arrange(desc(logratio))

word_ratios %>% 
  arrange(desc(abs(logratio)))

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Casey/Toomey)") +
  scale_fill_discrete(name = "", labels = c("Casey", "Toomey"))

