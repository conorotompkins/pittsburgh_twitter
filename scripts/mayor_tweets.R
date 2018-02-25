source("scripts/tidytext_functions.R")

#tweets_bill <- get_timelines("BillPeduto", n = 3200)
df_bill <- read_csv("data/bill_peduto_tweets.tweets.csv")

bill_stop_words <- c("t.co", "https", "amp")
bill_replacer <- c("'s$")

tweets_bill <- count_bigrams(df_bill, bill_stop_words, bill_replacer)
tweets_bill

visualize_bigrams(tweets_bill, 3,
                  title = "@BillPeduto tweets",
                  subtitle = "Bigram network",
                  caption = "@conor_tompkins")
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



