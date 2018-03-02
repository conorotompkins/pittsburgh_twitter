source("scripts/tidytext_functions.R")

set.seed(1234)

#tweets_bill <- get_timelines("BillPeduto", n = 3200)
df_bill <- read_csv("data/bill_peduto_tweets.tweets.csv")

bill_stop_words <- c("t.co", "https", "amp")

tweets_bill <- count_twitter_bigrams(df_bill, bill_stop_words)
tweets_bill

#bigram network chart
visualize_bigrams(tweets_bill, 3,
                  title = "@BillPeduto tweets",
                  subtitle = "Bigram network",
                  caption = "@conor_tompkins")

#using the function
bill_stopwords <- c("t.co", "https", "amp")

bill_words <- word_correlations(df_bill, 20, bill_stopwords)

visualize_word_correlations(bill_words, 
                            title = "@BillPeduto tweets",
                            subtitle = "Word correlation network",
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



