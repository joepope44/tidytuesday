## load rtweet
library(rtweet)

## store api keys (these are fake example values; replace with your own keys)
api_key <- "VFnLyo6yzC5yyGIXPIgSrFsGO"
api_secret_key <- "LXMKJYNOpjlIUdw1rH6Xv2aW767v3SfJo7TfZ87uoilX7J4kIS"

## authenticate via web browser
token <- create_token(
  app = "jwp_tweets",
  consumer_key = api_key,
  consumer_secret = api_secret_key)