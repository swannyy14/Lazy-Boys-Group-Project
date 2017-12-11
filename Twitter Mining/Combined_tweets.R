source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)



john_tweets_df <- readRDS("./John/john_house_tweets.rds")
eunsik_tweets_df <- readRDS("./Eunsik/sen_tweets.rds")

which(gsub('@','',house_twit_df$Twitter.Handle) %in% unique(neel_tweets_df$screenName))
