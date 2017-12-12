source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

library(tm)
library(SnowballC)
library(dplyr)
library(hunspell)
selected_cols <- c('text', 'favoriteCount', 'created', 'screenName', 'retweetCount')

john_tweets_df <- readRDS("./John/john_house_tweets.rds") %>% select(selected_cols)
eunsik_tweets_df <- readRDS("./Eunsik/sen_tweets.rds") %>% select(selected_cols)

all_tweets_df <- rbind(john_tweets_df, eunsik_tweets_df)

party_name_twit <- readRDS("../Codes/Analysis/all_twitter.rds")

names(party_name_twit)<-c("State", "Party", "screenName")
party_name_twit$screenName <- gsub("@", "", party_name_twit$screenName)
party_name_twit$screenName <- tolower(party_name_twit$screenName)
all_tweets_df$screenName <- tolower(all_tweets_df$screenName)

#tweets with Party column
tweets_party<-full_join(all_tweets_df, party_name_twit, by = c("screenName"))

#Republican tweets
tweets_party_rep<-filter(tweets_party, Party == "Republican")

#Democratic tweets
tweets_party_dem<-filter(tweets_party, Party == "Democrat")

#Reacted tweets
react_rep_tweet <- na.omit(tweets_party_rep)
react_dem_tweet <- na.omit(tweets_party_dem)

#reaction rates (who posted about it)
react_rate_rep <- nrow(react_rep_tweet)/nrow(tweets_party_rep)
react_rate_dem <- nrow(react_dem_tweet)/nrow(tweets_party_dem)


#Likes per post
like_per_post_rep <- sum(react_rep_tweet$favoriteCount)/nrow(react_rep_tweet)
like_per_post_dem <- sum(react_dem_tweet$favoriteCount)/nrow(react_dem_tweet)

#Most famous post by party
famous<-which.max(react_rep_tweet$favoriteCount)
react_rep_tweet[famous,]$text
react_rep_tweet[famous,]$favoriteCount
react_rep_tweet[famous,]$State

#Most famous post by party
famous<-which.max(react_dem_tweet$favoriteCount)
react_dem_tweet[famous,]$text
react_dem_tweet[famous,]$favoriteCount
react_dem_tweet[famous,]$State

tx_tweets <- tweets_party[tweets_party$State=="Texas",]
tx_tweets_rep <-filter(tx_tweets, Party == "Republican")
tx_tweets_dem <-filter(tx_tweets, Party == "Democrat")

react_tx_rep <- na.omit(tx_tweets_rep)
react_tx_dem <- na.omit(tx_tweets_dem)

nrow(react_tx_rep)/nrow(tx_tweets_rep)
nrow(react_tx_dem)/nrow(tx_tweets_dem)
