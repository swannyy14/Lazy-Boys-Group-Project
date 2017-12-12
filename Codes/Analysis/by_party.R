source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

library(tm)
library(SnowballC)
library(dplyr)
library(hunspell)
library(ggplot2)
selected_cols <- c('text', 'favoriteCount', 'created', 'screenName', 'retweetCount')

john_tweets_df <- readRDS("../../Twitter Mining/John/john_house_tweets.rds") %>% select(selected_cols)
eunsik_tweets_df <- readRDS("../../Twitter Mining/Eunsik/sen_tweets.rds") %>% select(selected_cols)

all_tweets_df <- rbind(john_tweets_df, eunsik_tweets_df)

party_name_twit <- readRDS("all_twitter.rds")

names(party_name_twit) <- c("State", "Party", "screenName")
party_name_twit$screenName <- gsub("@", "", party_name_twit$screenName)
party_name_twit$screenName <- tolower(party_name_twit$screenName)
all_tweets_df$screenName <- tolower(all_tweets_df$screenName)

#tweets with Party column
tweets_party<-full_join(all_tweets_df, party_name_twit, by = c("screenName"))

#Republican tweets
tweets_party_rep<-filter(tweets_party, Party == "R")

#Democratic tweets
tweets_party_dem<-filter(tweets_party, Party == "D")

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
famous_r<-which.max(react_rep_tweet$favoriteCount)
react_rep_tweet[famous_r,]$text
react_rep_tweet[famous_r,]$favoriteCount
react_rep_tweet[famous_r,]$State

#Most famous post by party
famous_d<-which.max(react_dem_tweet$favoriteCount)
react_dem_tweet[famous_d,]$text
react_dem_tweet[famous_d,]$favoriteCount
react_dem_tweet[famous_d,]$State

tx_tweets <- tweets_party[tweets_party$State=="Texas",]
tx_tweets_rep <-filter(tx_tweets, Party == "R")
tx_tweets_dem <-filter(tx_tweets, Party == "D")

react_tx_rep <- na.omit(tx_tweets_rep)
react_tx_dem <- na.omit(tx_tweets_dem)

nrow(react_tx_rep)/nrow(tx_tweets_rep)
nrow(react_tx_dem)/nrow(tx_tweets_dem)


#combine reacted tweets for both parties
react_tweet <- na.omit(tweets_party)
react_tweet_avgs <- react_tweet %>% group_by(State, Party) %>% summarise(fav_avg = mean(favoriteCount))

ggplot(react_tweet_avgs, aes(x = Party, y = log(fav_avg), fill = Party)) +
  geom_bar(stat = "identity") + facet_wrap(~State) + ggtitle("Log of Average of Favorites by Party") +
  scale_fill_manual(values=c('blue','red')) + ylab("Average Favorites")

# jpeg("log_avg_favorite.jpg")

