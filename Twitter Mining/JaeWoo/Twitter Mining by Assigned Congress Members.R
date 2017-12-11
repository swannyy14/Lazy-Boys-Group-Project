#READ THE ATTACHED TEXT FILE
#Mining Twitter Data

source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

library(twitteR)
library(dplyr)
library(stringi)

api_key <- 'PUT YOUR API KEY HERE'
api_secret <- 'PUT YOUR API SECRET HERE'
access_token <- 'PUT YOUR ACCESS TOKEN HERE'
access_token_secret <- 'PUT YOUR ACCESS TOKEN SECRET HERE'
setup_twitter_oauth(consumer_key = api_key, consumer_secret = api_secret)

############################## FOR EUNSIK ##############################
##SENATE
sen_twitter_table <- readRDS('sen_twitter_table.rds')

#extract tweets for the senators
sen_twitter <- sen_twitter_table$Twitter.Handle %>% gsub('@','',.)
sen_tweets <- list()

for (i in sen_twitter){
  sen_tweets[[i]] <- userTimeline(i, n = 100)
}

pickDate <- function(twitter_df, start_time = "2017-11-05 06:00:00 UTC", end_time = "2017-11-07 11:59:59 UTC"){
  times <- strptime(twitter_df$created, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')
  twitter_df[which((times >= start_time) & (times <= end_time)),]
}

sen_tweets_df <- lapply(sen_tweets,
                        function(x) pickDate(twListToDF(x))) %>% do.call(rbind,.)


rownames(sen_tweets_df) <- NULL
saveRDS(sen_tweets_df, 'sen_tweets.rds')
check_df <- readRDS('sen_tweets.rds')
head(check_df)

grep("shooting", check_df$text, value=TRUE,ignore.case = TRUE)

############################## FOR NEEL, JAEWON, JOHN ##############################
##HOUSE OF REP

#read the file that contains the twitter account for house of representatives, put it into a data.frame format
#some members have no twitter account. So we will get rid of them.
house_twit_df <- read.csv('congress_twitter_acc.csv', header = TRUE, 
                          stringsAsFactors = FALSE, skip = 2, encoding = 'UTF-8')
house_twit_df$Representative <- stri_trans_general(house_twit_df$Representative, "Latin-ASCII")
no_twit_acc <- c(1:nrow(house_twit_df))[-grep('@',house_twit_df$Twitter.Handle)]
house_twit_df[no_twit_acc,] #see who has the twitter account, but written in the wrong format
house_twit_df$Representative[337] <- paste(house_twit_df$Representative[337], 'Cartwright')
house_twit_df$Twitter.Handle[360] <- paste0('@', house_twit_df$Twitter.Handle[360])
house_twit_df$Twitter.Handle[422] <- paste0('@', house_twit_df$Twitter.Handle[422])

#now get rid of members with no actual twitter account and blank observations
no_twit_acc <- c(1:nrow(house_twit_df))[-grep('@',house_twit_df$Twitter.Handle)]
house_twit_df <- house_twit_df[-no_twit_acc,]

house_twit_df$Representative <- gsub('representative', '', house_twit_df$Representative, ignore.case = TRUE)
house_twit_df$Representative <- gsub('delegate', '', house_twit_df$Representative, ignore.case = TRUE)

house_index_break <- length(house_twit_df$Twitter.Handle) %/% 3

##Neel
#house_index <- 1:house_index_break

##Jaewon
#house_index <- (house_index_break+1):(2*house_index_break)

# #John
#house_index <- (2*house_index_break+1):(length(house_twit_df$Twitter.Handle))

# extract tweets for the house members
house_twitter <- house_twit_df$Twitter.Handle %>% gsub('@','',.)

get_tweets <- function(twitterAccs, n = 250, delete_nonExist = TRUE, delete_empty = TRUE){
  #function that retrieves tweets from twitter accounts, and if the provided twitter account
  #if delete_nonExist is TRUE, then delete non-existent accounts from the returned list
  #if delete_empty is TRUE, then delete empty twitter accounts
  require(twitteR)
  tweets_list <- list()
  for (acc in twitterAccs){
    tweets_list[[acc]] <-
      tryCatch({userTimeline(acc, n)},
               error = function(a){ return (NA) })
  }
  if (delete_nonExist){
    tweets_list <- tweets_list[which(!is.na(tweets_list))]
  }
  if (delete_empty){
    tweets_list <- tweets_list[which(sapply(tweets_list, length) != 0)]
  }
  return(tweets_list)
}

house_tweets <- get_tweets(house_twitter, n = 100)
# saveRDS(house_tweets_df, 'neel_house_tweets_list.rds') #NEEL
# saveRDS(house_tweets_df, 'jaewon_house_tweets_list.rds') #JAEWON
# saveRDS(house_tweets, 'john_house_tweets_list.rds') #JOHN


#UCT is ahead of CST by 6 hours
#Texas shooting happened on Nov 5th, 11:20a.m. CST
#pick observations whose rows are in between Nov 5th to No 7th
pickDate <- function(twitter_df, start_time = "2017-11-05 06:00:00 UTC", end_time = "2017-11-07 00:00:00 UTC"){
  times <- strptime(twitter_df$created, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')
  twitter_df[which((times >= start_time) & (times <= end_time)),]
}

house_tweets_df <- lapply(house_tweets,
                          function(x) pickDate(twListToDF(x))) %>% do.call(rbind,.)

#IGNORE#
# house_tweets_df <- data.frame()
# for (i in 1:length(house_tweets)){
#   temp_house_tweets <- twListToDF(house_tweets[[i]])
#   temp_house_tweets <- pickDate(temp_house_tweets)
#   house_tweets_df <- rbind(house_tweets_df, temp_house_tweets)
# }

rownames(house_tweets_df) <- NULL

## MAKE SURE TO UNCOMMENT THE CORRECT LINE TO SAVE THE FILE
# saveRDS(house_tweets_df, 'neel_house_tweets.rds') #NEEL
# saveRDS(house_tweets_df, 'jaewon_house_tweets.rds') #JAEWON
# saveRDS(house_tweets_df, 'john_house_tweets.rds') #JOHN