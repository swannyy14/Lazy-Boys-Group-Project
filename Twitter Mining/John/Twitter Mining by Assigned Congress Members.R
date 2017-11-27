#READ THE ATTACHED TEXT FILE
#Mining Twitter Data

source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

library(twitteR)
library(dplyr)

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
  sen_tweets[[i]] <- userTimeline(i, n = 150)
}

sen_tweets_df <- do.call(rbind.data.frame, lapply(sen_tweets,twListToDF))
rownames(sen_tweets_df) <- NULL
saveRDS(sen_tweets, 'sen_tweets.rds')


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

##John
#house_index <- (2*house_index_break+1):(length(house_twit_df$Twitter.Handle))

# extract tweets for the house members
house_twitter <- house_twit_df$Twitter.Handle %>% gsub('@','',.)

get_tweets <- function(twitterAccs, n = 150, delete_nonExist = TRUE, delete_empty = TRUE){
  #function that retrieves tweets from twitter accounts, and if the provided twitter account
  #if delete_nonExist is TRUE, then delete non-existent accounts from the returned list
  #if delete_empty is TRUE, then delete empty twitter accounts
  require(twitteR)
  tweets_list <- list()
  for (acc in twitterAccs){
    tweets_list[[twitterAccs[acc]]] <-
      tryCatch({userTimeline(twitterAccs[acc], n)},
               error = function(a){ return (NA) })
  }
  if (delete_nonExist){
    tweets_list <- tweets_list[which(!is.na(tweets_list))]
  }
  if (delete_empty){
    tweets_list <- tweets_list[which(sapply(house_tweets, length) == 0)]
  }
  return(tweets_list)
}

house_tweets <- get_tweets(house_twitter)

# house_tweets <- house_tweets[which(!(is.na(house_tweets) | sapply(house_tweets, length) == 0))] 

house_tweets_df <- lapply(house_tweets, twListToDF) %>% do.call(rbind,.)

rownames(house_tweets_df) <- NULL

## MAKE SURE TO UNCOMMENT THE CORRECT LINE TO SAVE THE FILE

# saveRDS(house_tweets_df, 'neel_house_tweets.rds') #NEEL
# saveRDS(house_tweets_df, 'jaewon_house_tweets.rds') #JAEWON
saveRDS(house_tweets_df, 'john_house_tweets.rds') #JOHN