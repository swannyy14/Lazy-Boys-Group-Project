#try scrping data: 
#install.packages('rvest')
library(rvest)
library(dplyr)
library(stringi)
library(twitteR)

##### No need to run this code again (have the object saved)
# # list of senators and their twitter accounts
# # sen_twitter_url <- 'https://www.birdsonganalytics.com/blog/2017/02/21/full-list-of-us-senators-on-twitter-2017/'
# # sen_web <- read_html(sen_twitter_url)
# # 
# # sen_twitter_table <- html_nodes(sen_web,'table') %>% html_table() %>% as.data.frame()
# # sen_twitter_table <- transform.data.frame(sen_twitter_table, Twitter.Handle = paste0('@', Twitter.Handle))
# # saveRDS(sen_twitter_table, 'sen_twitter_table.rds')
# 
sen_twitter_table <- readRDS('sen_twitter_table.rds')

#install.packages("pdftools")
#library(pdftools)
house_twit_df <- read.csv('congress_twitter_acc.csv', header = TRUE, 
                       stringsAsFactors = FALSE, skip = 2, encoding = 'UTF-8')
house_twit_df$Representative <- stri_trans_general(house_twit_df$Representative, "Latin-ASCII")
no_twit_acc <- c(1:nrow(house_twit_df))[-grep('@',house_twit_df$Twitter.Handle)]
house_twit_df[no_twit_acc,]
house_twit_df$Representative[337] <- paste(house_twit_df$Representative[337], 'Cartwright')
house_twit_df$Twitter.Handle[360] <- paste0('@', house_twit_df$Twitter.Handle[360])
house_twit_df$Twitter.Handle[422] <- paste0('@', house_twit_df$Twitter.Handle[422])

no_twit_acc <- c(1:nrow(house_twit_df))[-grep('@',house_twit_df$Twitter.Handle)]
house_twit_df <- house_twit_df[-no_twit_acc,]

repr <- gregexpr('^\\w+', house_twit_df$Representative)
unique(regmatches(house_twit_df$Representative,repr))

house_twit_df$Representative <- gsub('representative', '', house_twit_df$Representative, ignore.case = TRUE)
house_twit_df$Representative <- gsub('delegate', '', house_twit_df$Representative, ignore.case = TRUE)

#this data frame contains STATE, HOUSE OF REP NAME, TWITTER HANDLE
head(house_twit_df)

##Now, try to extract the House of Rep's Political Party
# house_party_url <- 'https://en.wikipedia.org/wiki/115th_United_States_Congress'
# house_party_url %>% read_html() %>% 
#   html_nodes('#mw-content-text > div > div:nth-child(60) > table') %>%
#   html_text() %>% strsplit('\\n') -> house_party
# 
# house_party <- house_party[[1]][-c(which(house_party[[1]] == ''),558,549,545,542,535,531,528,523,516)]
# 
# h_state_index <- grep('\\w+\\[edit\\]', house_party)
# house_by_party <- house_party[seq_along(house_party)[-h_state_index]]
# 
# house_states <- house_party[h_state_index] %>% gsub('\\[edit\\]','',., ignore.case = TRUE)
# house_state_col <- mapply(rep,house_states,c(h_state_index[2:length(h_state_index)],
#                                              length(house_party)+1) - h_state_index - 1) %>%
#   unlist() %>% unname()
# house_party_df <- data.frame(State = house_state_col, house_by_party)
# saveRDS(house_party_df, 'house_party_df.rds')

house_party_df <- readRDS('house_party_df.rds')

##extract tweets for the senators
# sen_twitter <- sen_twitter_table$Twitter.Handle %>% gsub('@','',.)
# sen_tweets <- list()
# 
# for (i in sen_twitter){
#   sen_tweets[[i]] <- userTimeline(i, n = 20)
# }
# 
# saveRDS(sen_tweets, 'sen_tweets.rds')


#start mining twitter data
library(twitteR)

api_key <- '2Sxgqqf0fpfFzjUCwoxfkQiMa'
api_secret <- 'RquO4sxeLBp8o9yZH9I5haeyckY9dfTJb7mX180JjRZKV1Z4iQ'
access_token <- '142134584-ULO3fBv7P96tPlnln8kclhA0oVSgIWKsktAUMZvQ'
access_token_secret <- 'wKbTK1icm333EbAZvUNJ3euxQ9Pw75Fnhea8y8lZHjTKO'
setup_twitter_oauth(consumer_key = api_key, consumer_secret = api_secret)

sen_tweets <- readRDS('sen_tweets.rds')
sum(lapply(sen_tweets,length))

str(sen_tweets)

## of tweets for the senator
do.call(sum,lapply(sen_tweets,length))


#extract tweets for the house members
house_twitter <- house_twit_df$Twitter.Handle %>% gsub('@','',.)
house_tweets <- list()

house_twitter1 <- house_twitter[1:10]

for (acc in house_twitter1){
  house_tweets[[acc]] <-
    tryCatch({userTimeline(acc,n = 20,since = '2017-11-05')}, error = function(a){return(NA)})
}
saveRDS(house_tweets, 'house_tweets.rds')

house_tweets <- readRDS('house_tweets.rds')
#get rid of empty twitter accounts
house_tweets <- house_tweets[which(!(is.na(house_tweets) | sapply(house_tweets, length) == 0))] 

house_tweets_df <- lapply(house_tweets, twListToDF) %>% do.call(rbind,.)


