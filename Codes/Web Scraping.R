#try scrping data: 
#install.packages('rvest')
library(rvest)
library(dplyr)
library(stringi)
library(twitteR)

source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

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

#from the names column, rid of all the unnecessary characters, spaces, and symbols
house_twit_df$Representative <- house_twit_df$Representative %>%
  gsub('representative', '', ., ignore.case = TRUE) %>%
  gsub('delegate', '', ., ignore.case = TRUE) %>%
  gsub('^ ', '',.) %>%
  gsub('\\n',' ',.)

house_twit_df$Representative <- stri_trans_general(house_twit_df$Representative, "Latin-ASCII")

#this data frame contains STATE, HOUSE OF REP NAME, TWITTER HANDLE
head(house_twit_df)

# #Now, try to extract the House of Rep's Political Party
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
colnames(house_party_df)[2] <- "Representative"
house_party_df$Representative <- as.character(house_party_df$Representative)
house_party_df$Representative[c(443,444,445,446,447,448)] <- c("Amata Coleman Radewagen", "Eleanor Holmes Norton",
                                                               "Madeleine Bordallo", "Gregorio Sablan", "Jenniffer Gonzalez",
                                                               "Stacey Plaskett")

#get rid of the "vacant" row
house_party_df <- house_party_df[grep(' \\([RDI]\\)', house_party_df$Representative),]

house_party_df$Party <- regexpr(' \\([RDI]\\)', house_party_df$Representative) %>% 
  regmatches(house_party_df$Representative, .) %>% gsub(' ', '', .)

#function that converts (R), (D), (I) to according political parties
toParty <- function(house_df){
  if (house_df == "(D)"){
    house_df <- "Democrat"
  } else if (house_df == "(R)"){
    house_df <- "Republican"
  } else {
    house_df <- "Independent"
  }
  return (house_df)
}

house_party_df$Party <- lapply(house_party_df$Party, toParty)

#rid of special accented characters
house_party_df$Representative <- stri_trans_general(house_party_df$Representative, "Latin-ASCII")


house_party_df$Representative <- 
  house_party_df$Representative %>%
  gsub(' \\([RDI]\\)(.)+', '', .) %>%
  gsub('[\\(\\)RDI .]+$', '', .) %>%
  gsub('at-large', '', ., ignore.case = TRUE) %>%
  gsub('^[^[:alpha:]]+','',.) %>%
  gsub('[.\']', '', .) %>%
  gsub('^[:space:]+', '', .) %>%
  gsub('[:space:]+$', '', .) %>%
  gsub(' jr$', '', ., ignore.case = TRUE)

#########################START CLEANING DF#########################

#Unfactorize Column
house_party_df<-data.frame(lapply(house_party_df,as.character),stringsAsFactors = FALSE)

#Replace State name with abbreviation
for(i in seq_along(state.name)){
  house_party_df[house_party_df[, 1] == state.name[i], 1] = state.abb[i]
}

#Factorize State
house_party_df$State<-as.factor(house_party_df$State)
house_twit_df$State<-as.factor(house_twit_df$State)

#Party DF name parsing
party_full_name <- house_party_df$Representative
party_full_name<-sub("'", "", party_full_name)
party_first <- sub("[[:space:]].*", "", party_full_name)
party_middle_last <- sub("^[[:alpha:]]*[[:space:]]", "", party_full_name)
party_middle <- sub("[[:alpha:]]+$", "", party_middle_last)
party_last <- sub("^[[:alpha:]]+[[:space:]]", "", party_middle_last)
party_last <- sub("^[[:alpha:]]+[[:punct:]]", "", party_last)

#Twit DF name parsing
twit_full_name <- house_twit_df$Representative
twit_full_name<-sub("'", "", twit_full_name)
twit_first <- sub("[[:space:]].*", "", twit_full_name)
twit_middle_last <- sub("^[[:alpha:]]*[[:space:]]", "", twit_full_name)
twit_middle <- sub("[[:alpha:]]+$", "", twit_middle_last)
twit_last <- sub("^[[:alpha:]]+[[:space:]]", "", twit_middle_last)
twit_last <- sub("^[[:alpha:]]+[[:punct:]]", "", twit_last)

#Divide Name Column
house_party_df <- data.frame(State=house_party_df$State,
                             First = party_first,
                             Last = party_last,
                             Party = house_party_df$Party,
                             stringsAsFactors = FALSE)
house_twit_df <- data.frame(State=house_twit_df$State,
                            First = twit_first,
                            Last = twit_last,
                            Twitter.Handle = house_twit_df$Twitter.Handle,
                            stringsAsFactors = FALSE)

#FULL JOIN
house_party_tweet <- dplyr::full_join(house_party_df, house_twit_df, by = c("State","Last"))

#Arrange to look for inaccurate name
house_party_tweet_arr <- arrange(house_party_tweet, State, Last, First.x, First.y, Twitter.Handle)

#NA.omit
house_party_tweet_arr <- na.omit(house_party_tweet_arr)
rownames(house_party_tweet_arr) <- NULL

#1. remove duplicate twitter accounts (gotta remove the right rows)
table(house_party_tweet_arr$Twitter.Handle)[which(table(house_party_tweet_arr$Twitter.Handle) > 1)]
delete_tweet_row <- c(85, 267, 82, 272, 266, 195, 196, 69)
house_party_tweet_arr <- house_party_tweet_arr[-delete_tweet_row,]

house_party_tweet_arr$Twitter.Handle[delete_tweet_row]

#2. 
not_included <- house_twit_df$Twitter.Handle[
  which(!(house_twit_df$Twitter.Handle %in% house_party_tweet_arr$Twitter.Handle))]
length(not_included)
not_included_members <- data.frame(
  house_twit_df[grepl(paste(not_included, collapse = "|") , house_twit_df$Twitter.Handle),],
  party = rep(NA, length(not_included)))
rownames(not_included_members) <- NULL
not_included_members$party <- 
  c('R', 'R', 'R', 'R', 'D', 'R', 'R',
    'R', 'D', 'D', 'D', 'D', 'D', 'D',
    'D', 'D', 'R', 'R', 'D', 'R', 'D',
    'D', 'R', 'D', 'D', 'R', 'D', 'D', 
    'D', 'D', 'D', 'D', 'D', 'D', 'R',
    'D', 'D', 'R', 'R', 'D', 'R', 'D',
    'R', 'R', 'R', 'D', 'R', 'R', 'D', 
    'D', 'D', 'R', 'D', 'R', 'D', 'R', 
    'R', 'R', 'R', 'R', 'R', 'R', 'R', 
    'R', 'R', 'D', 'D', 'R', 'D', 'D',
    'D', 'R', 'D', 'R', 'D', 'D', 'R',
    'R', 'R', 'D', 'R', 'D', 'R', 'D',
    'D', 'D', 'R', 'D', 'D', 'R', 'D',
    'D', 'D', 'D', 'D', 'R', 'R', 'R',
    'R', 'R', 'R', 'R', 'R', 'R', 'D',
    'D', 'R', 'D', 'D', 'D', 'R', 'R',
    'R', 'R', 'R', 'R', 'D', 'D', 'R',
    'R', 'R', 'R', 'R', 'R', 'R', 'R',
    'D', 'D', 'D', 'R', 'R', 'D', 'R')

