source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

#install.packages("tm")
#install.packages('SnowballC')
#install.packages("hunspell")
library(tm)
library(SnowballC)
library(dplyr)
library(hunspell)
selected_cols <- c('text', 'favoriteCount', 'created', 'screenName', 'retweetCount')

john_tweets_df <- readRDS("./John/john_house_tweets.rds") %>% select(selected_cols)
eunsik_tweets_df <- readRDS("./Eunsik/sen_tweets.rds") %>% select(selected_cols)

all_tweets_df <- rbind(john_tweets_df, eunsik_tweets_df)

texas_keywords <- c("heart", "pray", "texas", "attack", "violence", "shooting",
                    "sutherland", "victim", "tragedies", "tragedy", "gun")

texas_tweets_df <- all_tweets_df[grep(paste(texas_keywords, collapse = "|"),
                                      all_tweets_df$text, ignore.case = TRUE),]
rownames(texas_tweets_df) <- NULL #make sure the row numbers are right

clean_tweet <- function(x){ #function to remove non graphic characters, link, hashtag, username
  require(dplyr)
  iconv(x, "latin1", "ASCII", sub = "") %>%
    gsub('https.+$', '', .) %>%       #remove links
    gsub('&amp', '', .) %>%           #remove &amp characters
    gsub('@[[:alnum:]]+', '', .) %>%  #remove twitter usernames
    gsub('#[[:alnum:]]+', '', .) %>%  #remove hashtags
    gsub("[[:punct:]]+", " ", .)      #remove punctuation
}

find_stem <- function(char_vec){ #function that finds stem of the words
  vec_split <- strsplit(char_vec, "[[:space:]]+")[[1]]
  for (i in 1:length(vec_split)){
    hunspell_word <- hunspell_stem(vec_split[i])[[1]]
    if (length(hunspell_word) != 0){
      vec_split[i] <- hunspell_word[1]
    }
  }
  return(paste(vec_split, collapse = ' '))
}

##maybe a function that corrects spelling errors first

complete_stripWhiteSpace <- function(x){ #completely strip all whitespaces at the end of the character vectors
  return_x <- gsub('^[[:space:]]+', '', x) %>% gsub('[[:space:]]+$', '', .)
  return (return_x)
}

texas_tweets_df$text <- sapply(texas_tweets_df$text, clean_tweet) #clean up tweets

texas_corpus <- VCorpus(VectorSource(texas_tweets_df$text)) #convert tweets to a corpus
corpus.copy <- texas_corpus

new_texas <- texas_corpus %>% tm_map(tolower) %>% #all lower case
  tm_map(removeNumbers) %>% #remove numbers
  tm_map(replacePunct) %>% #replace punctuation with spaces
  tm_map(removeWords, stopwords()) %>% #find stem words
  tm_map(find_stem) %>% 
  tm_map(complete_stripWhiteSpace)

inspect(new_texas)