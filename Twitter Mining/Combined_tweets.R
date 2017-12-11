source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

#install.packages("tm")
#install.packages('SnowballC')
library(tm)
library(SnowballC)
selected_cols <- c('text', 'favoriteCount', 'created', 'screenName', 'retweetCount')

john_tweets_df <- readRDS("./John/john_house_tweets.rds") %>% select(selected_cols)
eunsik_tweets_df <- readRDS("./Eunsik/sen_tweets.rds") %>% select(selected_cols)

all_tweets_df <- rbind(john_tweets_df, eunsik_tweets_df)

texas_keywords <- c("heart", "pray", "texas", "attack", "violence", "shooting",
                    "sutherland", "victim", "tragedies", "tragedy", "gun")

texas_tweets_df <- all_tweets_df[grep(paste(texas_keywords, collapse = "|"),
                                      all_tweets_df$text, ignore.case = TRUE),]

texas_tweets_df$text <- sapply(texas_tweets_df$text,
                               function(x) iconv(x, "latin1", "ASCII", sub="")) %>% #to avoid error while using tm
  gsub('https.+$', '', .) %>% #remove links
  gsub('@[[:alnum:]]+', '', .) %>% #remove twitter usernames
  gsub('#[[:alnum:]]+', '', .) #remove hashtags


rownames(texas_tweets_df) <- NULL

replacePunct <- function(x){ #function to replace punctuation with space
  return(gsub("[[:punct:]]+", ' ', x))
}

texas_corpus <- VCorpus(VectorSource(texas_tweets_df$text)) #convert tweets to a corpus
corpus.copy <- texas_corpus

new_texas <- texas_corpus %>% tm_map(content_transformer(tolower)) %>% #all lower case
  tm_map(removeNumbers) %>% #remove numbers
  tm_map(replacePunct) %>% #replace punctuation with spaces
  tm_map(removeWords, stopwords()) %>% #get rid of unnecessary stop words (and, or, etc)
  tm_map(stemDocument) %>% #reduce words to their roots
  tm_map(gsub, pattern = "[aeyuio]+$", replacement = "", x = .) %>%
  tm_map(stemCompletion, dictionary = corpus.copy) %>%
  tm_map(stripWhitespace)

as.character(new_texas[[1]])
wordStem("heavy")

stopwords()
tm_map(texas_corpus, content_transformer(tolower))
       
getTransformations()
