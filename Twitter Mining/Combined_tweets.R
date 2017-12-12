source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

#install.packages("tm")
#install.packages('SnowballC')
#install.packages("hunspell")
#install.packages("RColorBrewer")
#install.packages("wordcloud")

library(tm)
library(SnowballC)
library(dplyr)
library(hunspell)
library(wordcloud)
library(RColorBrewer)

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
  return_vec <- iconv(x, "latin1", "ASCII", sub = "") %>%
    gsub('https.+$', '', .) %>%       #remove links
    gsub('&amp', '', .) %>%           #remove &amp characters
    gsub('@[[:alnum:]]+', '', .) %>%  #remove twitter usernames
    gsub('#[[:alnum:]]+', '', .) %>%  #remove hashtags
    gsub("[[:punct:]]+", " ", .)      #remove punctuation
  return (return_vec)
}

change_trivial <- function(char_vec){ #make trivial changes to avoid unwanted outcomes
  return_vec <- gsub('texas', 'Texas', char_vec) %>%
    gsub(' w ', '', .)
  return (return_vec)
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

hunspell_correction <- function(char_vec){ #function that decides which word needs correction, and chooses first suggestion
  vec_split <- strsplit(char_vec, "[[:space:]]+")[[1]]
  need_correction <- which(hunspell_check(vec_split) == FALSE)
  for (i in need_correction){
    corrected <- hunspell_suggest(as.character(vec_split[i]))[[1]]
    if (length(corrected) != 0){
      vec_split[i] <- corrected[1]
    }
  }
  return (paste(vec_split, collapse = ' '))
}

##maybe a function that corrects spelling errors first

complete_stripWhiteSpace <- function(x){ #completely strip all whitespaces at the end of the character vectors
  return_x <- gsub('^[[:space:]]+', '', x) %>% 
    gsub('[[:space:]]+$', '', .) %>%
    gsub('[\n\t]', '', .)
  return (return_x)
}

new_texas <- texas_tweets_df$text %>%
  clean_tweet %>% #remove non graphic and unncessary characters
  tolower %>%
  stripWhitespace %>%
  removeNumbers %>%
  removeWords(words = stopwords()) %>%
  complete_stripWhiteSpace %>%
  sapply(find_stem, USE.NAMES = FALSE) %>%
  change_trivial %>%
  sapply(hunspell_correction, USE.NAMES = FALSE)

new_texas_corpus <- VCorpus(VectorSource(new_texas)) #convert tweets to a corpus
inspect(new_texas_corpus)

as.character(new_texas_corpus[[2]])

new_texas_dtm <- DocumentTermMatrix(new_texas_corpus)

class(new_texas_dtm)

wordcloud(new_texas_corpus)
