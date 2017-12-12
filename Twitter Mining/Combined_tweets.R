source.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source.dir)

#install.packages("tm")
#install.packages('SnowballC')
#install.packages("hunspell")
#install.packages("RColorBrewer")
#install.packages("wordcloud")
#install.packages("tidytext")

library(tm)
library(dplyr)
library(hunspell)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)
library(tidytext)

selected_cols <- c('text', 'favoriteCount', 'created', 'screenName', 'retweetCount')

john_tweets_df <- readRDS("./John/john_house_tweets.rds") %>% select(selected_cols)
eunsik_tweets_df <- readRDS("./Eunsik/sen_tweets.rds") %>% select(selected_cols)

all_tweets_df <- rbind(john_tweets_df, eunsik_tweets_df)

texas_keywords <- c("heart", "pray", "texas", "attack", "violence", "shooting",
                    "sutherland", "victim", "tragedies", "tragedy", "gun")

texas_tweets_df <- all_tweets_df[grep(paste(texas_keywords, collapse = "|"),
                                      all_tweets_df$text, ignore.case = TRUE),]
rownames(texas_tweets_df) <- NULL #make sure the row numbers are right
texas_tweets_df$screenName <- texas_tweets_df$screenName %>% tolower


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

# orig_mar <- par()$mar
# par(mar = c(2,1,2,1))
# wordcloud(new_texas_corpus, colors = brewer.pal(8, name = 'Set2'))
# png('all_wordcloud.png')
# par(mar = orig_mar)


#list of all congress members with party and twitter acc
twitter_party <- readRDS("../Codes/Analysis/all_twitter.rds") 
colnames(twitter_party) <- c("State", "Party", "screenName")
twitter_party$screenName <- twitter_party$screenName %>% gsub('@','',.) %>% tolower()

tweet_with_party <- left_join(texas_tweets_df, twitter_party, by = 'screenName') 
tweet_with_party$document <- 1:nrow(tweet_with_party)


#word cloud by party
new_texas_D <- split(new_texas, tweet_with_party$Party)[[1]]
new_texas_R <- split(new_texas, tweet_with_party$Party)[[2]]

# orig_mar <- par()$mar
# par(mfrow = c(1,2), mar = c(2,1,2,1))
# new_texas_D_corpus <- VCorpus(VectorSource(new_texas_D))
# new_texas_R_corpus <- VCorpus(VectorSource(new_texas_R))
# wordcloud(new_texas_D_corpus, colors = brewer.pal(5, name = 'Set2'))
# wordcloud(new_texas_R_corpus, colors = brewer.pal(5, name = 'Set2'))
# par(mar = orig_mar, mfrow = c(1,1))


#############start sentiment analysis
new_texas_dtm <- DocumentTermMatrix(new_texas_corpus)

#new data frame that contains words, which is labelled by the speaker and his/her party
tweets_tidy <- tidy(new_texas_dtm)
tweets_tidy$document <- as.integer(tweets_tidy$document)
tweets_tidy <- full_join(tweets_tidy, 
                               subset(tweet_with_party, select = c(document, screenName, Party, State)),
                               by = 'document')



#get sentiment ratings
#use afinn scoring
sentiment_rating_afinn <- get_sentiments('afinn')
colnames(sentiment_rating_afinn) <- c("term", "score")
tweets_afinn_score <- inner_join(tweets_tidy, sentiment_rating_afinn, by = 'term')
tweets_afscore_state <- tweets_afinn_score %>% group_by(State) %>% summarise(avg_score = mean(score))


sent1_plot1 <- ggplot(tweets_afinn_score, aes(x = Party, y = score, fill = Party)) + 
  stat_boxplot(geom = 'errorbar') + 
  geom_boxplot() +
  ggtitle("Score Distribution of\n sentiment by Party") + 
  stat_summary(fun.y = mean, colour = 'yellow', size = 2, geom='point') + 
  scale_fill_manual(values = c('steelblue4', 'firebrick2'))

sent1_plot2 <- ggplot(tweets_afscore_state, aes(x = State, y = avg_score, fill = State)) +
  geom_bar(stat = 'identity') +
  ggtitle("Score Distribution of sentiment by state") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 75)) +
  theme(legend.position="none")

grid.arrange(sent1_plot1, sent1_plot2, ncol = 2, widths = 1:2)

#jpeg("afinn score distributions.jpg")


#use bing scoring
nrc_rating <- get_sentiments('nrc')
nrc_rating <- nrc_rating %>% filter(sentiment %in% c('fear', 'negative', 'sadness', 'anger', 'surprise', 'disgust'))

colnames(nrc_rating) <- c("term", "sentiment")
tweets_nrc <- inner_join(tweets_tidy, nrc_rating, by = 'term')
tweets_nrc_party <- tweets_nrc %>% #count sentiment by party
  group_by(Party, sentiment) %>% 
  summarise(sentiment_count = n()) %>%
  mutate(sentiment_proportion = sentiment_count/sum(sentiment_count))

ggplot(tweets_nrc_party, aes(x = sentiment, y = sentiment_proportion, fill = Party)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c('steelblue4', 'firebrick2')) +
  ggtitle("Distribution of Sentiment by Party") +
  ylab("Proportion") +
  xlab("Sentiment")
