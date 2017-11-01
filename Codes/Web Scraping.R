#try scrping data: 
#install.packages('rvest')
library(rvest)
library(dplyr)
library(stringi)

#list of senators and their twitter accounts
sen_twitter_url <- 'https://www.birdsonganalytics.com/blog/2017/02/21/full-list-of-us-senators-on-twitter-2017/'
sen_web <- read_html(sen_twitter_url)
 
sen_twitter_table <- html_nodes(sen_web,'table') %>% html_table() %>% as.data.frame() %>% mutate(h_or_s = 'Senate')
sen_twitter_table <- transform.data.frame(sen_twitter_table, Twitter.Handle = paste0('@', Twitter.Handle))


#house of representatives
congress_url <- 'https://resist.blog/us-house-representatives-twitter-list/'
congress_web <- read_html(congress_url)

congress_url <- html_nodes(congress_web, 'td')

congress_messy <- html_text(congress_url)
congress_messy <- congress_messy[-which(congress_messy == '')]

#extract the state names, and associate each congress member by state
state_index <- grep('^(north|south|rhode|new|west)?\\s?\\w+$', congress_messy, ignore.case = TRUE)
state_iteration <- state_index[-1] - state_index[-length(state_index)] - 1 #number of times each state should be repeated
state_iteration <- c(state_iteration, length(congress_messy) - state_index[length(state_index)])
state_col <- unname(unlist(mapply(rep, congress_messy[state_index], state_iteration))) #state column

#start extracting names, twitter account, house or senate
congress_tweets_string <- congress_messy[-state_index]
congress_tweets_string <- gsub('\\n',' ',congress_tweets_string)

congress_reg <- regexpr('(House|Senate)?\\s?@\\w+', congress_tweets_string)

#names of the members

strip_whitespace <- function(x){ #function that removes whitespace from the edge
  rm1 <- gsub('^\\s+','',x)
  rm2 <- gsub('\\s+$','',rm1)
  return(rm2)
}
member_names <- substr(congress_tweets_string, 1, congress_reg-2)  %>%
  gsub('Rep.','',.) %>%  strip_whitespace() 
member_info <- regmatches(congress_tweets_string, congress_reg) %>% strsplit('@')
h_or_s <- do.call(rbind, member_info)[,1] %>% strip_whitespace()
twitter_handle <- paste0('@',do.call(rbind, member_info)[,2])

#data frame for tidied data
house_twitter_table <- data.frame(state_col, 'h_or_s' = h_or_s, Twitter.Handle = twitter_handle,
                              Real.Name = member_names, stringsAsFactors = FALSE) %>%
  filter(h_or_s != 'Senate')

#members that did not get classified in House or Senate (missing value)
undef_twitter <- house_twitter_table$Twitter.Handle[house_twitter_table$h_or_s == '']
sen_twitter <- sen_twitter_table$Twitter.Handle

#get rid of members that coincide with the senate table
duplicate_member <- character(0)
for (i in undef_twitter){
  if (i %in% sen_twitter){
    duplicate_member <- c(duplicate_member, i)
  }
}

#simplify names to first and last name
first_and_last <- function(full_name){
  first_name <- regmatches(full_name, regexpr('^\\w+', full_name))
  last_name <- regmatches(full_name, regexpr('\\w+$', full_name))
  return(paste(first_name, last_name))
}
# 
# namesss <- house_twitter_table$Real.Name[c(72,178,320,420)]
# 
# first_and_last(namesss)

heremaybe <- house_twitter_table %>% filter(!(Twitter.Handle %in% duplicate_member)) %>% 
  mutate(h_or_s = 'House', Real.Name = first_and_last(Real.Name))

comparetwo <- data.frame(heremaybe$Real.Name, house_twitter_table$Real.Name)

house_twitter_table <- house_twitter_table %>% filter(!(Twitter.Handle %in% duplicate_member)) %>% 
  mutate(h_or_s = 'House', Real.Name = first_and_last(Real.Name))

#get rid of special characters (for some reason if I put it in a function it returns wacky characters)
house_twitter_table$Real.Name <- stri_trans_general(house_twitter_table$Real.Name, "Latin-ASCII")

#associate each house member with a party
