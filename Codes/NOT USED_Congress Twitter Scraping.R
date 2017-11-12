#attempted to obtain twitter acc of each congress members and their political parties, but might be uselss;
#kept just in case

#house of representatives
#might be uselss
# congress_url <- 'https://resist.blog/us-house-representatives-twitter-list/'
# congress_web <- read_html(congress_url)
# 
# congress_url <- html_nodes(congress_web, 'td')
# 
# congress_messy <- html_text(congress_url)
# congress_messy <- congress_messy[-which(congress_messy == '')]
# 
# #extract the state names, and associate each congress member by state
# state_index <- grep('^(north|south|rhode|new|west)?\\s?\\w+$', congress_messy, ignore.case = TRUE)
# state_iteration <- state_index[-1] - state_index[-length(state_index)] - 1 #number of times each state should be repeated
# state_iteration <- c(state_iteration, length(congress_messy) - state_index[length(state_index)])
# state_col <- unname(unlist(mapply(rep, congress_messy[state_index], state_iteration))) #state column
# 
# #start extracting names, twitter account, house or senate
# congress_tweets_string <- congress_messy[-state_index]
# congress_tweets_string <- gsub('\\n',' ',congress_tweets_string)
# 
# # testt <- grep('samuel', congress_tweets_string, ignore.case = TRUE, value = TRUE)
# # 
# # hello <- regexpr('(House|Senate)?\\s?@\\w+', testt)
# # 
# # substr(testt,1,hello-1)
# 
# congress_reg <- regexpr('(House|Senate)?\\s?@\\w+', congress_tweets_string)
# #congress_tweets_string[214]
# #names of the members
# 
# 
# strip_whitespace <- function(x){ #function that removes whitespace from the edge
#   rm1 <- gsub('^\\s+','',x)
#   rm2 <- gsub('\\s+$','',rm1)
#   return(rm2)
# }
# member_names <- substr(congress_tweets_string, 1, congress_reg-1) %>%
#   gsub('Rep.','',.) %>% gsub('[.\'?!,]', ' ', .) %>% gsub(' (jr|sr) ', '', .,ignore.case = TRUE) %>% strip_whitespace() 
# member_info <- regmatches(congress_tweets_string, congress_reg) %>% strsplit('@')
# h_or_s <- do.call(rbind, member_info)[,1] %>% strip_whitespace()
# twitter_handle <- paste0('@',do.call(rbind, member_info)[,2])
# 
# #data frame for tidied data
# house_twitter_table <- data.frame(state_col, 'h_or_s' = h_or_s, Twitter.Handle = twitter_handle,
#                               Real.Name = member_names, stringsAsFactors = FALSE) %>%
#   filter(h_or_s != 'Senate')
# 
# #members that did not get classified in House or Senate (missing value)
# undef_twitter <- house_twitter_table$Twitter.Handle[house_twitter_table$h_or_s == '']
# sen_twitter <- sen_twitter_table$Twitter.Handle
# 
# #get rid of members that coincide with the senate table
# duplicate_member <- character(0)
# for (i in undef_twitter){
#   if (i %in% sen_twitter){
#     duplicate_member <- c(duplicate_member, i)
#   }
# }
# 
# #simplify names to first and last name
# #IMPORTANT: the function doesn't work as intended. (need to strip the jr.sr parts)
# 
# first_and_last <- function(full_name){
#   first_name <- regmatches(full_name, regexpr('^\\w+', full_name))
#   last_name <- regmatches(full_name, regexpr('\\w+$', full_name))
#   return(paste(first_name, last_name))
# }
# # 
# # namesss <- house_twitter_table$Real.Name[c(72,178,320,420)]
# # 
# # first_and_last(namesss)
# 
# heremaybe <- house_twitter_table %>% filter(!(Twitter.Handle %in% duplicate_member)) %>% 
#   mutate(h_or_s = 'House', Real.Name = first_and_last(Real.Name))
# 
# comparetwo <- data.frame(heremaybe$Real.Name, house_twitter_table$Real.Name)
# 
# house_twitter_table <- house_twitter_table %>% filter(!(Twitter.Handle %in% duplicate_member)) %>% 
#   mutate(h_or_s = 'House', Real.Name = first_and_last(Real.Name))
# 
# #get rid of special characters (for some reason if I put it in a function it returns wacky characters)
# house_twitter_table$Real.Name <- stri_trans_general(house_twitter_table$Real.Name, "Latin-ASCII")
# 
# #associate each house member with a party