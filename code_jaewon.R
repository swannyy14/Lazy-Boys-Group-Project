library(twitteR)
library(devtools)
library(ROAuth)
library(openssl)
library(httpuv)
library(base64enc)

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "./cacert.pem", package = "RCurl")))


#My twitter application OAuth info (https://dev.twitter.com/apps)
reqURL <- "https://api.twitter.com/oauth/request_token"        #The URL provided for retrieving request tokens
accessURL <- "https://api.twitter.com/oauth/access_token"      #The URL provided for retrieving acess tokens
authURL <- "https://api.twitter.com/oauth/authorize"           #The URL provided for authorization/verification purposes 
consumerKey <- "LY3wYeXrybGmNAG1BcR8G17z6"                     #웹에서 확인한 자신의 consumerKey를 입력
consumerSecret <- "zAjLbSs97oiyuhcToD3LV5Lh9OQyUsUgIXVeyMMHYTfq5NT5pB" #웹에서 확인한 자신의 consumerSecret을 입력
accessToken <- "917393984288448513-KCtszTWxvV1ZZk0sIGJM8oIpkOi729y"
accessSecret <- "adLVekMFnTlPGYXoXXpJhR4vi3nOQDyPw33HRAKZ7E8EH"

setup_twitter_oauth(consumerKey,consumerSecret)

#http://thinktostart.com/twitter-authentification-with-r/



searchTwitter("Go Illini")


test_tw <- searchTwitter("prayfortexas", since = '2017-11-05 00:00:00', until = '2017-11-05 12:00:00', n=3200)
test_tw <- searchTwitter("prayfortexas", since = '2017-11-04', until = '2017-11-05', n=3200)
test_tw <- twListToDF(test_tw)


extract.hashes = function(vec){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}

#https://www.r-bloggers.com/using-r-to-find-obamas-most-frequent-twitter-hashtags/



test_tw <- userTimeline("BarackObama", n = 3200)
test_tw <- twListToDF(test_tw)
vec1 <- test_tw$text

dat <- head(extract.hashes(vec1),50)
dat2 <- transform(dat, tag = reorder(tag, freq))


library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat = "identity", fill = "blue")
p = p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Obama team (@BarackObama)")
p

#UsefulWebsites______________________________________________________________________________________________

#http://rstatistics.net/extracting-tweets-with-r/

#__________________________________________________________________________________________________________
test_tw <- userTimeline("realDonaldTrump", n = 3200)
test_tw <- twListToDF(test_tw)
vec1 <- test_tw$text

dat <- head(extract.hashes(vec1),50)
dat2 <- transform(dat, tag = reorder(tag, freq))

p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat = "identity", fill = "blue")
p = p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Trump team (@realDonaldTrump)")
p


searchTwitter("hi", since='2017-10-01', until='2017-10-10')

