library(twitteR, lib.loc = "//ad.uillinois.edu/engr/instructional/jaewonc3/Downloads/STAT385 Statistical Programming Method/jaewon_library")
library(devtools, lib.loc = "//ad.uillinois.edu/engr/instructional/jaewonc3/Downloads/STAT385 Statistical Programming Method/jaewon_library")
library(ROAuth, lib.loc = "//ad.uillinois.edu/engr/instructional/jaewonc3/Downloads/STAT385 Statistical Programming Method/jaewon_library")
library(openssl, lib.loc = "//ad.uillinois.edu/engr/instructional/jaewonc3/Downloads/STAT385 Statistical Programming Method/jaewon_library")
library(httpuv, lib.loc = "//ad.uillinois.edu/engr/instructional/jaewonc3/Downloads/STAT385 Statistical Programming Method/jaewon_library")
library(base64enc, lib.loc = "//ad.uillinois.edu/engr/instructional/jaewonc3/Downloads/STAT385 Statistical Programming Method/jaewon_library")
library(RCurl, lib.loc = "//ad.uillinois.edu/engr/instructional/jaewonc3/Downloads/STAT385 Statistical Programming Method/jaewon_library")
library(curl, lib.loc = "//ad.uillinois.edu/engr/instructional/jaewonc3/Downloads/STAT385 Statistical Programming Method/jaewon_library")

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



prayfortexas_4_5 <- searchTwitter("prayfortexas", since = '2017-11-04 ', until = '2017-11-05', n=3200)
prayfortexas_4_5 <- twListToDF(prayfortexas_4_5)

prayfortexas_5_6 <- searchTwitter("prayfortexas", since = '2017-11-05 ', until = '2017-11-06 ', n=32000)
prayfortexas_5_6 <- twListToDF(prayfortexas_5_6)

prayfortexas_6_7 <- searchTwitter("prayfortexas", since = '2017-11-06 ', until = '2017-11-07 ', n=32000)
prayfortexas_6_7 <- twListToDF(prayfortexas_6_7)

prayfortexas_7_8 <- searchTwitter("prayfortexas", since = '2017-11-06 ', until = '2017-11-07 ', n=32000)
prayfortexas_7_8 <- twListToDF(prayfortexas_7_8)


guncontrolnow_4_5 <- searchTwitter("guncontrolnow", since = '2017-11-04 ', until = '2017-11-05', n=32000)
guncontrolnow_4_5 <- twListToDF(guncontrolnow_4_5)



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

