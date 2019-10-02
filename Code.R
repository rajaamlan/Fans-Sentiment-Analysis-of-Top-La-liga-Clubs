rm(list=ls())
library(NLP)
library(ROAuth)
library(twitteR)
library(syuzhet)
library(tm)
library(SnowballC)
library(stringi)
library(topicmodels)

consumer_key    = 'sz6x0nvL0ls9wacR64MZu23z4'
consumer_secret = 'ofeGnzduikcHX6iaQMqBCIJ666m6nXAQACIAXMJaFhmC6rjRmT'
access_token  = '854004678127910913-PUPfQYxIjpBWjXOgE25kys8kmDJdY0G'
access_secret = 'BC2TxbhKXkdkZ91DXofF7GX8p2JNfbpHqhshW1bwQkgxN'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets_rm <- searchTwitter("#realmadrid + #barcelona + #atletico + #sevilla + #valencia", n=500,lang = "en")
#tweets_barca <- searchTwitter("#fcbarcelona" , n=5000,lang = "en")
#tweets_atm <- searchTwitter("#atleticomadrid" , n=5000,lang = "en")
#tweets_s <- searchTwitter("#sevilla" , n=5000,lang = "en")
#tweets_v <- searchTwitter("#valencia" , n=5000,lang = "en")
#convert to dataframe
rmfc_tweet <-twListToDF(tweets_rm)
barca_tweet <-twListToDF(tweets_barca)
atm_tweet <-twListToDF(tweets_atm)
s_tweet <-twListToDF(tweets_s)
v_tweet <-twListToDF(tweets_v)
#preprocess data
rmfc_tweet <- rmfc_tweet$text
barca_tweet <-barca_tweet$text
atm_tweet <-atm_tweet$text
s_tweet <-s_tweet$text
v_tweet <-v_tweet$text
#converting all text to lower
rmfc_tweet <-tolower(rmfc_tweet)
barca_tweet <-tolower(barca_tweet)
atm_tweet <-tolower(atm_tweet)
s_tweet <-tolower(s_tweet)
v_tweet <-tolower(v_tweet)
#replace blank space
rmfc_tweet <-gsub("rt", "", rmfc_tweet)
barca_tweet <-gsub("rt", "", barca_tweet)
atm_tweet <-gsub("rt", "", atm_tweet)
s_tweet <-gsub("rt", "", s_tweet)
v_tweet <-gsub("rt", "", v_tweet)
#replace username
rmfc_tweet <- gsub("@\\w+", "", rmfc_tweet)
barca_tweet <- gsub("@\\w+", "", barca_tweet)
atm_tweet <- gsub("@\\w+", "", atm_tweet)
s_tweet <- gsub("@\\w+", "", s_tweet)
v_tweet <- gsub("@\\w+", "", v_tweet)
#remove punctuation
rmfc_tweet <- gsub("[[:punct:]]", "", rmfc_tweet)
barca_tweet <- gsub("[[:punct:]]", "", barca_tweet)
atm_tweet <- gsub("[[:punct:]]", "", atm_tweet)
s_tweet <- gsub("[[:punct:]]", "", s_tweet)
v_tweet <- gsub("[[:punct:]]", "", v_tweet)
#removing links
rmfc_tweet <- gsub("http\\w+", "", rmfc_tweet)
barca_tweet <- gsub("http\\w+", "", barca_tweet)
atm_tweet<- gsub("http\\w+", "", atm_tweet)
s_tweet <- gsub("http\\w+", "", s_tweet)
v_tweet <- gsub("http\\w+", "", v_tweet)
#removing tabs
rmfc_tweet <- gsub("[ |\t]{2,}", "", rmfc_tweet)
barca_tweet <- gsub("[ |\t]{2,}", "", barca_tweet)
atm_tweet <- gsub("[ |\t]{2,}", "", atm_tweet)
s_tweet<- gsub("[ |\t]{2,}", "", s_tweet)
v_tweet<- gsub("[ |\t]{2,}", "", v_tweet)
#removing blank spaces at te beginning
rmfc_tweet <- gsub("^ ", "", rmfc_tweet)
barca_tweet <- gsub("^ ", "", barca_tweet)
atm_tweet<- gsub("^ ", "", atm_tweet)
s_tweet <- gsub("^ ", "", s_tweet)
v_tweet <- gsub("^ ", "", v_tweet)

#removing blank spaces in the end
rmfc_tweet <- gsub(" $", "", rmfc_tweet)
barca_tweet <- gsub(" $", "", barca_tweet)
atm_tweet<- gsub(" $", "", atm_tweet)
s_tweet <- gsub(" $", "", s_tweet)
v_tweet <- gsub(" $", "", v_tweet)
#create corpus
rm_corpus=Corpus(VectorSource(rmfc_tweet))
barca_corpus=Corpus(VectorSource(barca_tweet))
atm_corpus=Corpus(VectorSource(atm_tweet))
s_corpus=Corpus(VectorSource(s_tweet))
v_corpus=Corpus(VectorSource(v_tweet))
#removing stop words
rm_corpus <- tm_map(rm_corpus, removeWords,stopwords(kind = 'en'))
barca_corpus <- tm_map(barca_corpus, removeWords,stopwords(kind = 'en'))
atm_corpus <- tm_map(atm_corpus, removeWords,stopwords(kind = 'en'))
s_corpus <- tm_map(s_corpus, removeWords,stopwords(kind = 'en'))
v_corpus <- tm_map(v_corpus, removeWords,stopwords(kind = 'en'))
#getting emotions
mysentiment_rm<-get_nrc_sentiment((rmfc_tweet))
mysentiment_barca<-get_nrc_sentiment((barca_tweet))
mysentiment_atm<-get_nrc_sentiment((atm_tweet))
mysentiment_s<-get_nrc_sentiment((s_tweet))
mysentiment_v<-get_nrc_sentiment((v_tweet))

#calculating total score for each sentiment
Sentimentscores_rm<-data.frame(colSums(mysentiment_rm[,]))
Sentimentscores_barca<-data.frame(colSums(mysentiment_barca[,]))
Sentimentscores_atm<-data.frame(colSums(mysentiment_atm[,]))
Sentimentscores_s<-data.frame(colSums(mysentiment_s[,]))
Sentimentscores_v<-data.frame(colSums(mysentiment_v[,]))

names(Sentimentscores_rm)<-"Score"
Sentimentscores_rm<-cbind("sentiment"=rownames(Sentimentscores_rm),Sentimentscores_rm)
rownames(Sentimentscores_rm)<-NULL

names(Sentimentscores_barca)<-"Score"
Sentimentscores_barca<-cbind("sentiment"=rownames(Sentimentscores_barca),Sentimentscores_barca)
rownames(Sentimentscores_barca)<-NULL

names(Sentimentscores_atm)<-"Score"
Sentimentscores_atm<-cbind("sentiment"=rownames(Sentimentscores_atm),Sentimentscores_atm)
rownames(Sentimentscores_atm)<-NULL

names(Sentimentscores_s)<-"Score"
Sentimentscores_s<-cbind("sentiment"=rownames(Sentimentscores_s),Sentimentscores_s)
rownames(Sentimentscores_s)<-NULL

names(Sentimentscores_v)<-"Score"
Sentimentscores_v<-cbind("sentiment"=rownames(Sentimentscores_v),Sentimentscores_v)
rownames(Sentimentscores_v)<-NULL

#plotting sentiments with scores
library(ggplot2)
ggplot(data=Sentimentscores_rm,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Real Madrid")

ggplot(data=Sentimentscores_barca,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Barcelona")


ggplot(data=Sentimentscores_atm,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Atlectico Madrid")


ggplot(data=Sentimentscores_s,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sevilla")

ggplot(data=Sentimentscores_v,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Valencia")
