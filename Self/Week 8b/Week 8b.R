library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

text<-readLines(file.choose())
docs<-Corpus(VectorSource(text))
inspect(docs)

toSpace<-content_transformer(function(x, pattern) gsub(pattern, " ",x))
docs<-tm_map(docs,toSpace,"/")
docs<-tm_map(docs,toSpace,"@")
docs<-tm_map(docs,toSpace,"\\|")

docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs, removeWords, stopwords("english"))
docs<-tm_map(docs, removePunctuation)
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,stemDocument)

dtm<-TermDocumentMatrix(docs)
dtm_m<-as.matrix(dtm)
dtm_v<-sort(rowSums(dtm_m), decreasing=TRUE)
dtm_d<-data.frame(word=names(dtm_v),freq=dtm_v)
head(dtm_d, 5)

barplot(dtm_d[1:5,]$freq, las=2, names.arg=dtm_d[1:5,]$word,
        col="lightgreen", main="Top 5 most frequent words",
        ylab="word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words=dtm_d$word, freq=dtm_d$freq, min.freq=5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8,"Dark2"))
#word association
findAssocs(dtm,terms=c("good","work","health"), corlimit=0.25)
findAssocs(dtm,terms=findFreqTerms(dtm, lowfreq=50),corlimit=0.25) #words at least 50 times

####SENTIMENT SCORE


##tak perlu pre-process. ambil dari txt balik
syuzhet_vector<-get_sentiment(text, method="syuzhet")
head(syuzhet_vector,10)
summary(syuzhet_vector)

##bing
bing_vector<-get_sentiment(text, method="bing")
head(bing_vector,10)
summary(bing_vector)

##affin
afinn_vector<-get_sentiment(text, method="afinn")
head(afinn_vector,10)
summary(afinn_vector)

##nrc
nrc_vector<-get_sentiment(text, method="nrc")
head(nrc_vector,10)
summary(nrc_vector)

#compare the lexicon for the first six lines

rbind(sign(head(syuzhet_vector)),
      sign(head(bing_vector)),
      sign(head(afinn_vector)))

##emotion classification
##nrc bagi 8 classify 8 sentiments

d<-get_nrc_sentiment(text)
head(d,10)

##visualization

td<-data.frame(t(d))
td_new<-data.frame(rowSums(td[2:253]))
names(td_new)[1]<-"count"
td_new<-cbind("sentiment"=rownames(td_new),td_new)
rownames(td_new)<-NULL
td_new2<-td_new[1:8,]

#plot 1 #count of  word associated with each sentiment
quickplot(sentiment, data=td_new2,weight=count,geom="bar",
          fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#plot 2 count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[,1:8]))),
  horiz=T,
  cex.names=0.7,
  las=1,
  main="Emotions in text", xlab="Percentage"
)
