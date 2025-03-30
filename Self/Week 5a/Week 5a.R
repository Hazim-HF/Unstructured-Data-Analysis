library(tm)
library(stringr)
mytext<-VectorSource(sentences)
docs<-Corpus(mytext)
inspect(docs)

writeLines(as.character(docs[[30]])) #baca line

getTransformations()

#create custom transformation
toSpace<-content_transformer(function(x,pattern)
  {return(gsub(pattern," ",x))})
as.character(docs[[191]])

docs<-tm_map(docs,toSpace,"-") #hilangkan -
as.character(docs[[191]])


docs<-tm_map(docs,removePunctuation)
inspect(docs)
docs<-tm_map(docs,content_transformer(tolower))
inspect(docs)
docs<-tm_map(docs,removeNumbers)
inspect(docs)
docs<-tm_map(docs,removeWords, stopwords("english"))
inspect(docs)

docs<-tm_map(docs,removeWords,"slid") #remove specific word
inspect(docs)    
docs<-tm_map(docs,stripWhitespace)
inspect(docs) 

library(SnowballC)
docs2<-tm_map(docs,stemDocument) #remove imbuhan.  ada ayat yang ubah maksud
inspect(docs2) 

writeLines(as.character(docs[43])) 
writeLines(as.character(docs2[43]))

library(textstem)
docs3<-stem_strings(docs)[1]
writeLines(as.character(docs3))
a<-unlist(str_split(docs3,"[,]"))
docs4<-lemmatize_strings(docs)[1]
b<-unlist(str_split(docs4,"[,]"))

#compare stem and lemmatize
a[43] #stem
b[43] #lemmatize


#compare original and lemmatize

sentences[3]
b[3] #lemmatize ubah maksud perkataan


dtm<-DocumentTermMatrix(docs)
inspect(dtm[1:2,1:100]) #dia shows ayat 1:2
freq<-colSums(as.matrix(dtm))
length(freq) #after cleaning using tm_map

ord<-order(freq, decreasing=T)
freq[head(ord)] #top 5 frequency

dtm<-DocumentTermMatrix(docs, control=list(wordLengths=c(2,20),
                                           bounds=list(global=c(2,30)))) #filter panjang & freq

inspect(dtm[1:2,1:100])
freq<-colSums(as.matrix(dtm))
length(freq)
ord<-order(freq, decreasing=T)
freq[head(ord)]

##Once done, insert into dataframe
wf<-data.frame(names(freq),freq)
names(wf)<-c("TERM","FREQ")
head(wf)

findFreqTerms(dtm,lowfreq=10) #words yang freq at least 10
findAssocs(dtm,"get",0.3) #correlation at least 0.30 with get word

library(ggplot2)
subs<-subset(wf,FREQ>=10)
ggplot(subs,aes(x=TERM, y=FREQ))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1)) #only for >=10 freq 

ggplot(wf,aes(x=TERM, y=FREQ))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1)) #show all including small freq


library(wordcloud)
wordcloud(names(freq),freq) #in general
wordcloud(names(freq),freq.min.freq=10) #focus at >=10 
wordcloud(names(freq),freq,colors=brewer.pal(8,"Darker")) #tak dapat run
wordcloud(names(freq),freq,colors=brewer.pal(12,"Paired"))

install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(wf)
wordcloud2(wf, size=0.5)
wordcloud2(wf, size=0.5, color="random-light", backgroundColor="black")
wordcloud2(wf,shape="star",size=0.5)
wordcloud2(wf,figPath="love.png", color="skyblue", backgroundColor="black")

letterCloud(wf,word="R",color="random-light",backgroundColor="black")
