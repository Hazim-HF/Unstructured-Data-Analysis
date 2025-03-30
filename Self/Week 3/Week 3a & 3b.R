#Retrieve textual data

eg1<-read.table(file.choose(), fill=T, header=F) # header=T if we use row and column
eg1                                                  # fill=T required if for text data
eg1[1,] #first sentense


eg2<-read.csv(file.choose(), header=F)
eg2[1,]

##VectorSource

library(tm)
eg3<-c("Hi", "Welcome to STQD6114", "Tuesday, 11-1pm")
mytext<-VectorSource(eg3)
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

eg4<-t(eg1) #t = transpose
a<-sapply(1:7, function(x)                       #tukarkan text kepada vector format mcm
  trimws(paste(eg4[,x], collapse=" "), "right")) #csv dan juga a<-c("d", "s"). Then baru boleh guna VectorSource
mytext<-VectorSource(a)
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

##DataframeSource
## sebab second row terlalu panjang
eg5<-read.csv(file.choose(), header=F)
docs<-data.frame(doc_id=c("doc_1","doc_2"),
                 text=c(as.character(eg5[1,]), as.character(eg5[2,])),
                 dmeta1=1:2,dmeta2=letters[1:2], stringsAsFactors=F)
mytext<-DataframeSource(docs)
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

mytext<-DirSource("C:\\Users\\user\\Downloads\\Master's Degree\\Sem 2\\Analitik Data Tak Berstruktur\\Week 3\\movies-20220516") #folder kena ada dekat document
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]]) ##document pertama

##Exercise
#Create a folder 7 files (document) of songs lyrics
#and read these documents as corpus in R

eg6<-readLines("https://en.wikipedia.org/wiki/Data_science") #copy URL
eg6[grep("\\h2",eg6)] ##h2 = second header. kalau p=paragraph
eg6[grep("\\p",eg6)] #kalau p=paragraph

install.packages("XML")
library(XML)
doc<-htmlParse(eg6)
doc.text<-unlist(xpathApply(doc,'//p',xmlValue)) ##untuk baca p
unlist(xpathApply(doc,'//h2',xmlValue))

library(httr)
eg7<-GET("https://en.wikipedia.org/wiki/Data_science")
doc<-htmlParse(eg7)
doc.text<-unlist(xpathApply(doc,'//h2',xmlValue))

library(rvest)
eg8<-read_html("https://www.edureka.co/blog/what-is-data-science/")
nodes<-html_nodes(eg8,'li') ##select from the website content that we want
texts<-html_text(nodes) ##sama macam unlist dalam package XML


##Multiple pages from web

pages<-paste0("https://www.amazon.com/s?k=ramen&language=en_US&crid=1UJV4ZWD2XNN1&currency=JPY&sprefix=ram%2Caps%2C387&ref=nb_sb_noss_1",0:4)
eg9<-read_html(pages[1])  #get extract info from 1st page
nodes<-html_nodes(eg9,'.a-price-whole')
texts<-html_text(nodes)

## select dua maklumat dari selector gadget
nodes<-html_nodes(eg9,'.a-text-normal , .a-price-whole')
texts<-html_text(nodes)

Price<-function(pages){
  url<-read_html(pages)
  nodes<-html_nodes(url,'.a-text-normal , .a-price-whole')
  html_text(nodes)
}

sapply(pages,Price)
price_item<-do.call(c,lapply(pages,Price))

##Twitter API

library(rtweet)
twitter_token<-create_token(
  app=appname,
  consumer_key=key,
  consumer_secret=secret,
  access_token=access_token,
  access_secret=access_secret)

##Twitter's REST API
#search 500 tweets using #covid

rstat_tweets<-search_tweets(q='#covid',n=500, lang='en')
rstat_tweets$text

users<-search_users(q="#covid",n=500) #untuk cari maklumat tentang users yang guna #covid

##Twitter's STREAM API

streamtime=60 # strean for 60 second

#backup file
filename<-"rtry.json"

#stream tweets

rstats_tweets<-stream_tweets(q="#covid", timeout=streamtime,
                             file_name=filename, parse=F) 
rtweet<-parse_stream(filename) ##tweet
rtweet$text

users_data(rtweet) ##users information


##Spotify

install.packages("spotifyr")
library(spotifyr)
access_token<-get_spotify_access_token()
queen_features<-get_artist_audio_features(artist="Queen")
