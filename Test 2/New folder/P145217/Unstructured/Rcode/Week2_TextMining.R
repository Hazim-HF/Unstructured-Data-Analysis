# Part 1: Extract Text from Files
setwd("C:/Users/PC 12/Desktop/P145217/Unstructured/Rcode/")

eg1<-read.table("Attachment 1-20250505/GC.txt",fill=T,header=F) #Data GC.txt
eg1[1,]
eg2<-read.csv("Attachment 1-20250505/GC.csv",header=F) #Data GC.csv
eg2[1,]
#Using tm package
library(tm)
eg3<-c("Hi!","Welcome to STQD6114","Tuesday, 11-1pm")
mytext<-VectorSource(eg3)
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

# Three common source of data input method
#Example using DirSource
mytext<-DirSource("Attachment 2 - movies-20250505")
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

#Example using VectorSource
eg4<-t(eg1) #From example 1
a<-sapply(1:7,function(x)
  trimws(paste(eg4[,x],collapse=" "),"right"))
a = apply(eg1, MARGIN=1, function(x) {trimws(paste(x, collapse=' '))}) # or use this
mytext<-VectorSource(a)
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

#Example using DataFrameSource
eg5<-read.csv("Attachment 1-20250505/Doc6.csv",header=F) #Using doc6.csv
docs<-data.frame(doc_id=c("doc_1","doc_2"),
                 text=c(as.character(eg5[1,]),as.character(eg5[2,])),
                 dmeta1=1:2,dmeta2=letters[1:2],stringsAsFactors=F)
mytext<-DataframeSource(docs)
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

eg6<-readLines("https://en.wikipedia.org/wiki/Data_science")
eg6[grep("\\h2",eg6)]
eg6[grep("\\p",eg6)] #paragraph
#Using library XML
library(XML)
doc<-htmlParse(eg6)
doc.text<-unlist(xpathApply(doc,'//p',xmlValue))
unlist(xpathApply(doc,'//h2',xmlValue))
#Using library httr
eg7<-GET("https://www.edureka.co/blog/what-is-data-science/")
doc<-htmlParse(eg7)
doc.text<-unlist(xpathApply(doc,'//p',xmlValue))

#Using library rvest
library(rvest)
eg8<-read_html("https://www.edureka.co/blog/what-is-data-science/")
nodes<-html_nodes(eg8,'.color-4a div span , .btn-become-profesional-link+ p')
texts<-html_text(nodes)
#Selecting multiple pages
pages<-
  paste0('https://www.amazon.co.jp/s?k=skincare&crid=28HIW1TYLV9UM&sprefix=skincare%2Caps%2C268&r
ef=nb_sb_noss_1&page=',0:9)
eg10<-read_html(pages[1])
nodes<-html_nodes(eg10,'.a-price-whole')
texts<-html_text(nodes)
Price<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url ,'.a-price-whole ')
  html_text(nodes)}
sapply(pages,Price)
do.call("c",lapply(pages,Price))

