# Text Sentiment Analysis

library(tm) # for text mining
library(SnowballC) # for text stemming
library(wordcloud) # word-cloud generator
library(RColorBrewer) # color palettes
library(syuzhet) # for sentiment analysis
library(ggplot2) # for plotting graphs

# Read the text file from local machine , choose file interactively
text <- readLines(file.choose())
#text = read.table(file.choose(), fill=T, header =F)

## Sentiment scores
# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods have different scales
syuzhet_vector <- get_sentiment(text, method="syuzhet")
head(syuzhet_vector)
head(syuzhet_vector,10) # see the first 10 elements of the vector
summary(syuzhet_vector)
# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)
#afinn
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)
#nrc
nrc_vector <- get_sentiment(text, method="nrc")
head(nrc_vector)
summary(nrc_vector)
#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

## Emotion classification
# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score :
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust
# and if the sentiment is positive or negative

d<-get_nrc_sentiment(text)
head (d,10) # head(d,10) - just to see top 10 lines
#Visualization
td<-data.frame(t(d)) #transpose
td_new <- data.frame(rowSums(td)) #The function rowSums computes column sums across rows for each level of a grouping variable.
names(td_new)[1] <- "count" #Transformation and cleaning
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot 1 - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment,ylab="count")+ggtitle("Survey sentiments")
#Plot 2 - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab="Percentage"
)