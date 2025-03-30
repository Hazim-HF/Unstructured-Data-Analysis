library(tm)
docs<-Corpus(DirSource("C:\\Users\\user\\Downloads\\Master's Degree\\Sem 2\\Analitik Data Tak Berstruktur\\Week 8a\\TextMining-20220613"))
writeLines(as.character(docs[30]))

##Pre-Process
docs<-tm_map(docs, content_transformer(tolower))
toSpace<-content_transformer(function(x,pattern){return(gsub(pattern," ", x))}) #symbol
docs<-tm_map(docs, toSpace,"-")
docs<-tm_map(docs,toSpace,":")
docs<-tm_map(docs,toSpace,"'")
docs<-tm_map(docs,toSpace,"·")
docs<-tm_map(docs,toSpace,"·   ")
docs<-tm_map(docs,toSpace,""")
docs<-tm_map(docs,toSpace,""")

docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords, stopwords("english"))
docs<-tm_map(docs, stripWhitespace)

writeLines(as.character(docs[30]))

docs<-tm_map(docs, stemDocument, language = "english")
docs<-tm_map(docs,content_transformer(gsub), pattern="organis", replacement="organ")
docs<-tm_map(docs,content_transformer(gsub), pattern="organiz", replacement="organ")
docs<-tm_map(docs,content_transformer(gsub), pattern="andgovern", replacement="govern")
docs<-tm_map(docs,content_transformer(gsub), pattern="inenterpris", replacement="enterpris")
docs<-tm_map(docs,content_transformer(gsub), pattern="team-", replacement="team")

myStopwords<-c("can","say","one","way","use","also","howev","tell","will",
               "much","need","take","tend","even","like","particular","rather",
               "said","get","well","make","ask","come","end","first",
               "two","help","often","may","might","seen","someth","thing",
               "point","post","look","right","now","think","'ve","'re")
docs<-tm_map(docs,removeWords, myStopwords)

dtm<-DocumentTermMatrix(docs)
dtm.tfidf<-weightTfIdf(dtm) #for normalisation
dtm.tfidf<-removeSparseTerms(dtm.tfidf,0.999)
tfidf.Matrix<-as.matrix(dtm.tfidf)
inspect(dtm)

library(proxy)
dist.matrix=dist(tfidf.Matrix, method='cosine')

##Perform Clustering
truth.K=2
library(dbscan)

#Kmeans 2 cluster
clustering.kmeans<-kmeans(tfidf.Matrix,truth.K)
library(cluster)
clusplot(as.matrix(dist.matrix), clustering.kmeans$cluster, color=T, shade=T, labels=2,lines=0)

#Kmeans 3 cluster
clustering.kmeans<-kmeans(tfidf.Matrix, 3)
clusplot(as.matrix(dist.matrix), clustering.kmeans$cluster, color=T, shade=T, labels=2,lines=0)

#hierarchical
clustering.hierarchical<-hclust(dist.matrix, method="ward.D2")
summary(clustering.hierarchical)
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,2) #2- no of clusters
rect.hclust(clustering.hierarchical,3) 

##dbscan
clustering.dbscan<-hdbscan(dist.matrix, minPts=10)
plot(as.matrix(dist.matrix),col=clustering.dbscan$cluster+1L) #1L - cosine similarity

#combine result

master.cluster<-clustering.kmeans$cluster
slave.hierarchical<-cutree(clustering.hierarchical, k=3)
slave.dbscan<-clustering.dbscan$cluster

#plotting combine

library(colorspace)
points<-cmdscale(dist.matrix, k=2)
palette<-diverge_hcl(3) #how many colors

layout(matrix(1:3, ncol=1))

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

table(master.cluster)
table(slave.hierarchical)
table(slave.dbscan)

##Elbow plot

cost_df<-data.frame()

#run k-means for all clusters up to 100

for(i in 1:100){
  kmeans<-kmeans(x=tfidf.Matrix,centers=i,iter.max=100)
  
  #combine clusters number
  cost_df<-rbind(cost_df, cbind(i,kmeans$tot.withinss))
}
names(cost_df)<-c("cluster","cost")

plot(cost_df$cluster,cost_df$cost)
lines(cost_df$cluster,cost_df$cost)


####Example 2

### Data Acquisition
# Creating the empty dataset with the formatted columns
dataframe <- data.frame(ID=character(),
                        datetime=character(),
                        content=character(),
                        label=factor())
source.url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip'
target.directory <- '/tmp/clustering-r'
temporary.file <- tempfile()
download.file(source.url, temporary.file)
unzip(temporary.file, exdir = target.directory)
# Reading the files
target.directory <- paste(target.directory, 'Health-Tweets', sep = '/')
files <- list.files(path = target.directory, pattern='.txt$')

# Filling the dataframe by reading the text content
for (f in files) {
  news.filename = paste(target.directory , f, sep ='/')
  news.label <- substr(f, 0, nchar(f ) - 4) # Removing the 4 last characters => '.txt'
  news.data <- read.csv(news.filename,
                        encoding = 'UTF-8',
                        header = FALSE,
                        quote = "",
                        sep = '|',
                        col.names = c('ID', 'datetime', 'content'))
  # Trick to handle native split problem (cf. notebook for detail)
  news.data <- news.data[news.data$content != "", ]
  news.data['label'] = news.label # We add the label of the tweet
  # Massive data loading memory problem : only loading a few (cf. notebook for detail)
  news.data <- head(news.data, floor(nrow(news.data) * 0.05))
  dataframe <- rbind(dataframe, news.data) # Row appending
}
unlink(target.directory, recursive = TRUE) # Deleting the temporary directory

#Data preparation
sentences <- sub("http://([[:alnum:]|[:punct:]])+", '', dataframe$content)
corpus <- Corpus(VectorSource(sentences))
### Data preprocessing (Cleaning up)
# Handling UTF-8 encoding problem from the dataset
#for Mac corpus.cleaned <- tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
corpus.cleaned <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus.cleaned <- tm_map(corpus.cleaned, removeWords, stopwords('english')) # Removing stop-wor
ds
corpus.cleaned <- tm_map(corpus, stemDocument, language = "english") # Stemming the words
corpus.cleaned <- tm_map(corpus.cleaned, stripWhitespace) # Trimming excessive whitespaces
strwrap(corpus.cleaned[3158])
### Present text data numerically, weighted TF-IDF
tdm <- DocumentTermMatrix(corpus.cleaned)
tdm.tfidf <- weightTfIdf(tdm)
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.999)
tfidf.matrix <- as.matrix(tdm.tfidf)
inspect(tdm)

### Cosine distance matrix (useful for specific clustering algorithms)
library(proxy)
dist.matrix = dist(tfidf.matrix, method = "cosine")
truth.K=5
### Perform clustering
library(dbscan)
clustering.kmeans <- kmeans(tfidf.matrix, truth.K)
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2")
clustering.dbscan <- hdbscan(dist.matrix, minPts = 10)

#Kmeans 3 cluster
clustering.kmeans<-kmeans(tfidf.Matrix, truth.K)
clusplot(as.matrix(dist.matrix), clustering.kmeans$cluster, color=T, shade=T, labels=2,lines=0)

#hierarchical
clustering.hierarchical<-hclust(dist.matrix, method="ward.D2")
summary(clustering.hierarchical)
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,5) #5- no of clusters


##dbscan
clustering.dbscan<-hdbscan(dist.matrix, minPts=10)
plot(as.matrix(dist.matrix),col=clustering.dbscan$cluster+1L) #1L - cosine similarity


#Combine results
master.cluster <- clustering.kmeans$cluster
slave.hierarchical <- cutree(clustering.hierarchical, k = truth.K)
slave.dbscan <- clustering.dbscan$cluster

#plotting results
library(colorspace)
points <- cmdscale(dist.matrix, k = 2)
palette <- diverge_hcl(truth.K) # Creating a color palette, need library(colorspace)
layout(matrix(1:3,ncol=1))
plot(points, main = 'K-Means clustering', col = as.factor(master.cluster),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
#Tabulate how many documents in each clusters
table(master.cluster) #k-means
table(slave.hierarchical) #hierarchical
table(slave.dbscan) #hdbscan

#Elbow plot
#accumulator for cost results
cost_df <- data.frame()
#run kmeans for all clusters up to 100
for(i in 1:1000){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=tfidf.matrix, centers=i, iter.max=100)
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
plot(cost_df$cluster, cost_df$cost)
lines(cost_df$cluster, cost_df$cost)