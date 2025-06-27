setwd("C:/Users/PC 12/Desktop/P145217/Unstructured/")

library(tm)

mytext<-DirSource("TextMining")
docs<-VCorpus(mytext)
docs <- tm_map(docs,content_transformer(tolower))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "[-:'\\.-]")
docs <- tm_map(docs, removePunctuation) #remove punctuation
docs <- tm_map(docs, removeNumbers) #Strip digits
docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords
docs <- tm_map(docs, stripWhitespace) #remove whitespace
tdm <- DocumentTermMatrix(docs) #Create document term matrix
tdm
dtm = DocumentTermMatrix(docs, control=list(wordLengths=c(2,30),
                                            bounds=list(global=c(2,30))))
#Present text data numerically, weighted TF-IDF
tdm.tfidf <- weightTfIdf(tdm)
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.999)
tfidf.matrix <- as.matrix(tdm.tfidf)
# Cosine distance matrix (useful for specific clustering algorithms)
library(proxy)
dist.matrix <- dist(tfidf.matrix, method = "cosine")



truth.K=2
#Perform clustering
library(dbscan)
clustering.kmeans <- kmeans(tfidf.matrix, truth.K)
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2")
clustering.dbscan <- hdbscan(dist.matrix, minPts = 10)
library(cluster)
clusplot(as.matrix(dist.matrix),clustering.kmeans$cluster,color=T,shade=T,labels=2,lines=0)
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,2)
plot(as.matrix(dist.matrix),col=clustering.dbscan$cluster+1L)
#Combine results
master.cluster <- clustering.kmeans$cluster
slave.hierarchical <- cutree(clustering.hierarchical,
                             k = truth.K)
slave.dbscan <- clustering.dbscan$cluster
#plotting results
library(colorspace)
points <- cmdscale(dist.matrix, k = 2)
palette <- diverge_hcl(truth.K) # Creating a color palette, need library(colorspace)
#layout(matrix(1:3,ncol=1))
plot(points, main = 'K-Means clustering', 
     col = as.factor(master.cluster),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Hierarchical clustering', 
     col = as.factor(slave.hierarchical),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Density-based clustering', 
     col = as.factor(slave.dbscan),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
table(master.cluster)
table(slave.hierarchical)
table(slave.dbscan)


#Elbow plot
#accumulator for cost results
cost_df <- data.frame()
#run kmeans for all clusters up to 100
for(i in 1:20){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=tfidf.matrix, centers=i, iter.max=100)
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
plot(cost_df$cluster, cost_df$cost)
lines(cost_df$cluster, cost_df$cost)

