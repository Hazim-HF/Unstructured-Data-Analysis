# R submission for test 1
# 4.
library(stringr)

monkey = read.csv("C:/Users/PC 12/Downloads/LittleMonkeys.csv",header=F)
monkey

# 4a)
str_view(monkey,"\\!")

# 4b)
str_view(monkey, "(.)\\1")

# 4c)
str_view(monkey, "[aeiou]{2}")

# 4d)
gregexpr(pattern="mon",monkey)[[1]]
# Index: 55 178 221 344 388 512 554 677 719 840
str_view(monkey, "\\bmon")
# "mon" after a boundary 
# not using ^ because it doesnt require the SENTENCE to start with "mon"

# 4e)
str_count(monkey, "monkeys")
# 9 occurences



# =====================
# 5.
library(tm)
# Input
news = read.table("C:/Users/PC 12/Downloads/BBCnews.txt",fill=T,header=F)
head(news)
news1 = apply(news, MARGIN=1, function(x) {trimws(paste(x, collapse=' '))}) # or use this
head(news1)

# Convert to corpus
mytext = VectorSource(news1)
mycorpus = Corpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])


# 5a)
# Text preprocessing
# Function to remove matching pattern
toSpace = content_transformer(function(x,pattern){return(gsub(pattern," ",x))})
toNothing = content_transformer(function(x,pattern){return(gsub(pattern,"",x))})

docs = mycorpus

# Remove hyphens '-'
docs1 = tm_map(docs,toSpace, '-') 
# Remove numbers
docs2 = tm_map(docs1,removeNumbers)
# Remove punctuation
docs3 = tm_map(docs2,removePunctuation)
# Transform lowercase
docs4 = tm_map(docs3,content_transformer(tolower))
# Remove stopwords
docs5 = tm_map(docs4,removeWords,stopwords("english")) 
# Strip whitespace
docs6 = tm_map(docs5,toSpace,'\\s+') # Remove multiple white space artifacts that come from previous preprocessing
docs7 = tm_map(docs6,toNothing,'(^\\s+|\\s+$)') # Removing leading and trailing whitespae
# docs7 is the final output


# 5b)
# Lemmatization
library(textstem)
docs8 = lemmatize_strings(docs7)

docs9 = unlist(str_split(docs8,"[,]"))
docs9


# 5c) Tabulate the terms and frequency
library(dplyr)

dtm<-DocumentTermMatrix(docs9)
dtm

dtm_m = as.matrix(dtm)
freq = colSums(dtm_m)
freq

freqtable = data.frame(term=names(freq), count=freq)
freqtable

freqtable5 = freqtable %>% filter(freq>=5)
freqtable5

# 5d) Word cloud + barchart
library(wordcloud)
wordcloud(names(freq),freq,colors=brewer.pal(8,"Dark2"), random.order = F)

# Barchart with freq > 5, too many terms 
ggplot(freqtable5,aes(x=term,y=count))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1)) #Show all, include terms that hv small freq

# Barchart with freq > 10
ggplot(freqtable5 %>% filter(count>10),aes(x=term,y=count))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1)) #Show all, include terms that hv small freq



# 5e) Associated terms with "coast"
findAssocs(dtm, "coast", 0.2)













