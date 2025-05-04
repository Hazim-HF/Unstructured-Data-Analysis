install.packages('tidytext')

library(topicmodels)

data('AssociatedPress')

AssociatedPress

ap_lda = LDA(AssociatedPress, k=4, control=list(seed=1234))

library(tidytext)

ap_topics = tidy(ap_lda, matrix='beta')

library(ggplot2)
library(dplyr)

ap_top_terms = ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) # added

ggplot(data=ap_top_terms, aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales='free') +
  coord_flip() +
  scale_x_reordered() # added

library(tidyr)

beta_spread = ap_topics %>%
  mutate(topic = paste0('topic', topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > 0.003 | topic2 > .003) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  mutate(term=reorder(term, log_ratio))

ggplot(data=beta_spread, aes(term, log_ratio)) + 
  geom_col(show.legend=FALSE) +
  coord_flip()

getwd()

#----------------------------------------------------------------

### Text Analysis I: LDA
library(tidytext)
library(topicmodels)
library(tidyr)
library(ggplot2)
library(dplyr)

data("AssociatedPress")

ap_lda<-LDA(AssociatedPress,k=2,control=list(seed=1234)) #create two-topic LDA model

ap_topics<-tidy(ap_lda,matrix="beta") #Extract the per-topic-per-word-probabilities

#Find terms that are most common within each topics
ap_top_terms <- ap_topics %>% 
  group_by(topic) %>% 
  top_n(10,beta) %>% 
  ungroup () %>% 
  arrange (topic, -beta)

ap_top_terms %>% 
  mutate(term=reorder(term,beta)) %>%
  ggplot(aes(term,beta,fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic,scales="free") +
  coord_flip() #visualize the above

beta_spread <- ap_topics %>% 
  mutate (topic=paste0("topic",topic)) %>% 
  spread(topic,beta) %>%
  filter (topic1>0.001 | topic2 > 0.001) %>% 
  mutate(log_ratio = log2(topic2/topic1))

beta_spread %>% 
  mutate(term=reorder(term,log_ratio)) %>%
  ggplot(aes(term,log_ratio)) +
  geom_col(show.legend=FALSE) +
  coord_flip()

ap_documents = tidy(ap_lda, matrix="gamma") #Extract the per-document-per-topic-probabilities

ap_documents

tidy(AssociatedPress) %>%
  filter(document==6) %>%
  arrange(desc(count)) #Check the most common words in the document, eg document 6