library(tidytext)
library(topicmodels)
library(tidyr)
library(ggplot2)
library(dplyr)

data("AssociatedPress")
ap_lda<-LDA(AssociatedPress, k=2, control=list(seed=1234)) #create 2-topics LDA
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta") #extract per-topic per- word prob
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_wide%>%mutate(term=reorder(term,log_ratio))%>%ggplot(aes(term, log_ratio))+geom_col(show.legend=FALSE)+coord_flip()


ap_documents <- tidy(ap_lda, matrix = "gamma") #extract per-doc per-topic prob
ap_documents

tidy(AssociatedPress) %>%     #in doc 6
  filter(document == 6) %>%
  arrange(desc(count))
