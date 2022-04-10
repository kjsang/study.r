# 11.Topic분석 : LDA모형(토픽분석)
library(topicmodels)

# 11.1 LDA
text_lda <- LDA(dtm, k=2, control=list(seed=1234))

text_lda




## 11.2 특정 토픽에서 특정단어가 생성될 확률
text_topic <- text_lda %>% # 토픽(2)X단어(5) 행렬
  tidy(matrix = "beta") 
text_topic

## 토픽별로 정리
text_topic_terms <- text_topic %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  ungroup() %>%
  arrange(topic, -beta)
text_topic_terms

## 토픽별로 그래프
text_topic_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  mutate(topic = str_c("topic", topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## 상대비교
beta_wide <- text_topic %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = beta) %>%
  arrange(desc(topic1, topic2))

beta_wide






## 11.3 특정 문서가 특정 토픽을 반영할 확률
text_doc <- text_lda %>%
  tidy(matrix="gamma")

gamma_wide <- text_doc %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = gamma) %>%
  arrange(desc(topic1, topic2))

gamma_wide









