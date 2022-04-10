# 11.Topic분석 : LDA모형(토픽분석)
library(topicmodels)

# 11.1 LDA
youth_lda <- LDA(dtm, k=5, control=list(seed=1234))

youth_lda





# 11.2 특정 토픽에서 특정단어가 생성될 확률
youth_topic <- youth_lda %>% # 토픽(2)X단어(5) 행렬
  tidy(matrix = "beta") 

youth_topic

## 토픽별로 정리
youth_topic_terms <- youth_topic %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  ungroup() %>%
  arrange(topic, -beta)

youth_topic_terms

## 토픽별로 그래프
youth_topic_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  mutate(topic = str_c("topic", topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## 토픽별 비교
beta_wide <- youth_topic_terms %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = beta) %>%
  arrange(desc(topic1, topic2, topic3, topic4, topic5))

beta_wide %>%
  print(n=40)







## 11.3 특정 문서가 특정 토픽을 반영할 확률
youth_doc <- youth_lda %>%
  tidy(matrix="gamma")


gamma_wide <- youth_doc %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = gamma) %>%
  arrange(desc(topic1, topic2, topic3, topic4, topic5))

gamma_wide %>%
  print(n=40)









