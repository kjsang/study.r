# 11.Topic분석 : LDA모형(토픽분석)
library(topicmodels)
library(furrr)


# 11.1 최적 토픽수 분석

topics <- c(2:10)

study_lda <- topics %>%
  future_map(LDA, x = study_dtm, control = list(seed = 1234))

study_lda_prep <- tibble(k = topics,
                         perplex = map_dbl(study_lda, 
                                           perplexity))
study_lda_prep

study_lda_prep%>%
  ggplot(mapping = aes(x = k, 
                       y = perplex)) +
  geom_point() +
  geom_line()






# 11.2 5 토픽으로 분석
study_lda_5 <- LDA(study_dtm, k=5, control=list(seed=1234))

str(study_lda_5)






## 11.3 특정 토픽에서 특정단어가 생성될 확률
study_topic_5 <- study_lda_5 %>% # 토픽(2)X단어(5) 행렬
  tidy(matrix = "beta") 

study_topic_5

## 토픽별로 정리
study_topic_terms <- study_topic_5 %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  ungroup() %>%
  arrange(topic, -beta)

study_topic_terms

## 토픽별로 그래프
study_topic_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(mapping = aes(x = beta, 
                       y = term, 
                       fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## 상대비교
beta_wide <- study_topic_terms %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = beta) %>%
  arrange(desc(topic1, topic2, topic3, topic4, topic5))

beta_wide %>%
  print(n=40)

terms(study_lda_5, 10)






## 11.4 특정 문서가 특정 토픽을 반영할 확률
study_gamma <- study_lda_5 %>%
  tidy(matrix="gamma")

gamma_wide <- study_gamma %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = gamma) %>%
  arrange(desc(topic1, topic2, topic3, topic4, topic5))

gamma_wide %>%
  print(n=30)
