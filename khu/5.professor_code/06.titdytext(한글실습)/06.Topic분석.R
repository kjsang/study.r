# 11.Topic분석 : LDA모형(토픽분석)
library(topicmodels)
library(furrr)






# 11.1 최적 토픽수 분석

topics <- c(2:28)

bible_lda <- topics %>%
  future_map(LDA, x = bible_dtm, control = list(seed = 1234))

bible_lda_prep <- tibble(k = topics,
                         perplex = map_dbl(bible_lda, 
                                           perplexity))
bible_lda_prep

bible_lda_prep%>%
  ggplot(mapping = aes(x = k, 
                       y = perplex)) +
  geom_point() +
  geom_line()






# 11.2 10 토픽으로 분석
bible_lda_10 <- LDA(bible_dtm, k=10, control=list(seed=1234))

str(bible_lda_10)






## 11.3 특정 토픽에서 특정단어가 생성될 확률
bible_topic_10 <- bible_lda_10 %>% # 토픽(2)X단어(5) 행렬
  tidy(matrix = "beta") 

bible_topic_10

## 토픽별로 정리
bible_topic_terms <- bible_topic_10 %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  ungroup() %>%
  arrange(topic, -beta)

bible_topic_terms

## 토픽별로 그래프
bible_topic_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(mapping = aes(x = beta, 
                       y = term, 
                       fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## 상대비교
beta_wide <- bible_topic_terms %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = beta) %>%
  arrange(desc(topic1, topic2, topic3, topic4, topic5))

beta_wide %>%
  print(n=40)

terms(bible_lda_10, 10)






## 11.4 특정 문서가 특정 토픽을 반영할 확률
bible_gamma <- bible_lda_10 %>%
  tidy(matrix="gamma")

gamma_wide <- bible_gamma %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = gamma) %>%
  arrange(desc(topic1, topic2, topic3, topic4, topic5))

gamma_wide %>%
  print(n=30)
