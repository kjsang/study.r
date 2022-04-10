## 6. 단어빈도 그래프

## 6.1 구분별 단어빈도 출력  
study_group_tf_idf %>%
  group_by(구분) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, 구분)) %>%
  ggplot(aes(x = n, 
             y = word, 
             fill = 구분)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ 구분, ncol = 2, scales = "free") +
  scale_y_reordered()


## 6.2 구분별 단어 tf-idf 출력  
study_group_tf_idf %>%
  group_by(구분) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, 구분)) %>%
  ggplot(aes(x = tf_idf, 
             y = word, 
             fill = 구분)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ 구분, ncol = 2, scales = "free") +
  scale_y_reordered()


## 6.3 구분별 단어 log_odds 출력
study_group_tf_idf %>%
  group_by(구분) %>%
  slice_max(log_odds, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, log_odds, 구분)) %>%
  ggplot(aes(x = log_odds, 
             y = word, 
             fill = 구분)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ 구분, ncol = 2, scales = "free") +
  scale_y_reordered()

# 4번으로 다시 이동해서 문제단어 삭제 및 정리





# 7.단어-문서 행렬 만들기

# 7.단어-문서 행렬 만들기
# ppt 설명

## 7.1 tdm 만들기
study_group_tdm <- study_group_tf_idf %>%
  cast_tdm(document = 구분,  # 구분으로 수정
           term = word, 
           value = n)

tm::inspect(study_group_tdm)


study_group_tdm_tf_idf <- study_group_tf_idf %>%
  cast_tdm(document = 구분, # 구분으로 수정
           term = word, 
           value = tf_idf)

tm::inspect(study_group_tdm_tf_idf)


## 7.2 dtm 만들기 
study_group_dtm <- study_group_tf_idf %>%
  cast_dtm(document = 구분, # 구분으로 수정
           term = word, 
           value = n)

tm::inspect(study_group_dtm)

study_group_dtm_tf_idf <- study_group_tf_idf %>%
  cast_dtm(document = 구분, # 구분으로 수정
           term = word, 
           value = tf_idf)

tm::inspect(study_group_dtm_tf_idf)








