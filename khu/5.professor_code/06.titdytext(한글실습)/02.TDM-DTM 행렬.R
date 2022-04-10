## 6. 단어빈도 그래프

## 6.1 단어빈도 출력  
bible_count %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 50) %>%
  ggplot(aes(x=n, y=word)) +
  geom_col()


## 6.2 단어 tf-idf 출력  
bible_count %>%
  mutate(word = reorder(word, tf_idf)) %>%
  slice_max(tf_idf, n = 20) %>%
  ggplot(aes(x=tf_idf, y=word)) +
  geom_col()


## 6.3 단어 log_odds 출력 (추가)
bible_count %>%
  mutate(word = reorder(word, log_odds)) %>%
  slice_max(log_odds, n = 20) %>%
  ggplot(mapping = aes(x=log_odds, 
                       y=word)) +
  geom_col()

# 4번으로 다시 이동해서 문제단어 삭제 및 정리





# 7.단어-문서 행렬 만들기

# 7.단어-문서 행렬 만들기
# ppt 설명

## 7.1 tdm 만들기
bible_tdm <- bible_tf_idf %>%
  cast_tdm(document = 장,       # document = 장 
           term = word, 
           value = n)

tm::inspect(bible_tdm)


bible_tdm_tf_idf <- bible_tf_idf %>%
  cast_tdm(document = 장, 
           term = word, 
           value = tf_idf)

tm::inspect(bible_tdm_tf_idf)


## 7.2 dtm 만들기 
bible_dtm <- bible_tf_idf %>%
  cast_dtm(document = 장, 
           term = word, 
           value = n)

tm::inspect(bible_dtm)

bible_dtm_tf_idf <- bible_tf_idf %>%
  cast_dtm(document = 장, 
           term = word, 
           value = tf_idf)

tm::inspect(bible_dtm_tf_idf)

