## 6. 단어빈도 그래프

## 6.1 단어빈도 출력  
youth_count %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 40) %>%
  ggplot(mapping = aes(x = n, 
                       y = word)) +
  geom_col()


## 6.2 단어 tf-idf 출력  
youth_count %>%
  mutate(word = reorder(word, tf_idf)) %>%
  slice_max(tf_idf, n = 20) %>%
  ggplot(mapping = aes(x=tf_idf, 
                       y=word)) +
  geom_col()





# 7.단어-문서 행렬 만들기
# ppt 설명

# 7.1 tdm 만들기
tdm <- youth_tf_idf %>%
  cast_tdm(document = no, 
           term = word, 
           value = n)

# inspect는 여러 패키지에서 사용하므로 동시에 
# 사용할때는 특정패키지 명을 지정

tm::inspect(tdm)


tdm_tf_idf <- youth_tf_idf %>%
  cast_tdm(document=no, 
           term=word, 
           value=tf_idf)

tm::inspect(tdm_tf_idf)


## 7.2 dtm 만들기 
dtm <- youth_tf_idf %>%
  cast_dtm(document=no, 
           term=word, 
           value=n)

tm::inspect(dtm)

dtm_tf_idf <- youth_tf_idf %>%
  cast_dtm(document=no, 
           term=word, 
           value=tf_idf)

tm::inspect(dtm_tf_idf)











