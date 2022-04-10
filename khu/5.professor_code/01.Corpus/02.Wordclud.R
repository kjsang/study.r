# 7.Word cloud

## 7.1 Frequence Table 만들기

## 행렬로 전환
tdm_m <- as.matrix(tdm)

## 단어별 합계 구하기
tdm_sum <- sort(rowSums(tdm_m), 
                decreasing = TRUE) # 단어가 나타난 숫자별로 정렬

## 데이터 프레임으로 저장
tdm_df <- data.frame(word=names(tdm_sum),
                     freq=tdm_sum) # data.frame으로 변환
head(tdm_df,10)


library(wordcloud)
wordcloud(words=tdm_df$word, 
          freq=tdm_df$freq)
