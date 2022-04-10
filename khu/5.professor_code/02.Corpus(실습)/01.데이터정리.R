## 02.corpus(실습)

## youth_guarantee 데이터 실습

# 1.기본 package 설정

## 1.1 package 설치

# install.packages("tm") # 텍스트마이닝 도구
# install.packages("SnowballC") # 불용어 처리
# install.packages("dplyr") 

## 1.2 library 로드
library(tm)
library(SnowballC)
library(dplyr)





# 2.텍스트 가져오기(corpus)

## 2.1 데이터가져오기
youth_df <- read.csv("youth_guarantee.csv", 
                    stringsAsFactors=F, 
                    header=TRUE,
                    sep=",")
str(youth_df)

## 2.2 corpus(말뭉치)로 저장: 코퍼스는 문서가 저장되어 있는 공간
corp <- youth_df$text  %>%
  tm::VectorSource() %>%
  tm::Corpus()                      # C는 대문자

writeLines(as.character(corp[[1]])) # 1번 문서 내용 확인






# 3 용어(tocken) 만들기 (tokenization)
corp <- corp %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("english"))

writeLines(as.character(corp[[1]])) # 1번 문서 내용 확인





# 4. 불용어(stopwords) 처리

# 제거해 주고 싶은 단어가 있으면 아래의 removeWords를 이용해서 제거
st_word <- c(stopwords("english"),"and", "will", "also") 
corp <- tm_map(corp, removeWords, st_word) 

# 5. 어근 동일화(stemming)
corp <- tm_map(corp, stemDocument)

writeLines(as.character(corp[[1]])) # 1번 문서 내용 확인




# 6.단어-문서 행렬 만들기

## 6.1 tdm(Term-Document-Matrix 만들기): 단어중심일때
tdm <- TermDocumentMatrix(corp)

inspect(tdm)

tdm_tf_idf <- weightTfIdf(tdm)
inspect(tdm_tf_idf)

## 6.2 dtm(Document-Term-Matrix 만들기): 문서 중심일때 
dtm <- DocumentTermMatrix(corp)
inspect(dtm)

dtm_tf_idf <- weightTfIdf(dtm)
inspect(dtm_tf_idf)






