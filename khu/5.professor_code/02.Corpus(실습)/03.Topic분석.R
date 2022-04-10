# 8.Topic분석 : LDA모형(토픽분석)
# install.packages("topicmodels") # 토픽분석을 위해 추가 설치
library(topicmodels)

## 8.1 LDA
lda <- LDA(dtm, k=5, control=list(seed=1234))

####### 에러발생 #########
# Error in LDA(dtm, control = list(seed = 1234), k = 5) : 
#  Each row of the input matrix needs to contain at least one non-zero entry
###############

## 에러 해결
# ui = unique(dtm$i)
# dtm = dtm[ui,]


## 8.2 특정 토픽에서 특정단어가 생성될 확률
dim(lda@beta)   # 토픽(2)X단어(5) 행렬 
head(lda@beta)  # 토픽X단어 행렬 내용

## 8.3 특정 문서가 특정 토픽을 반영할 확률
dim(lda@gamma)  # 문서(4)X토픽(2) 행렬의 구조
head(lda@gamma) # 문서X토픽 행렬 내용

## 8.4 잠재토픽으로 분류된 상위12개 단어구조  
terms(lda, 12)

