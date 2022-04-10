## 12.Topic분석_IDM(Intertopic Distance Map)
# https://ldavis.cpsievert.me/reviews/reviews.html

# install.packages('tm')
# install.packages('stringr')
# install.packages('topicmodels')
# install.packages('lda')
# install.packages('LDAvis')
# install.packages('servr')

# install.packages("devtools")
# devtools::install_github("cpsievert/LDAvisData")

library(tm)
library(stringr)
library(topicmodels)
library(lda)
library(LDAvis)
library(servr)





## 12.1 LDA분석용 포멧 만들기
## tidy text -> list로 변환

study_doc <- study_long %>% 
  group_by(구분) %>% 
  summarize(word = str_c(word, collapse = " ")) %>%
  ungroup()

## 토큰화
doc.list <- strsplit(study_doc$word, "[[:space:]]+")
head(doc.list)

## 용어 테이블 만들기
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

## 단어명 저장
vocab <- names(term.table)
head(vocab)

## LDA용 포멧으로 저장 
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)


# 모델 적합
# 분석에 필요한 통계값 생성:
# 문서당 토큰 수 ()
doc.length <- sapply(documents, function(x) sum(x[2, ]))  

# frequencies of terms in the corpus
term.frequency <- as.integer(term.table)  






## 12.2 LDA모형 분석
## 파라미터 설정

K <- 4
G <- 100
alpha <- 0.02
eta <- 0.02

# 모델 생성
set.seed(1234)

lda_model <- lda.collapsed.gibbs.sampler(documents = documents,
                                          K = K, 
                                          vocab = vocab, 
                                          num.iterations = G, 
                                          alpha = alpha, 
                                          eta = eta, 
                                          initial = NULL, 
                                          burnin = 0,
                                          compute.log.likelihood = TRUE)


# LDAvis 시각화
theta <- t(apply(lda_model$document_sums + alpha, 
                 2, 
                 function(x) x/sum(x)))
phi <- t(apply(t(lda_model$topics) + eta, 
               2, 
               function(x) x/sum(x)))






## 12.3 시각화(IDM: Intertopic Distance Map)
## 시각화 변수 세팅

#한글로 결과 보기
options(encoding = 'UTF-8') 

json <- createJSON(phi = phi, 
                   theta = theta, 
                   doc.length = doc.length, 
                   vocab = vocab, 
                   term.frequency = term.frequency, 
                   encoding='UTF-8')

## 시각화 프로그램
serVis(json, out.dir = 'vis2', open.browser = TRUE)






## 12.4 테이블 만들기
dim(lda_model$topics)  #5개 토픽에 1770개의 단어 출현   
top.topic.words(lda_model$topics) #5개 각 토픽 별 상위 20개 단어
lda_topics <- data.frame(top.topic.words(lda_model$topics))
head(lda_topics, 10)
  
  
  
  
  
  
  
  
  