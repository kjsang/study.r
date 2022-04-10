
if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr, wordcloud,
  KoNLP, caret, tm
  )

tinytex::install_tinytex()
data("crude")
data("acq")
install.packages("reticulate")

to_dtm <- function(corpus){
  x <- tm_map(corpus, removePunctuation)
  x <- tm_map(x, removeWords, stopwords())
  return(DocumentTermMatrix(x))
}



crude_acq <- c(to_dtm(crude), to_dtm(acq))
crude_acq_df <- cbind(as.data.frame(as.matrix(crude_acq)),
                      LABEL=c(rep("crude", length(crude)),
                             rep("acq", length(acq))))
head(crude_acq_df[, c(1:5, length(crude_acq_df))])
train_idx <- createDataPartition(crude_acq_df$LABEL, p = 0.8)$Resample1
crude_acq.train <- crude_acq_df[train_idx,]
crude_acq.text <- crude_acq_df[-train_idx,]

tibble(
 body = c("오늘은 엑셀을 사용한 첫 날이다. 엑셀은 참 편하다",
           "컴퓨터가 고장났다!!! 컴퓨터가 고장나면 나는 엑셀을 할 수 없다.",
           "1년만에 일기를 다시 쓴다. 방가방가~~",
           "엑셀로 쓰는 일기는 우리의 생활을 풍요롭게 한다"),
 tag = c("diary1", "diary1", "diary2", "diary2")) -> docs
docs %>% 
  mutate(doc_id = 1:length(body),
         text = body) %>% 
  select(doc_id, text, tag) -> docs_corpus
docs_corpus %>% 
  as.data.frame() %>% 
  DataframeSource() %>% 
  Corpus() %>% 
  DocumentTermMatrix() %>% 
  tidy()


# 코퍼스 -------------------------------------------------
useSejongDic()
## 말뭉치 구성 ##
## 2019-0731로부터 10일 이전 동안의 랭킹뉴스 데이터들의 말뭉치 구성

mytextlocation <- "newsdata"
dir("newsdata")[1:11] #디렉토리 구조 파악

#VCorpus : 메모리에 저장되는 코퍼스 생성 함수
#DirSource : 디렉토리에 있는 확장명이 csv인 파일들을 코퍼스로 생성하기 위한 소스로 지정

mynewsdata <- VCorpus(DirSource(mytextlocation, pattern = "csv"))
mynewsdata

#tm 코퍼스의 구조 확인
str(mynewsdata[[1]]) # 문서별로 content(문서의 내용), meta(메타정보)요소를 가지는 리스트 구성

# 문서의 내용 추출 함수 content()
content(mynewsdata[[1]])

## 개별 문서 텍스트 데이터 사전처리 ##
# 말뭉치 데이터에서 하나의 문서만 우선 선택하여 처리

mynews <- mynewsdata[[1]]$content

# 영문표현 삭제
library(stringr)
mynews %>%
  str_replace_all("[[:lower:]]", "") %>%
  str_replace_all("[[:upper:]]", "") %>%
  str_replace_all("[^[:alpha:][:digit:]]", " ") %>%  #특수문자 제거
   str_replace_all("[一-龥]", "") %>%   #한자제거
  str_replace_all("[[:digit:]]", "") %>%  #숫자제거
  str_replace_all("\\c", "") %>%  #c제거
  str_replace_all("\"", "") %>%  #\제거
  str_trim() -> mytext
mytext %>% 
  KoSpacing::spacing()
library(KoSpacing)
reticulate::install_miniconda()
library(multilinguer)
install.packages("multilinguer")
install_miniconda()
has_conda()
set_env()
mytext %>% 
  as_tibble() %>% 
  rename(text = value) %>% 
  mutate(id = 1:length(text)) %>% 
  select(id, text) %>% 
  mutate(word = text %>% 
           extractNoun() %>% 
           lapply(unique)) %>% 
  unnest(word) %>% 
  select(id, word) %>% 
  count(word) %>% 
  arrange(desc(n))
