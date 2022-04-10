## 06.한글 텍스트마이닝


# 1.기본 package 로드

## 1.1 KoNOP 로드
library(KoNLP)

useNIADic() # 단어사전 사용 - useSejongDic()

# 1.2 library 로드
library(tidyverse)
library(tidytext) 
library(tm)




# 2.데이터가져오기(tibble)
# 데이터 가져오기
bible_tb <-  readxl::read_xlsx('개역한글성경.xlsx')
bible_tb

bible_ct_tb <- readxl::read_xlsx('개역한글성경_구분.xlsx')
bible_ct_tb



# 성경명 연결(inner_join)

bible_tb <- bible_tb %>%
  inner_join(bible_ct_tb, by="구분") %>%
  mutate(no = 1:31102) %>%  
  select(no, 성경명, 장, 절, 본문) 

bible_tb

bible_tb <- bible_tb %>%
  filter(성경명 == "마태복음")

bible_tb

# 데이터 전처리

bible_tb <- bible_tb %>%
  mutate(본문 = gsub("[[:cntrl:]]", "", 본문)) %>% 
  mutate(본문 = gsub("[[:punct:]]", "", 본문)) 

bible_tb





# 3.한글 형태소로 분리 및 tidy 형식으로 변환

# 명사추출
bible_tb_n <- bible_tb %>%
  unnest_tokens(input = 본문,
                output = morp,
                token=SimplePos09) %>%        # 한글 형태소로 분리
  filter(str_detect(morp, "/n")) %>%          # 명사 추출
  mutate(word = str_remove(morp, "/.*$")) %>% # 형태소 정보 제거
  filter(str_length(word)>=2)

bible_tb_n

# 동사, 형용사 추출
bible_tb_p <- bible_tb %>%
  unnest_tokens(input = 본문,
                output = morp,
                token=SimplePos09) %>%        # 한글 형태소로 분리
  filter(str_detect(morp, "/p")) %>%          # 명사,동사, 형용사 추출
  mutate(word = str_replace(morp, "/.*$", "다")) %>% # 형태소 정보 제거 (어간에 다 붙여주자)
  filter(str_length(word)>=2)

bible_tb_p

# 통합
bible_tb <- bind_rows(bible_tb_n, bible_tb_p)
  
bible_tb





# 4.용어 정리
st_word <- tibble(word=c("너희", "우리", "무엇", "열네",
                         "하시")) # 불용어 추가

bible_tb <- bible_tb %>%
  anti_join(st_word, by="word")

bible_tb <- bible_tb %>%
  mutate(word = gsub("영접하", "영접", word)) %>%
  mutate(word = gsub("제자들|제자이", "제자", word)) %>%
  mutate(word = gsub("하나|하나님", "하나님", word))

  
bible_tb
  




## 5 단어빈도(n) 및 tfidf

## 5.1 tf_idf 값 도출
## count 순서가 중요
# 전체데이터를 통합분석하기 위해 통합번호를 document로 사용함(총 85개)
# document = 장(28)

bible_tf_idf <- bible_tb %>%
  count(장, word, sort = TRUE) %>% # 문서 -> 단어 순으로
  bind_tf_idf(word, 장, n)         # 단어 -> 문서 순으로

bible_tf_idf

# 5.2 log ratio 추가
# install.packages("tidylo")
library(tidylo)

bible_tf_idf <- bible_tf_idf %>% 
  bind_log_odds(set = 장, 
                feature = word, 
                n = n) %>%
  rename(log_odds = "log_odds_weighted")

bible_tf_idf

## 5.3 단어 중심으로 통합
bible_count <- 
  bible_tf_idf %>%
  group_by(word)%>%
  summarise(n = sum(n, na.rm = TRUE),
            tf_idf = sum(tf_idf, na.rm = TRUE),
            log_odds = sum(log_odds, na.rm = TRUE)) %>%
  arrange(desc(n)) %>%
  ungroup()

bible_count


# bible_count %>%
#   write_excel_csv("bible_count.xls")







