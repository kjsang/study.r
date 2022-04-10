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
study_tb <- read_csv('우수학습법.csv',                       # csv 데이터 읽어오기
                     col_names = TRUE,                       
                     locale=locale('ko', encoding='euc-kr'), # 한글 인코딩처리
                     na=".")
study_tb

# 데이터 통합(long형)
# 전체데이터를 통합분석하기 위해 통합번호를 document로 사용함(총 85개)
# document = no(85)
study_long <- study_tb %>%
  pivot_longer(c("학습방법", "소규모대학", "기독교대학", 
                 "학습전략", "기대효과"),
               names_to = "구분",
               values_to = "word") %>%
  mutate(no = 1:85)  # co-occur을 위해 17 -> 85개로 수정


  
study_long

# 데이터 전처리

study_long$word[2]

study_long <- study_long %>%
  mutate(word = gsub("[[:cntrl:]]", "", word)) %>%
  mutate(word = gsub("[[:punct:]]", "", word)) %>%
  mutate(word = gsub("KC 대학|우리 대학|KC대학교", "케이씨대학", word)) %>%
  mutate(word = gsub("코로나 19|코로나", "코로나19", word))

study_long$word[2]






# 3.한글 형태소로 분리 및 tidy 형식으로 변환
# ppt 설명(한글 형태소)
study_long <- study_long %>%
  unnest_tokens(input = word,
                output = morp,
                token=SimplePos09) %>%        # 한글 형태소로 분리
  filter(str_detect(morp, "/n")) %>%          # 명사만 추출
  mutate(word = str_remove(morp, "/.*$")) %>% # 형태소 정보 제거
  filter(str_length(word)>=2)

study_long






# 4.용어 정리
st_word <- tibble(word=c("때문", "번째")) # 불용어 추가

study_long <- study_long %>%
  anti_join(st_word, by="word")

study_long <- study_long %>%
  mutate(word = gsub("공부하|공부하기|공부할", "공부", word)) %>%
  mutate(word = gsub("학습할", "학습", word)) %>%
  mutate(word = gsub("학생들", "학생", word)) %>%
  mutate(word = gsub("교수님|교수들", "교수", word))
  
  
study_long
  




## 5 단어빈도(n) 및 tfidf

## 5.1 tf_idf 값 도출
## count 순서가 중요
# 전체데이터를 통합분석하기 위해 통합번호를 document로 사용함(총 85개)
# document = no(85)

study_tf_idf <- study_long %>%
  count(no, word, sort = TRUE) %>% # 문서 -> 단어 순으로
  bind_tf_idf(word, no, n)         # 단어 -> 문서 순으로

study_tf_idf

# 5.2 log ratio 추가
# install.packages("tidylo")
library(tidylo)

study_tf_idf <- study_tf_idf %>% 
  bind_log_odds(set = no, 
                feature = word, 
                n = n) %>%
  rename(log_odds = "log_odds_weighted")

study_tf_idf

## 5.3 단어 중심으로 통합
study_count <- 
  study_tf_idf %>%
  group_by(word)%>%
  summarise(n = sum(n, na.rm = TRUE),
            tf_idf = sum(tf_idf, na.rm = TRUE),
            log_odds = sum(log_odds, na.rm = TRUE)) %>%
  arrange(desc(n)) %>%
  ungroup()

study_count


# study_count %>%
#   write_excel_csv("study_count.xls")







