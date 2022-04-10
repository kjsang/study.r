## 03.tidy text(영어)
## 참고문헌: https://www.tidytextmining.com/tidytext.html

#######################################################
# tidyverse: tidy형식의 데이터를 처리하는데 필요한 툴 #
#            ggplot2, purrr, tibble  3.0.3,           #
#            dplyr, tidyr, stringr, readr, forcats    #
#            등 8개 패키지를 한 곳에 묶은 형태        #
# tidytext: tidy문서로 만드는 변환                    #
#           rlang, tibble, dplyr, stringr, hunspell,  #
#           generics,lifecycle, Matrix, tokenizers,   #
#           janeaustenr, purrr                        # 
# tm: 텍스트마이닝 도구                               #
# SnowballC: 어간추출 - 'S', 'C' 대문자               # 
#######################################################



# 1.기본 package 설정

## 1.1 package 설치
# install.packages("tidyverse")
# install.packages("tidytext") # tidy문서
# install.packages("tm")        # 텍스트마이닝 도구
# install.packages("SnowballC") # 어간추출 - 'S', 'C' 대문자

## 1.2 library 로드
library(tidyverse)
library(tidytext) 
library(tm)
library(SnowballC) # wordStem = 어간분리



# 2.데이터가져오기(tibble)
text_tb <- read_csv("text.csv")





# 3.tidy 형식으로 변환
text_tb <- text_tb %>%
  unnest_tokens(input=text, 
                output=word)

text_tb





# 4.용어(tocken) 만들기 (tokenization)
st_word <- tibble(word=c("aaa", "bbb")) # 불용어 추가

text_tb <- text_tb %>%
  anti_join(stop_words, st_word, by="word") %>%  # 불용어 사전+추가 불용어   삭제
  filter(!grepl(pattern="\\d+", word)) %>%       # //d+ = 숫자의 정규표현식
  mutate(word=wordStem(word))                    # mutate: dplyr함수로 열추가

text_tb





# 5 단어빈도(n) 및 tf_idf

# 5.1 tf_idf 값 도출
# 문서별 단어빈도 
# tf_idf는 문서가 중요한 분석요소일때 필요
text_tf_idf <- text_tb %>%
  count(doc, word, sort = TRUE) %>% # 문서 -> 단어 순으로
  bind_tf_idf(word, doc, n)         # 단어 -> 문서 순으로

text_tf_idf


## 5.2 단어 중심으로 통합
text_count <- text_tf_idf %>%
  group_by(word)%>%
  summarise(n = sum(n, na.rm = TRUE),
            tf_idf = sum(tf_idf, na.rm = TRUE)) %>%
  arrange(desc(n)) %>%
  ungroup()

text_count







