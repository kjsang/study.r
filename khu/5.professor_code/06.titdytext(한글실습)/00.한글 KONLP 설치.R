## 05.한글 KONLP 설치

## https://mrchypark.github.io/post/KoNLP-%EC%84%A4%EC%B9%98-%EB%B0%A9%EB%B2%95/

#######################################################
# 1. KONLP (한국어 사전) 설치                         #
# jdk 설치: rJava, multilinguer                       #
# KoNLP 설치(github이용)                              #
#                                                     #
# 2. 텍스트마이닝 분석툴 설치                         #
# tidyverse: tidy형식의 데이터를 처리하는데 필요한 툴 #
#            ggplot2, purrr, tibble  3.0.3,           #
#            dplyr, tidyr, stringr, readr, forcats    #
#            등 8개 패키지를 한 곳에 묶은 형태        #
# tidytext: tidy문서로 만드는 변환                    #
#           rlang, tibble, dplyr, stringr, hunspell,  #
#           generics,lifecycle, Matrix, tokenizers,   #
#           janeaustenr, purrr                        # 
# tm: 텍스트마이닝 도구                               #
#######################################################


# 1.기본 package 설정

## 1.1 package 설치

## 1.1.1 jdk 설치
install.packages("rJava")
install.packages("multilinguer")

library(rJava)
library(multilinguer)
install_jdk()          # Java jdk 설치

rJava::.jinit()        # jdk 설치 확인 (0이면 설치완료)      



## 1.1.2 KoNLP 설치(github이용)
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', 
                        upgrade = "never", 
                        INSTALL_opts=c("--no-multiarch"))

# 2번째 방법
# install.packages('KoNLP', 
#                  repos = 'https://forkonlp.r-universe.dev')




## 1.2 텍스트 마이닝 패키지 설치
# tidytext: tidy문서로 만드는 변환
# tidyverse: tibble, tidyr, dplyr, stringr, ggplot2, forcats,  purrr, readr   
# 등 8개 패키지를 한 곳에 묶은 형태

# install.packages("tidytext") # tidy문서로 만드는 
# install.packages("tidyverse")


