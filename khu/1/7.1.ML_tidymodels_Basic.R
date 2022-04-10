# 0. packages and data loading

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, # modeling packages
               modeldata) # package including datasets
data(ames, package = "modeldata") # 데이터 적재
tidymodels_prefer() # tidymodels 함수 우선 사용

# 1. 주요변수 탐색
ames %>% 
  ggplot(aes(x = Sale_Price)) + # Sale_Price를 살펴보자
  geom_histogram(bins = 30, # 히스토그램의 간격 조정
                 col = "orange", fill = "yellow") # 슥삭 꾸며주기
# 그림을 살펴보면 데이터가 right-skewed 되어있다. 저렴한 집이 너무 많다 이거다. 이를 조정해주기 위해 로그를 취해준다. 

ames %>% 
  ggplot(aes(x = Sale_Price)) +
  geom_histogram(bins = 30) +
  scale_x_log10() # 스캐일에 로그
ames %<>% 
  mutate(Sale_Price = log10(Sale_Price)) # 판매량에 아예 로그를 씌워주자

# 2. 데이터 전처리
# 2.1. 데이터 분할
set.seed(123)
ames_split <- rsample::initial_split(ames, prop = 0.80, strata = Sale_Price) # 집값 기준으로 데이터 분할해주세욧
ames_train <- training(ames_split) # Analysis를 학습으로 적재
ames_test  <-  testing(ames_split) # Assess를 테스트로 적재
dim(ames_train)
# [1] 2342   74
