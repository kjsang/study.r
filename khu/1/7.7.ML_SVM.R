# 1. Libraries and Data Loading
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, e1071, # helper packages
               tidymodels, kernlab) # modeling packages
read_csv("autoparts.csv") -> raw_autoparts

# 2. 데이터 살펴보기
raw_autoparts %>% skimr::skim()
raw_autoparts %>% glimpse()

# 3. 데이터 전처리
raw_autoparts %>% 
  filter(prod_no=="90784-76001") %>% 
  select(-prod_no) %>% 
  filter(c_thickness < 1000) %>% 
  mutate(y_faulty = ifelse(c_thickness < 20 | c_thickness > 32, 1, 0)) -> autoparts

# 1. Libraries and Data Loading
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, kernlab) # modeling packages
read_csv("autoparts.csv") -> raw_autoparts

# 2. 데이터 살펴보기
raw_autoparts %>% skimr::skim()
raw_autoparts %>% glimpse()

# 3. 데이터 전처리
raw_autoparts %>% 
  filter(prod_no=="90784-76001") %>% 
  select(-prod_no) %>% 
  filter(c_thickness < 1000) %>% 
  mutate(y_faulty = ifelse(c_thickness < 20 | c_thickness > 32, 1, 0)) %>% 
  mutate(y_faulty = as.factor(y_faulty)) -> autoparts

# 3.1. 데이터 분할
set.seed(123)
auto_split <- rsample::initial_split(autoparts, prop = 0.70, strata = y_faulty) # 클래스 값 기준으로 데이터 분할해주세욧
auto_train <- training(auto_split) # Analysis를 학습으로 적재
auto_test  <-  testing(auto_split) # Assess를 테스트로 적재
auto_train %>%  glimpse()

# 4. model 만들기
