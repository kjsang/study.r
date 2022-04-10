# 1. Libraries and Data Loading
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, kernlab, e1071, ROCR) # modeling packages
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
auto_split <- rsample::initial_split(autoparts,
                                     prop = 0.70,
                                     strata = y_faulty) # 클래스 값 기준으로 데이터 분할해주세욧
auto_train <- training(auto_split) # Analysis를 학습으로 적재
auto_test  <-  testing(auto_split) # Assess를 테스트로 적재
auto_train %>%  skimr::skim()

# 4. model 만들기
# e1071::tune.svm(y_faulty ~ ., data = auto_train,
#                 gamma = 2^(-1:1), # 자동으로 범위 내에서 파라미터를 다양하게 입력
#                 cost = 2^(2:4)) # 가장 좋은 결과를 산출하는 파라미처 찾기 
# # Parameter tuning of ‘svm’:
# 
# - sampling method: 10-fold cross validation 
# 
# - best parameters:
#  gamma cost
#    0.5    8
# 
# - best performance: 0.00977979 



# 4.1. 모델만들기 기초 (e1071 패키지로 만들자)

svm(y_faulty ~ ., data = auto_train,
    gamma = 0.5, cost = 8) -> m
# Call:
# svm(formula = y_faulty ~ ., data = auto_train, gamma = 0.5, cost = 8)
# 
# 
# Parameters:
#    SVM-Type:  C-classification 
#  SVM-Kernel:  radial 
#        cost:  8 
# 
# Number of Support Vectors:  862

svm(y_faulty ~ ., data = auto_train,
    gamma = 0.5, cost = 8) -> svm.model
svm.pred <- predict(svm.model, auto_test)
table(pred = svm.pred, true = auto_test$y_faulty) 
ROC(test = svm.pred, stat = auto_test$y_faulty, plot = "ROC", AUC = T, main = "SVM")

predict(svm.model, newdata = auto_test)

