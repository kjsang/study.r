# 1. Libraries and Data Loading
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, discrim, klaR, # modeling packages
               caret, e1071, pROC) # 일단 설치하래서 하는 패키지

# 2. 데이터 살펴보기 및 변수 정리
readr::read_csv("Heart.csv") -> heart
heart %>% glimpse()
heart %<>% 
  select(-X1) 
head(heart)
heart %<>% 
  na.omit() # 결측값을 제거해라
heart %<>% 
  mutate(ChestPain = as.factor(ChestPain),
         Thal = as.factor(Thal),
         AHD = as.factor(AHD))
heart %>%  skimr::skim() # 개좋네 ㅋㅋ
# ── Data Summary ────────────────────────
#                            Values    
# Name                       Piped data
# Number of rows             297       
# Number of columns          14        
# _______________________              
# Column type frequency:               
#   factor                   3         
#   numeric                  11        
# ________________________             
# Group variables            None      
# 
# ── Variable type: factor ────────────────────────────────
#   skim_variable n_missing complete_rate ordered n_unique
# 1 ChestPain             0             1 FALSE          4
# 2 Thal                  0             1 FALSE          3
# 3 AHD                   0             1 FALSE          2
#   top_counts                         
# 1 asy: 142, non: 83, non: 49, typ: 23
# 2 nor: 164, rev: 115, fix: 18        
# 3 No: 160, Yes: 137                  
# 
# ── Variable type: numeric ───────────────────────────────
#    skim_variable n_missing complete_rate    mean     sd
#  1 Age                   0             1  54.5    9.05 
#  2 Sex                   0             1   0.677  0.468
#  3 RestBP                0             1 132.    17.8  
#  4 Chol                  0             1 247.    52.0  
#  5 Fbs                   0             1   0.145  0.352
#  6 RestECG               0             1   0.997  0.995
#  7 MaxHR                 0             1 150.    22.9  
#  8 ExAng                 0             1   0.327  0.470
#  9 Oldpeak               0             1   1.06   1.17 
# 10 Slope                 0             1   1.60   0.618
# 11 Ca                    0             1   0.677  0.939
#       p0   p25   p50   p75  p100 hist 
#  1    29    48  56    61    77   ▁▅▇▇▁
#  2     0     0   1     1     1   ▃▁▁▁▇
#  3    94   120 130   140   200   ▃▇▅▁▁
#  4   126   211 243   276   564   ▃▇▂▁▁
#  5     0     0   0     0     1   ▇▁▁▁▂
#  6     0     0   1     2     2   ▇▁▁▁▇
#  7    71   133 153   166   202   ▁▂▅▇▂
#  8     0     0   0     1     1   ▇▁▁▁▃
#  9     0     0   0.8   1.6   6.2 ▇▂▁▁▁
# 10     1     1   2     2     3   ▇▁▇▁▁
# 11     0     0   0     1     3   ▇▃▁▂▁

heart %>% 
  group_by(AHD) %>% 
  skimr::skim()
# ── Data Summary ────────────────────────
#                            Values    
# Name                       Piped data
# Number of rows             297       
# Number of columns          14        
# _______________________              
# Column type frequency:               
#   factor                   2         
#   numeric                  11        
# ________________________             
# Group variables            AHD       
# 
# ── Variable type: factor ─────────────────────────────────────────────────────────────────────────────────────────────────
#   skim_variable AHD   n_missing complete_rate ordered n_unique top_counts                        
# 1 ChestPain     No            0             1 FALSE          4 non: 65, non: 40, asy: 39, typ: 16
# 2 ChestPain     Yes           0             1 FALSE          4 asy: 103, non: 18, non: 9, typ: 7 
# 3 Thal          No            0             1 FALSE          3 nor: 127, rev: 27, fix: 6         
# 4 Thal          Yes           0             1 FALSE          3 rev: 88, nor: 37, fix: 12         
# 
# ── Variable type: numeric ────────────────────────────────────────────────────────────────────────────────────────────────
#    skim_variable AHD   n_missing complete_rate    mean     sd    p0   p25   p50   p75  p100 hist 
#  1 Age           No            0             1  52.6    9.55     29  44.8  52    59    76   ▁▆▇▅▂
#  2 Age           Yes           0             1  56.8    7.90     35  53    58    62    77   ▁▃▇▅▁
#  3 Sex           No            0             1   0.556  0.498     0   0     1     1     1   ▆▁▁▁▇
#  4 Sex           Yes           0             1   0.818  0.388     0   1     1     1     1   ▂▁▁▁▇
#  5 RestBP        No            0             1 129.    16.4      94 120   130   140   180   ▃▆▇▂▁
#  6 RestBP        Yes           0             1 135.    18.9     100 120   130   145   200   ▅▇▃▁▁
#  7 Chol          No            0             1 243.    53.8     126 209.  236.  268.  564   ▅▇▂▁▁
#  8 Chol          Yes           0             1 252.    49.7     131 218   253   284   409   ▂▇▇▃▁
#  9 Fbs           No            0             1   0.144  0.352     0   0     0     0     1   ▇▁▁▁▂
# 10 Fbs           Yes           0             1   0.146  0.354     0   0     0     0     1   ▇▁▁▁▂
# 11 RestECG       No            0             1   0.844  0.988     0   0     0     2     2   ▇▁▁▁▆
# 12 RestECG       Yes           0             1   1.18   0.977     0   0     2     2     2   ▆▁▁▁▇
# 13 MaxHR         No            0             1 159.    19.0      96 149   161   172   202   ▁▂▆▇▂
# 14 MaxHR         Yes           0             1 139.    22.7      71 125   142   157   195   ▁▃▇▇▂
# 15 ExAng         No            0             1   0.144  0.352     0   0     0     0     1   ▇▁▁▁▂
# 16 ExAng         Yes           0             1   0.540  0.500     0   0     1     1     1   ▇▁▁▁▇
# 17 Oldpeak       No            0             1   0.599  0.787     0   0     0.2   1.1   4.2 ▇▂▁▁▁
# 18 Oldpeak       Yes           0             1   1.59   1.31      0   0.6   1.4   2.5   6.2 ▇▅▃▁▁
# 19 Slope         No            0             1   1.41   0.598     1   1     1     2     3   ▇▁▃▁▁
# 20 Slope         Yes           0             1   1.82   0.567     1   1     2     2     3   ▃▁▇▁▁
# 21 Ca            No            0             1   0.275  0.634     0   0     0     0     3   ▇▁▁▁▁
# 22 Ca            Yes           0             1   1.15   1.02      0   0     1     2     3   ▇▇▁▆▃

set.seed(123)
heart_split <- rsample::initial_split(heart,
                                      prop = 0.8,
                                      strata = AHD)
heart_train <- heart_split %>% training()
heart_test <- heart_split %>% testing()

args(naive_Bayes) # 나이브베이즈 모델 인자 확인하는 방법
# function (mode = "classification", smoothness = NULL, Laplace = NULL)
# 나중에 Laplace = 1 로 설정하거나 smoothness를 변경하여 모델을 여러 번 돌릴 수 있음


bayes_model <- 
  naive_Bayes() %>%  # 사용 모델
  set_engine("klaR") # 사용 엔진 + 모드 설정도 있는데 베이즈 모델은 classification으로 고정

# recipe 만들기
# step_dummy(all_nominal(), -all_outcomes()) : one-hot-ecoding
# step_log(Gr_Liv_Area, base = 10) : 로그함수로 변환
# step_other(Neighborhood, threshold = 0.01) : 값이 적은 항목을 기타로 변환
# step_upsample(Flight_Status) # 데이터 균형화
# step_zv(all_predictors()) : 단일 고유 값 (예 : 모두 0) 변수 제거. 
# 특히, penalty 사용하는 모델에서 중요(logistic, SVM 등)
# step_normalize(all_numeric()) : 데이터 정규화

bayes_recipe <- 
  recipe(AHD ~ ., data = heart_train)
summary(bayes_recipe)
# # A tibble: 14 x 4
#    variable  type    role      source  
#    <chr>     <chr>   <chr>     <chr>   
#  1 Age       numeric predictor original
#  2 Sex       numeric predictor original
#  3 ChestPain nominal predictor original
#  4 RestBP    numeric predictor original
#  5 Chol      numeric predictor original
#  6 Fbs       numeric predictor original
#  7 RestECG   numeric predictor original
#  8 MaxHR     numeric predictor original
#  9 ExAng     numeric predictor original
# 10 Oldpeak   numeric predictor original
# 11 Slope     numeric predictor original
# 12 Ca        numeric predictor original
# 13 Thal      nominal predictor original
# 14 AHD       nominal outcome   original

bayes_workflow <- 
  workflow() %>% 
  add_model(bayes_model) %>% 
  add_recipe(bayes_recipe)
bayes_workflow # 베이즈 모델을 사용하여 위에서 만든 레시피를 탑재하고 분석을 실시하라는 명령

# 훈련데이터로 모델 훈련하기
bayes_train_fit <- 
  bayes_workflow %>%
  fit(data = heart_train)

# 모델 훈련 결과 확인
bayes_train_fit %>%
  pull_workflow_fit()

## 08.훈련모델 검정

# 예측결과표 생성

bayes_train_pred <- 
  predict(bayes_train_fit, 
          heart_train, 
          type = "prob") %>%
  bind_cols(predict(bayes_train_fit, 
                    heart_train)) %>% 
  bind_cols(heart_train %>% dplyr::select(AHD)) %>%
  print()

# 정오분류표(confusion matrix) 만들기

bayes_train_conf <-
  bayes_train_pred  %>%
  conf_mat(truth = AHD, 
           estimate = .pred_class)

bayes_train_conf

autoplot(bayes_train_conf, type = "heatmap") # mosaic 여기는 안해도 된다 
autoplot(bayes_train_conf, type = "mosaic")

summary(bayes_train_conf)

# f1: 재현율(Recall)(↑)과 정밀도(Precision)(↑)
# 재현율(Recall): 실제 Class 중에 잘 맞춘 것(=TPR=민감도)
# 정밀도(Precision): 예측 Class 중에 잘 맞춘 것
# 정확도 (Accuracy) : 클래스 0과 1 모두를 정확하게 분류

# ACU(area under the curve): ROC 정확도

bayes_train_pred %>%
  roc_auc(truth = AHD, 
          .pred_No)

# ROC 커브

train_auc <-
  bayes_train_pred %>%
  roc_curve(truth = AHD, 
            estimate = .pred_No) %>% 
  mutate(model = "train_auc")

autoplot(train_auc)

# 아래에서 gain과 lift 커브는 설명을 충분히 할 수 있도록 공부하자. 
# 그냥 그리는 것은 의미가 없을 것이다. 

# gain 커브
bayes_train_pred %>%
  gain_curve(truth = AHD, 
             estimate = .pred_No) %>%
  autoplot()

# lift 커브
bayes_train_pred %>%
  lift_curve(truth = AHD, 
             estimate = .pred_No) %>%
  autoplot()

# 테스트데이터 검정은 따로 해보자 ... 노션에서 봅시다 