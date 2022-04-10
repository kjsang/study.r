# 0. packages and data loading

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels,caret, # modeling packages
               kknn)# engine package
read_csv("Heart.csv") -> heart

# 1. 데이터 살펴본 후 전처리
heart %>% glimpse()
heart %<>% select(-X1) # 연번 변수를 삭제해주자
heart %<>% 
  na.omit() %>% #  결측치 제거한 수
  select(-c(ChestPain, Thal)) %>%  # 명목변수 제거
  mutate(AHD = as.factor(AHD))
# 2. 데이터 분할
set.seed(123)
heart_split <- rsample::initial_split(heart, prop = 0.80,
                                      strata = AHD)
heart_train <- training(heart_split) # Analysis를 학습으로 적재
heart_test  <-  testing(heart_split) # Assess를 테스트로 적재
dim(heart_train)

fitted_logistic_model<- logistic_reg() %>%
        # Set the engine
        set_engine("glm") %>%
        # Set the mode
        set_mode("classification") %>%
        # Fit the model
        fit(AHD~., data = heart_train)
tidy(fitted_logistic_model)    # Generate Summary Table

tidy(fitted_logistic_model, exponentiate = TRUE) %>% 
  filter(p.value < 0.05)
