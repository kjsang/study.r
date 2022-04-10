               
# 패키지 적재 ----------------------------------------------

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, rstatix, randomForest, randomForestExplainer,# modeling packages
               ggpubr) #  visualization


# 데이터 확인 및 정제 -----------------------------------------


read_csv("UniversalBank.csv") -> raw
raw %>% glimpse()
raw %<>%
  mutate(PersonalLoan = as.factor(PersonalLoan),
         PersonalLoan = ifelse(PersonalLoan == 1, "")) # 종속변수 팩터화

# 데이터 분할 ----------------------------------------------
set.seed(123)
raw.split <- rsample::initial_split(raw , strata = PersonalLoan,
                                    prop = 0.70)
raw.train <- training(raw.split) # Analysis를 학습으로 적재
raw.test  <-  testing(raw.split) # Assess를 테스트로 적재
raw.train %>%  skimr::skim()

# 훈련데이터 살펴보기
raw.train %>% 
  group_by(PersonalLoan) %>% 
  summarize(count = n())
# # A tibble: 2 x 2
#   PersonalLoan count
#   <fct>        <int>
# 1 0             3177
# 2 1              323


# 랜덤포레스트 with Tidymodels
parsnip::rand_forest(trees = 1000) %>% 
  set_mode("classification") %>% 
  set_engine("randomForest") %>% 
  fit(PersonalLoan ~ ., data = raw.train) -> raw.rf
raw.test %>% select(PersonalLoan) -> truth
# 결과 확인
raw.rf %>% predict(raw.test) %>% 
  bind_cols(truth)

raw.rf %>%
  predict(raw.test) %>%
  bind_cols(truth) %>%
  metrics(truth=PersonalLoan, estimate=.pred_class)
# # A tibble: 2 x 3
#   .metric  .estimator .estimate
#   <chr>    <chr>          <dbl>
# 1 accuracy binary         0.988
# 2 kap      binary         0.933

raw.rf %>%
  predict(raw.test) %>%
  bind_cols(truth) %>% 
  rename(obs = PersonalLoan,
         pred = .pred_class) %>% 
  conf_mat(truth = obs, estimate = pred)
 


# 혼동행렬 시각화
raw.rf %>%
  predict(raw.test) %>%
  bind_cols(truth) %>% 
  rename(obs = PersonalLoan,
         pred = .pred_class) %>% 
  conf_mat(obs, pred) %>% 
  ggplot2::autoplot(type = "heatmap")

two_class_example
