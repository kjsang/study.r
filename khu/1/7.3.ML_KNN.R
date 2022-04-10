# 0. packages and data loading

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels,caret, # modeling packages
               kknn,# engine package
               dotwhisker, # visualizing
               modeldata) # package including datasets
read_csv("wine.csv") %>% 
  as_tibble() %>% 
  mutate(Class = as.factor(Class)) -> raw_wine # Class를 factor로 변환

# 1. 데이터 분할
set.seed(123)
wine_split <- rsample::initial_split(raw_wine, prop = 0.80, strata = Class) # 클래스 값 기준으로 데이터 분할해주세욧
wine_train <- training(wine_split) # Analysis를 학습으로 적재
wine_test  <-  testing(wine_split) # Assess를 테스트로 적재
wine_train %>%  glimpse()
# [1] 141  14

# 2. 데이터 살펴보기
wine_train %>% 
  ggplot(aes(x = Alcohol, y = Acid, col = Class)) +
  geom_point(alpha = .2)

# 적절한 K 탐색
knn_mod <-
  nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification") %>% 
  translate()
knn_mod %>% 
  fit(Class ~ .,
      data = wine_test) -> knn_fit
knn_fit
# parsnip model object
# 
# Fit time:  4ms 
# 
# Call:
# kknn::train.kknn(formula = Class ~ ., data = data, ks = min_rows(5,     data, 5))
# 
# Type of response variable: nominal
# Minimal misclassification: 0.05405405
# Best kernel: optimal
# Best k: 5


# 3. 튜닝

tune <- caret::trainControl(
  method = "repeatedcv", # cross-validation 반복
  number = 10, # 훈련데이터의 fold 수
  repeats = 5 # cv 반복횟수
)
model1 <- caret::train(Class ~ ., data = wine_train,
                      method = "knn",
                      trControl = tune)
predict(model, newdata = wine_test) -> predict_wine
caret::confusionMatrix(predict_wine, wine_test$Class)
