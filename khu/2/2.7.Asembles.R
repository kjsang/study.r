if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, rstatix, adabag, neuralnet, nnet, keras,# modeling packages
               ggpubr #  visualization
               )
# 데이터 확인 ----------------------------------------------
iris %>% glimpse()


# 배깅 --------------------------------------------------
bagging(Species ~ ., data = iris, mfinal = 10) -> iris.bagging
iris.bagging$importance
# Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
#     74.22158     25.77842      0.00000      0.00000 
iris.bagging$trees[[10]] %>% plot()
iris.bagging$trees[[10]] %>% text()
iris.bagging %>% predict(newdata = iris) -> iris.bagging.pred
iris.bagging.pred$class %>% table(iris[,5])


# 부스팅 -------------------------------------------------
boosting(Species ~ ., data = iris, boos = T, mfinal = 10) -> iris.boost
iris.boost$importance
# Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
#    59.654175    24.152336     5.116672    11.076817 
iris.boost$tree[[10]] %>% plot()
iris.boost$tree[[10]] %>% text()
iris.boost %>% predict(newdata = iris) -> iris.boost.pred
iris.boost.pred$class %>% table(iris[,5])


# 신경망 -------------------------------------------------
iris %>% 
  filter(Species != "setosa") %>% 
  mutate(Species = as.numeric(Species) - 2) %>% 
  mutate(Species = as.factor(Species)) -> iris.binary
iris.binary
iris.split <- rsample::initial_split(iris.binary,prop = 0.60)
iris.train <- training(iris.split) # Analysis를 학습으로 적재
iris.test  <-  testing(iris.split) # Assess를 테스트로 적재
iris.train %>%  skimr::skim()

recipe(Species ~ ., data = iris.train) %>% 
  step_BoxCox(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  prep(training = iris.train, retain = T) -> iris.rec
bake(iris.rec, new_data = iris.test, all_predictors()) -> test_normalized
library(text)
# reticulate::install_miniconda()
# reticulate::conda_install(envname = 'r-reticulate', c('torch==0.4.1', 'transformers==3.3.1', 'numpy', 'nltk'), pip = TRUE)


set.seed(123)
iris.fit <-
  mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>% 
  set_mode("classification") %>% 
  set_engine("keras", verbose = 0) %>% 
  fit(Species ~ ., data = bake(iris.rec, new_data = NULL))

iris.test %>% 
  bind_cols(
    predict(iris.fit, new_data = test_normalized),
    predict(iris.fit, new_data = test_normalized, type = "prob")
  ) -> iris.result
