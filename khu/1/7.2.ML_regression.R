# 0. packages and data loading

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels,caret, # modeling packages
               kknn,
               modeldata) # package including datasets
ggplot2::theme_set((theme_grey(base_family='NanumGothic')))

#### KNN-clustering with caret ####
caret::trainControl(
  method = "repeatedcv", # cross-validation 반복
  number = 10, # 훈련데이터의 fold 수
  repeats = 5 # cv 반복횟수
)

#### KNN-clustering with tidymodels ####
nearest_neighbor(
  # mode: A single character string for the type of model. Possible values for this model are "unknown", "regression", or "classification".
  mode = "unknown",
  # neighbors: A single integer for the number of neighbors to consider (often called k). For kknn, a value of 5 is used if neighbors is not specified.
  neighbors = NULL,
  # weight_func :	A single character for the type of kernel function used to weight distances between samples. Valid choices are: "rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", or "optimal".
  weight_func = NULL,
  # dist_power : A single number for the parameter used in calculating Minkowski distance.
  dist_power = NULL
)

nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("regression") %>% 
  translate()

nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification") %>% 
  translate()

#### example ####
knn_mod <-
  nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("regression") %>% 
  translate()
knn_mod %>% 
  fit(pecies ~ Sepal.Length + Sepal.Width,
      data = iris)

iris %<>% 
  as_tibble()
