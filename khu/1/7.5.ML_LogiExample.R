# 1. Libraries and Data Loading
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, # modeling packages
               modeldata) # package including datasets

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
# 2. Generating train and test split
set.seed(123)
heart_split <- rsample::initial_split(heart,
                                      prop = 0.8,
                                      strata = AHD)
heart_train <- heart_split %>% training()
heart_test <- heart_split %>% testing()


# 3. Model Fitting/Training
fitted_logistic_model <- 
  parsnip::logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification") %>% 
  fit(AHD ~ ., data = heart_train)
tidy(fitted_logistic_model,
     exponentiate = TRUE,
     conf.level = 0.95) %>%  # Generate Summary Table
  filter(p.value < 0.05)

# 4. Model Evaluation on Test data Set
pred_class <- predict(fitted_logistic_model,
                    new_data = heart_test,
                    type = "class")
pred_class[1:5,]
# # A tibble: 5 x 1
#   .pred_class
#   <fct>      
# 1 No         
# 2 No         
# 3 Yes        
# 4 No         
# 5 No  
pred_proba <- predict(fitted_logistic_model,
                      new_data = heart_test,
                      type = "prob")
pred_proba[1:5,]
# # A tibble: 5 x 2
#   .pred_No .pred_Yes
#      <dbl>     <dbl>
# 1   0.777     0.223 
# 2   0.914     0.0864
# 3   0.0279    0.972 
# 4   0.963     0.0372
# 5   0.824     0.176 

heart_results <- heart_test %>%
  select(AHD) %>%
  bind_cols(pred_class, pred_proba)

# Confusion Matrix
yardstick::conf_mat(heart_results, truth = AHD,
                    estimate = .pred_class)
#           Truth
# Prediction No Yes
#        No  31   6
#        Yes  1  22
yardstick::accuracy(heart_results, truth = AHD,
                    estimate = .pred_class)
yardstick::sens(heart_results, truth = AHD,
                    estimate = .pred_class)
yardstick::spec(heart_results, truth = AHD,
                    estimate = .pred_class)

# 등등등... 엄청 많지만 생략

custom_metrics <- metric_set(accuracy, sens, spec, precision, recall, f_meas, kap, mcc)
custom_metrics(heart_results,
               truth = AHD,
               estimate = .pred_class)

# ROC-AUC
roc_auc(heart_results,
        truth = AHD,
        .pred_No)
heart_results %>% 
  roc_curve(truth = AHD, .pred_No) %>% 
  autoplot()
