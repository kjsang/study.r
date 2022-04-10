# 1. Libraries and Data Loading
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, kernlab, e1071, ROCR, rstatix) # modeling packages
read_csv("autoparts.csv") -> raw_autoparts
# 2. 데이터 살펴보기
raw_autoparts %>% skimr::skim()
raw_autoparts %>% glimpse()

# 3. 데이터 전처리
raw_autoparts %>% 
  filter(prod_no=="90784-76001") %>% 
  select(-prod_no) %>% 
  filter(c_thickness < 1000) %>% 
  filter(highpressure_time != 65534) -> autoparts
autoparts %>% rstatix::get_summary_stats()

# 3.1. 데이터 분할
 


svm(c_thickness ~ .,
    data = auto_train,
    gamma = 0.5,
    cost = 8) -> svm.model
svr.pred <- predict(svm.model, auto_test)
plot(x = auto_test$c_thickness,
     y = svr.pred,
     main = "SVR")
mse <- mean((svr.pred - auto_test$c_thickness) ^ 2)
mse

svr.predz


# 라벨달기 --------------------------------------------------------------------



# 선형회귀와 비교
lm(c_thickness ~ ., data = auto_train) %>% 
  summary()
# Call:
# lm(formula = c_thickness ~ ., data = auto_train)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -21.6595  -0.6187  -0.0255   0.5621  29.7904 
# 
# Coefficients:
#                     Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)        7.178e+02  4.019e+00  178.610   <2e-16 ***
# fix_time           5.027e-02  6.088e-03    8.258   <2e-16 ***
# a_speed           -1.801e+01  5.037e-01  -35.757   <2e-16 ***
# b_speed            1.993e+00  1.818e-01   10.964   <2e-16 ***
# separation        -7.619e-01  4.338e-03 -175.638   <2e-16 ***
# s_separation      -7.493e-01  4.384e-03 -170.914   <2e-16 ***
# rate_terms         1.000e-02  4.320e-03    2.315   0.0206 *  
# mpa               -1.486e-01  1.733e-03  -85.739   <2e-16 ***
# load_time         -1.480e-01  9.728e-03  -15.210   <2e-16 ***
# highpressure_time  1.151e-05  1.240e-05    0.928   0.3534    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.801 on 15226 degrees of freedom
# Multiple R-squared:  0.786,	Adjusted R-squared:  0.7859 
# F-statistic:  6214 on 9 and 15226 DF,  p-value: < 2.2e-16

lm(c_thickness ~ ., data = auto_train) -> lm.model?
predict(lm.model, auto_test) -> lm.pred
plot(x = auto_test$c_thickness, y = lm.pred)
mse <- mean((lm.pred - auto_test$c_thickness)^2) ; mse


score <- c(2,3,4,5,6)
score %<>% + 2

library(KoNLP)
