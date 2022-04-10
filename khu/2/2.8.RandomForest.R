if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, rstatix, randomForest,# modeling packages
               ggpubr #  visualization
               )
set.seed(123)
stagec %<>% 
  as_tibble() %>% 
  filter(!is.na(eet))
stagec.split <- rsample::initial_split(stagec , strata = ploidy, prop = 0.70)
stagec.train <- training(stagec.split) # Analysis를 학습으로 적재
stagec.test  <-  testing(stagec.split) # Assess를 테스트로 적재
stagec.train %>%  skimr::skim()
stagec.train
randomForest(ploidy ~ ., data = stagec.train, 
             ntree = 100, proximity = T) -> rf
rf
plot(rf)
# 
# Call:
#  randomForest(formula = ploidy ~ ., data = stagec.train, ntree = 100,      proximity = T) 
#                Type of random forest: classification
#                      Number of trees: 100
# No. of variables tried at each split: 2
# 
#         OOB estimate of  error rate: 7.59%
# Confusion matrix:
#            diploid tetraploid aneuploid class.error
# diploid         37          1         0  0.02631579
# tetraploid       1         36         0  0.02702703
# aneuploid        3          1         0  1.00000000

importance(rf)
varImpPlot(rf)

predict(rf, newdata = stagec.test)
