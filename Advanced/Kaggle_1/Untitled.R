# Loading Libraries
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, 
               xgboost, h2o, corrplot, rpart.plot, corrgram, lightgbm, ggplot2, highcharter, 
               ggthemes, psych, scales, treemap, treemapify, repr, cowplot, magrittr, ggpubr,
               RColorBrewer, plotrix, ggrepel, tidyverse, gridExtra, reshape2, janitor)

lst <- c(
  "tidyverse", "skimr", "GGally", "plotly", "viridis", "caret", "randomForest", "e1071", "rpart", 
  "xgboost", "h2o", "corrplot", "rpart.plot", "corrgram", "lightgbm", "ggplot2", "highcharter", 
  "ggthemes", "psych", "scales", "treemap", "treemapify", "repr", "cowplot", "magrittr", "ggpubr",
  "RColorBrewer", "plotrix", "ggrepel", "tidyverse", "gridExtra", "reshape2", "janitor", "descr", 
  "dplyr","boot","maps", "tidyquant","wesanderson"
)

as_tibble(installed.packages()) %>%
  select(Package, Version)  %>%
  filter(Package %in% lst)

df <- read_csv("real_estate_db.csv")
df %>% str()
df %>% summary()
df %>% 
  select_if(is.numeric) -> numerics
numerics %>%   
colnames()
# [1] "SUMLEVEL"                    "zip_code"                    "area_code"                  
# [4] "lat"                         "lng"                         "ALand"                      
# [7] "AWater"                      "pop"                         "male_pop"                   
# [10] "female_pop"                  "rent_mean"                   "rent_median"                
# [13] "rent_stdev"                  "rent_sample_weight"          "rent_samples"               
# [16] "rent_gt_10"                  "rent_gt_15"                  "rent_gt_20"                 
# [19] "rent_gt_25"                  "rent_gt_30"                  "rent_gt_35"                 
# [22] "rent_gt_40"                  "rent_gt_50"                  "universe_samples"           
# [25] "used_samples"                "hi_mean"                     "hi_median"                  
# [28] "hi_stdev"                    "hi_sample_weight"            "hi_samples"                 
# [31] "family_mean"                 "family_median"               "family_stdev"               
# [34] "family_sample_weight"        "family_samples"              "hc_mortgage_mean"           
# [37] "hc_mortgage_median"          "hc_mortgage_stdev"           "hc_mortgage_sample_weight"  
# [40] "hc_mortgage_samples"         "hc_mean"                     "hc_median"                  
# [43] "hc_stdev"                    "hc_samples"                  "hc_sample_weight"           
# [46] "home_equity_second_mortgage" "second_mortgage"             "home_equity"                
# [49] "debt"                        "second_mortgage_cdf"         "home_equity_cdf"            
# [52] "debt_cdf"                    "hs_degree"                   "hs_degree_male"             
# [55] "hs_degree_female"            "male_age_mean"               "male_age_median"            
# [58] "male_age_stdev"              "male_age_sample_weight"      "male_age_samples"           
# [61] "female_age_mean"             "female_age_median"           "female_age_stdev"           
# [64] "female_age_sample_weight"    "female_age_samples"          "pct_own"                    
# [67] "married"                     "married_snp"                 "separated"                  
# [70] "divorced"      

# What is the distribution of the family mean

fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}
fig(7,8) # 함수형태로 만들었다. 플롯 크기를 조정하는 역할이다. 

numerics %>% filter(!is.na(family_mean)) %>%
  summarize(mean=mean(family_mean), sd=sd(family_mean))

subset.rent <- numerics %>%
  filter(!is.na(rent_mean))

p1 <- ggplot(data=subset.rent, aes(x=rent_mean))+
  geom_histogram(aes(y=..density..), bins = 40, fill="#81F781")+
  stat_function(fun=dnorm, color="black",
                args=list(mean=mean(subset.rent$rent_mean), 
                          sd=sd(subset.rent$rent_mean))) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5)) + labs(title="Right Skewed Distribution", 
                                                   x="Rent Mean", y="Probability")


subset.female <- numerics %>%
  filter(!is.na(female_age_mean))