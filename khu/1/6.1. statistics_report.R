###############################
#### 2. 영어성적 향상 분석 ####
###############################
# A학교의 전교생 중 표본을 추출하여 1학기와 2학기의 영어 성적을 측정했다. 1학기에 비해서 2학기에 학생들의 영어 성적이 향상됐는지 확인하고자 한다. 해당 데이터는 같은 표본 내에서 차이를 검증할 수 있는 대응 표본 t-test 과정을 사용할 수 있다. 

#### 2.0. 패키지 로드 ####
pacman::p_load(tidyverse, infer, tidyr, tidymodels, MASS, gginference, rstatix, 
               ggpubr, magrittr)

#### 2.1. 데이터 불러오기 ####
read_csv("score2.csv") -> raw2

#### 2.2. 데이터 살펴보기 ####
raw2 %>% 
  rstatix::get_summary_stats()
# # A tibble: 2 x 13
#   variable     n   min   max median    q1    q3   iqr   mad  mean    sd    se    ci
#   <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 after       27     3     5      5     4     5     1  0     4.48 0.643 0.124 0.254
# 2 before      27     1     5      4     4     5     1  1.48  4.15 1.03  0.198 0.406

#### 2.3. 데이터 전처리 ####
raw2 %>% 
  summarise(group_NA = sum(is.na(after)),
            score_NA = sum(is.na(before)))
# # A tibble: 1 x 2
#   group_NA score_NA
#      <int>    <int>
# 1        0        0

#### 2.4. 데이터 분석 ####

# 정규성 검정
models  <- lm(score ~ test, data = raw2)
ggqqplot(residuals(models))

raw2 %>% 
  shapiro_test(score) # 숫자형 변수인 score만 검정하였음
# # A tibble: 1 x 3
#   variable statistic            p
#   <chr>        <dbl>        <dbl>
# 1 score        0.743 0.0000000229
raw2 %>% 
  ggqqplot(x = "score", facet.by = "test", color = "test", palette = "jco")

raw2 %>% 
  t_test(after ~ before, paired = T)

raw2 %>% 
  gather(key = "test",
         value = "score") %>% 
  t_test(score ~ test, paired = T)
# # A tibble: 1 x 8
#   .y.   group1 group2    n1    n2 statistic    df     p
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl>
# 1 score after  before    27    27      1.33    26 0.195

#### 2.5. 시각화 ####
raw2 %<>% 
  gather(key = "test",
         value = "score") # 대응표본 검정을 위해 데이터를 타이디로 만들어준다.
raw2 %>% 
  t_test(score ~ test, paired = T) -> raw2_t_test

# Box플롯 그려보자
raw2 %>% 
  ggpaired(x = "test", y = "score", color = "test", palette = "jco",
           line.color = "gray", line.size = 0.4, ylim = c(0,7)) +
  stat_pvalue_manual(raw2_t_test, label = "T-Test, p = {p}", y.position = 5.5)
# 그림이 안 예쁘게 나오네요



# 3. 어떤 음료회사가 신제품을 개발하여 기존의 자사제품보다 소비자 선호도가 좋을지를 사전에 파악하기 위해서 고객에게 제품을 시식하고 평가를 받은 데이터이다. 기존 제품 맛과 신제품에 대한 전체 집단의 평균이 동일한지에 대해 95% 신뢰수준 하에서 검정하고자 합니다.

#### 0. 패키지 로드 ####
pacman::p_load(tidyverse, infer, tidyr, tidymodels, MASS, gginference, rstatix, ggpubr)

#### 1. 데이터 불러오기 ####
read_csv("score3.csv") -> raw3
?tidyr
#### 2. 데이터 살펴보기 ####
raw3 %>% 
  dplyr::glimpse()
Rows: 60
Columns: 2
# $ group <chr> "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "…
# $ score <dbl> 200, 235, 267, 300, 375, 500, 399, 423, 357, 244, 453, 4…
raw3 %>% 
  rstatix::get_summary_stats()
# # A tibble: 1 x 13
#   variable     n   min   max median    q1    q3   iqr   mad  mean    sd    se    ci
#   <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 score       60   181   500    389  356.  431.  74.8  50.4  385.  78.2  10.1  20.2


raw3 %>% 
  group_by(group) %>% 
  summarise(mean_score = mean(score))
# # A tibble: 2 x 2
# group mean_score
# <chr>      <dbl>
# 1 b           354.
# 2 c           399 


#### 3. 데이터 전처리 ####
# 3.1. 결측값 확인
raw3 %>% 
  summarise(group_NA = sum(is.na(group)),
            score_NA = sum(is.na(score)))
# # A tibble: 1 x 2
# group_NA score_NA
# <int>    <int>
#   1        0        0

#### 4. 데이터 분석 ####
  
raw3 %>% 
  infer::t_test(formula = score ~ group, # 그룹별 점수에 대한 t-test
         order = c("b", "c"),
         conf_level = 0.95, # 신뢰도 95% 수준에서 검정
         alternative = "two-sided") # 평균 동일한지이기 때문에 양측검정
raw3 %>% 
  rstatix::t_test(data =.,
                  formula = score ~ group,
                  comparisons = list(c("b", "c")),
                  alternative = "two.sided",
                  mu = 0,
                  conf.level = 0.95,
                  detailed = T) -> raw3_t_test

ggpubr::ggboxplot(raw3,
                  x = "group", y = "score",
                  color = "group", palette = "jco") +
  stat_pvalue_manual(raw3_t_test, label = "T-Test, p = {p}", y.position = 550) # 여기서 감격을 안 할 수가 없었다...

# # A tibble: 1 x 15
#   estimate estimate1 estimate2 .y.   group1 group2    n1    n2 statistic     p    df conf.low conf.high method
# *    <dbl>     <dbl>     <dbl> <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl>    <dbl>     <dbl> <chr> 
# 1    -44.7      354.       399 score b      c         19    41     -1.87 0.073  26.5    -93.9      4.47 T-test
# # … with 1 more variable: alternative <chr>

# # A tibble: 1 x 6
# statistic  t_df p_value alternative lower_ci upper_ci
# <dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>
# 1     -1.87  26.5  0.0730 two.sided      -93.9     4.47

#### 4.1. 데이터 시각화 ####
raw3 %>% 
  ggplot(aes(x = group, y = score)) +
  geom_boxplot()

raw3_observed_statistic <- raw3 %>%
  specify(score ~ group) %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_observed_statistic

raw3_null_distribution_2_sample_permute <- raw3 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_null_distribution_2_sample_theoretical <- raw3 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_null_distribution_2_sample_permute %>%
  visualize() + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_null_distribution_2_sample_theoretical %>%
  visualize(method = "theoretical") + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_null_distribution_2_sample_permute %>%
  visualize(method = "both") + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_p_value_2_sample <- raw3_null_distribution_2_sample_permute %>%
  get_p_value(obs_stat = raw3_observed_statistic,
              direction = "two-sided")

raw3_p_value_2_sample

# gginference를 사용해보자,
raw3 %>% 
  t_test(formula = score ~ group, # 그룹별 점수에 대한 t-test
               order = c("b", "c"),
               conf_level = 0.95, # 신뢰도 95% 수준에서 검정
               alternative = "two-sided") %>% 
  ggttest()
?ggttest
#### 5. 분석결과 해석 ####



# 4. 자동차 회사는 고객만족도를 조사하였다. 각 회사의 고객만족도에 차이가 있는지 95% 신뢰수준 하에서 검정하고자 한다.


#### 0. 패키지 로드 ####
pacman::p_load(tidyverse, infer, tidyr)

#### 1. 데이터 불러오기 ####
read_csv("score3.csv") -> raw3
?tidyr
#### 2. 데이터 살펴보기 ####
raw3 %>% 
  dplyr::glimpse()
# Rows: 60
# Columns: 2
# $ group <chr> "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "…
# $ score <dbl> 200, 235, 267, 300, 375, 500, 399, 423, 357, 244, 453, 4…

raw3 %>% 
  group_by(group) %>% 
  summarise(mean_score = mean(score))
# # A tibble: 2 x 2
# group mean_score
# <chr>      <dbl>
# 1 b           354.
# 2 c           399 


#### 3. 데이터 전처리 ####
# 3.1. 결측값 확인
raw3 %>% 
  summarise(group_NA = sum(is.na(group)),
            score_NA = sum(is.na(score)))
# # A tibble: 1 x 2
# group_NA score_NA
# <int>    <int>
#   1        0        0

# 3.2. 이상치 확인
raw3 %>% 
  group_by(group) %>% 
  identify_outliers(score)
# # A tibble: 4 x 4
# group score is.outlier is.extreme
# <chr> <dbl> <lgl>      <lgl>     
# 1 c       278 TRUE       FALSE     
# 2 c       279 TRUE       FALSE     
# 3 c       282 TRUE       FALSE     
# 4 c       181 TRUE       TRUE 

#### 4. 데이터 분석 ####

raw3 %>% 
  t_test(data = raw3, formula = score ~ group, # 그룹별 점수에 대한 t-test
         order = c("b", "c"),
         conf_level = 0.95, # 신뢰도 95% 수준에서 검정
         alternative = "two-sided") # 평균 동일한지이기 때문에 양측검정

# # A tibble: 1 x 6
# statistic  t_df p_value alternative lower_ci upper_ci
# <dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>
# 1     -1.87  26.5  0.0730 two.sided      -93.9     4.47

#### 4.1. 데이터 시각화 ####
raw3 %>% 
  ggplot(aes(x = group, y = score)) +
  geom_boxplot()


raw3_observed_statistic <- raw3 %>%
  specify(score ~ group) %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_observed_statistic

raw3_null_distribution_2_sample_permute <- raw3 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_null_distribution_2_sample_theoretical <- raw3 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_null_distribution_2_sample_permute %>%
  visualize() + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_null_distribution_2_sample_theoretical %>%
  visualize(method = "theoretical") + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_null_distribution_2_sample_permute %>%
  visualize(method = "both") + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_p_value_2_sample <- raw3_null_distribution_2_sample_permute %>%
  get_p_value(obs_stat = raw3_observed_statistic,
              direction = "two-sided")

raw3_p_value_2_sample

#### 5. 분석결과 해석 ####



# 0. 패키지 로드 
## install.package("rstatix")
library(rstatix)

# 1. 데이터 불러오기
raw4 <- read_csv("score4.csv")

# 2. 데이터 살펴보기 
raw4 %>% 
  glimpse()
raw4 %>% 
  group_by(group) %>% 
  summarise(mean_score = mean(score))
# # A tibble: 3 x 2
# group mean_score
# <chr>      <dbl>
# 1 P           70.6
# 2 Q           53.6
# 3 R           51.5

# 3. 데이터 분석

raw4 %>% 
  group_by(group) %>%
  get_summary_stats(score, type = "mean_sd")
# # A tibble: 3 x 5
#   group variable     n  mean    sd
#   <chr> <chr>    <dbl> <dbl> <dbl>
# 1 P     score        9  70.6  5.27
# 2 Q     score        7  53.6 17.3 
# 3 R     score       13  51.5 21.3 

models  <- lm(score ~ group, data = raw4)
ggqqplot(residuals(models)

# 4. 데이터 시각화
raw4 %>% 
  ggboxplot(x = "group",
            y = "score",
            color = "group",
            palette = "jco")

  
aov(score ~ group, data=raw4) %>% 
  tidy()
# # A tibble: 2 x 6
# term         df sumsq meansq statistic p.value
# <chr>     <dbl> <dbl>  <dbl>     <dbl>   <dbl>
# 1 group         2 2099.  1049.      3.65  0.0401
# 2 Residuals    26 7477.   288.     NA    NA   

# 등분산성 테스트
bartlett.test(score ~ group, data=raw4) %>% 
  tidy()
# A tibble: 1 x 4
# statistic p.value parameter method                                   
# <dbl>   <dbl>     <dbl> <chr>                                    
#   1      12.3 0.00216         2 Bartlett test of homogeneity of variances


