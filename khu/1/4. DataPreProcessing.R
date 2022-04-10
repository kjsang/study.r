# 결측치 다루기

library(tidyverse)
tibble(sex = c("M", "M", NA, "F", "F"),
       score = c(5, 4, 3, 4, NA)) -> tb
is.na(tb)
# sex score
# [1,] FALSE FALSE
# [2,] FALSE FALSE
# [3,]  TRUE FALSE
# [4,] FALSE FALSE
# [5,] FALSE  TRUE

tb %>% 
  filter(!is.na(score)) %>% #결측치를 제거한 후
  summarize(sum = sum(score)) # 스코어를 보면 오류 안 난다
# # A tibble: 1 x 1
# sum
# <dbl>
#   1    16

# 여러 변수에서 문제가 있을 때?: 1번째 방법
tb %>% 
  filter(!is.na(score) & !is.na(sex)) %>% # & 로 연결
           summarize(sum = sum(score)) 
# # A tibble: 1 x 1
# sum
# <dbl>
#   1    13

# 여러 변수에서 문제가 있을 때?: 2번째 방법
tb %>% 
  na.omit() %>% # 모든 결측치 초함된 row 제거
  summarize(sum = sum(score)) 
# # A tibble: 1 x 1
# sum
# <dbl>
#   1    13

# 여러 변수에서 문제가 있을 때?: 3번째 방법
tb %>% 
  summarize(sum = sum(score, na.rm = T)) # 결측치 제외한 값만 계산하기
# # A tibble: 1 x 1
# sum
# <dbl>
#   1    16

exam <- read_csv("csv_exam.csv") # 데이터 불러오기
exam[c(3,8,5), "math"] <- NA # 결측치 넣기

exam %>% 
  summarise(mean_math = mean(math, na.rm=T),
            sum_math = sum(math, na.rm = T))

# 결측치를 평균값으로 대체하기
exam %>% 
  mutate(math = ifelse(is.na(math), 
                       mean(math, na.rm = T), # 평균으로 대체
                       math))

# # A tibble: 20 x 5
# id class  math english science
# <dbl> <dbl> <dbl>   <dbl>   <dbl>
# 1     1     1  50        98      50
# 2     2     1  60        97      60
# 3     3     1  58.2      86      78
# 4     4     1  30        98      58
# 5     5     2  58.2      80      65
# 6     6     2  50        89      98
# 7     7     2  80        90      45
# 8     8     2  58.2      78      25
# 9     9     3  20        98      15
# 10    10     3  50        98      45
# 11    11     3  65        65      65
# 12    12     3  45        85      32
# 13    13     4  46        98      65
# 14    14     4  48        87      12
# 15    15     4  75        56      78
# 16    16     4  58        98      65
# 17    17     5  65        68      98
# 18    18     5  80        78      90
# 19    19     5  89        68      87
# 20    20     5  78        83      58


# 혼자서 해보기1: 결측치가 있는 데이터 다루기
mpg_NA <- mpg 
mpg_NA[c(65, 124, 131, 153, 212), "hwy"] <- NA # NA 할당하기
mpg_NA %>% 
  select(drv, hwy) %>% 
  summarise(drv_NA = sum(is.na(drv)),
            hwy_NA = sum(is.na(hwy))) # 결측치 갯수 알아보자
# # A tibble: 1 x 2
#       drv_NA hwy_NA
#       <int>  <int>
#   1      0      5

# 결측치 제거한 차종별 연비 구하기
mpg_NA %>% 
  select(drv, hwy) %>% 
  filter(!is.na(hwy)) %>% # 결측치 제거
  group_by(drv) %>% 
  summarize(mean_hwy = mean(hwy)) # 변수 평균 구하기
# # A tibble: 3 x 2
# drv   mean_hwy
# <chr>    <dbl>
# 1 4         19.2
# 2 f         28.2
# 3 r         21  


# 이상치 찾아보자
outlier <- tibble(sex = c(1, 2, 1, 3, 2, 1),
                  score = c(5, 4, 3, 4, 2, 6))
# 이상치를 NA로 대체해주자
outlier %>% 
  mutate(sex = ifelse(sex==3, NA, sex), # 3이면 결측치처리
         score = ifelse(score > 5, NA, score)) # 5를 넘어서면 결측치 처리
# A tibble: 6 x 2
# sex score
# <dbl> <dbl>
# 1     1     5
# 2     2     4
# 3     1     3
# 4    NA     4
# 5     2     2
# 6     1    NA

# 결측치를 제외하고 분석해보자
outlier %>% 
  mutate(sex = ifelse(sex==3, NA, sex), # 3이면 결측치처리
         score = ifelse(score > 5, NA, score)) %>% # 5를 넘어서면 결측치 처리
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarize(mean_score = mean(score))
# # A tibble: 2 x 2
# sex mean_score
# <dbl>      <dbl>
# 1     1          4
# 2     2          3


# 이상치 중 극단치를 처리하는 방법
mpg %>% 
  ggplot(aes(y = hwy)) +
  geom_boxplot() # 이렇게 하면 극단치 못찾는다...

boxplot(mpg$hwy)$stats # 극단치 기준을 37로 잡아라!
#       [,1]
# [1,]   12
# [2,]   18
# [3,]   24
# [4,]   27
# [5,]   37

# 이상치가 몇 개인지 알아보자: 3개가 나오네
mpg %>% 
  mutate(hwy = ifelse(hwy < 12 | hwy > 37, NA, hwy)) %>% 
  summarize(hwy_NA = sum(is.na(hwy)))
# # A tibble: 1 x 1
# hwy_NA
# <int>
#   1      3

# 혼자서 해보기: 이상치 처리하기
mpg <- tibble(ggplot2::mpg) # mpg 데이터 불러오기
mpg[c(10, 14, 58, 93), "drv"] <- "k" # drv 이상치 할당
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42) # cty 이상치 할당

mpg %>% 
  mutate(drv = ifelse(drv %in% c("4", "f", "r"), drv, NA)) %>%
  summarize(drv_NA = sum(is.na(drv))) # 4개가 있는 것으로 확인
# A tibble: 1 x 1
# drv_NA
# <int>
#   1      4

boxplot(mpg$cty)$stats # 이상치... 이거 말고는 어떻게 알지?

mpg %>% 
  mutate(drv = ifelse(drv %in% c("4", "f", "r"), drv, NA)) %>%
  mutate(cty = ifelse(cty < 9 | hwy > 26, NA, cty)) %>% 
  summarize(drv_NA = sum(is.na(drv)),
            cty_NA = sum(is.na(cty))) # 각각 4개, 71개
# # A tibble: 1 x 2
# drv_NA cty_NA
# <int>  <int>
#   1      4     71

# 이상치를 제거한 후 그룹평균 구하기
mpg %>% 
  mutate(drv = ifelse(drv %in% c("4", "f", "r"), drv, NA)) %>%
  mutate(cty = ifelse(cty < 9 | hwy > 26, NA, cty)) %>% 
  group_by(drv) %>% 
  summarize(mean_cty = mean(cty, na.rm = T)) %>% 
  filter(!is.na(drv))
# # A tibble: 3 x 2
# drv   mean_cty
# <chr>    <dbl>
# 1 4         14.6
# 2 f         17.2
# 3 r         14.0

