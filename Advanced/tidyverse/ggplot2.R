
# 패키지 로드 ------------------------------------------------------------------

if (!require("pacman"))  install.packages("pacman")
pacman::p_load(tidyverse, magrittr, # helper packages
               rstatix) # statistics tool


# 데이터 살펴보기 ----------------------------------------------------------------

diamonds %>% glimpse()
# # A tibble: 7 x 13
#   variable     n   min     max  median     q1     q3     iqr     mad    mean      sd     se
#   <chr>    <dbl> <dbl>   <dbl>   <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>
# 1 carat    53940   0.2  5.01e0    0.7    0.4  1.04e0    0.64 4.74e-1 7.98e-1 4.74e-1  0.002
# 2 depth    53940  43    7.9 e1   61.8   61    6.25e1    1.5  1.04e+0 6.17e+1 1.43e+0  0.006
# 3 price    53940 326    1.88e4 2401    950    5.32e3 4374.   2.48e+3 3.93e+3 3.99e+3 17.2
# 4 table    53940  43    9.5 e1   57     56    5.9 e1    3    1.48e+0 5.75e+1 2.23e+0  0.01
# 5 x        53940   0    1.07e1    5.7    4.71 6.54e0    1.83 1.38e+0 5.73e+0 1.12e+0  0.005
# 6 y        53940   0    5.89e1    5.71   4.72 6.54e0    1.82 1.36e+0 5.74e+0 1.14e+0  0.005
# 7 z        53940   0    3.18e1    3.53   2.91 4.04e0    1.13 8.45e-1 3.54e+0 7.06e-1  0.003
# # … with 1 more variable: ci <dbl>
# > diamonds %>% glimpse()
diamonds %>% rstatix::get_summary_stats()
# Rows: 53,940
# Columns: 10
# $ carat   <dbl> 0.23, 0.21, 0.23, 0.29, 0.31, 0.24, 0.24, 0.26, 0.22, 0.23, 0.30, 0.23, 0…
# $ cut     <ord> Ideal, Premium, Good, Premium, Good, Very Good, Very Good, Very Good, Fai…
# $ color   <ord> E, E, E, I, J, J, I, H, E, H, J, J, F, J, E, E, I, J, J, J, I, E, H, J, J…
# $ clarity <ord> SI2, SI1, VS1, VS2, SI2, VVS2, VVS1, SI1, VS2, VS1, SI1, VS1, SI1, SI2, S…
# $ depth   <dbl> 61.5, 59.8, 56.9, 62.4, 63.3, 62.8, 62.3, 61.9, 65.1, 59.4, 64.0, 62.8, 6…
# $ table   <dbl> 55, 61, 65, 58, 58, 57, 57, 55, 61, 61, 55, 56, 61, 54, 62, 58, 54, 54, 5…
# $ price   <int> 326, 326, 327, 334, 335, 336, 336, 337, 337, 338, 339, 340, 342, 344, 345…
# $ x       <dbl> 3.95, 3.89, 4.05, 4.20, 4.34, 3.94, 3.95, 4.07, 3.87, 4.00, 4.25, 3.93, 3…
# $ y       <dbl> 3.98, 3.84, 4.07, 4.23, 4.35, 3.96, 3.98, 4.11, 3.78, 4.05, 4.28, 3.90, 3…
# $ z       <dbl> 2.43, 2.31, 2.31, 2.63, 2.75, 2.48, 2.47, 2.53, 2.49, 2.39, 2.73, 2.46, 2…

diamonds %>%
  ggplot(mapping = aes(x = cut, # x축에 cut변수를 표시
                       fill = cut)) + # 색상 표시
  geom_bar() # 자동으로 y축을 count함

diamonds %>%
  group_by(cut) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = cut, y = n)) + # x축에 n 수를 표시
  geom_bar(stat = "identity") # 막대의 높이를 y 변수의 원값으로 매핑 가능

diamonds %>%
  ggplot() +
  stat_summary( # 데이터의 요약값을 표현하고 싶을 때 사용
    aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

mpg %>%
  ggplot() +
  geom_jitter(aes(x = displ, y = hwy)) # 점을 반올림해주는 걸 흐트러뜨려준다.
mpg %>%
  ggplot() +
  geom_jitter(aes(x= cty, y = hwy))
my_variable <- 10