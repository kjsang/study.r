### Hypothesis Testing

#### 1. package loading ####
pacman::p_load(tidyverse, infer, moderndive, nycflights13, ggplot2movies, tidymodels, tidyr)
# library(tidyverse)
# library(infer)
# library(moderndive)
# library(nycflights13)
# library(ggplot2movies)

## install.packages("tidymodels")


#### 2. data setting ####
rawN3 <- read_csv("htest01.csv")
rawN3 %>% 
  glimpse()
# Rows: 6
# Columns: 2
# $ group  <chr> "A", "A", "A", "B", "B", "B"
# $ height <dbl> 170, 177, 163, 168, 190, 185

#### data split ####
ggplot(rawN3,
       aes(x = group,
           y = height)) +
  geom_point()

# 2. 평균비교
rawN3 %>% 
  t_test(formula = height ~ group,
         alternative = "two.sided")
# 
# # A tibble: 1 x 6
# statistic  t_df p_value alternative lower_ci upper_ci
# <dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>
# 1     -1.41  3.30   0.245 two.sided      -34.6     12.6


rawN10 <- read_csv("htest02.csv")
rawN10 %>% 
  specify(height ~ group) %>% 
  hypothesize(null = "p", mu = 40) %>%
  calculate(stat = "t") -> rawN10_observed_statistic

rawN10_null_distribution_1_sample <- rawN10 %>%
  specify(height ~ group) %>%
  hypothesize(null = "p", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "t")

rawN10_null_distribution_1_sample %>%
  visualize() + 
  shade_p_value(rawN10_observed_statistic,
                direction = "two-sided")

rawN10_p_value_1_sample <- rawN10_null_distribution_1_sample %>%
  get_p_value(obs_stat = rawN10_observed_statistic,
              direction = "two-sided")

rawN10_p_value_1_sample


#### 대응표본 t검정 #### 
rawN10_observed_statistic <- rawN10 %>%
  specify(height ~ group) %>%
  calculate(stat = "t", order = c("A", "B"))

rawN10_observed_statistic

rawN10_null_distribution_2_sample_permute <- rawN10 %>%
  specify(height ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("A", "B"))

rawN10_null_distribution_2_sample_theoretical <- rawN10 %>%
  specify(height ~ group) %>%
  hypothesize(null = "independence") %>%
  # generate() isn't used for the theoretical version!
  calculate(stat = "t", order = c("A", "B"))

rawN10_null_distribution_2_sample_permute %>%
  visualize() + 
  shade_p_value(rawN10_observed_statistic,
                direction = "two-sided")

rawN10_null_distribution_2_sample_theoretical %>%
  visualize(method = "theoretical") + 
  shade_p_value(rawN10_observed_statistic,
                direction = "two-sided")

rawN10_null_distribution_2_sample_permute %>%
  visualize(method = "both") + 
  shade_p_value(rawN10_observed_statistic,
                direction = "two-sided")

rawN10_p_value_2_sample <- rawN10_null_distribution_2_sample_permute %>%
  get_p_value(obs_stat = rawN10_observed_statistic,
              direction = "two-sided")

rawN10_p_value_2_sample

t_test(x = rawN10, 
       formula = height ~ group, 
       order = c("A", "B"),
       alternative = "two-sided")

#### 차이검정 ####
read_csv("htest02d.csv") -> raw_d # 데이터 불러오기
raw_d %<>% 
  mutate(difference = after - before) 
raw_d %>% 
  t_test(response = difference,
       alternative = "less")

rawN30 <- read_csv("htest03.csv") # 이건 뭔가 어렵다... 못하겠음 
rawN30 %>% 
  infer::prop_test(formula = height ~ group,
                   order = c("A", "B"),
                   alternative = "two-sided",
                   conf_level = 0.95)


# Tidy ANOVA (Analysis of Variance) with infer

# 1. 데이터 불러오기 및 살펴보기 
raw_ANOVA <- read_csv("htest04.csv")
dplyr::glimpse(raw_ANOVA)
raw_ANOVA %>% 
  group_by(group) %>% 
  summarise(mean_height = mean(height))

# 2. 데이터 ANOVA 분석
raw_ANOVA %>% 
  specify(height ~ group) %>% 
  calculate(stat = "F")
# # A tibble: 1 x 1
#   stat
#   <dbl>
#1  18.2

raw_ANOVA_observed_f_statistic <- raw_ANOVA %>%
  specify(height ~ group) %>%
  calculate(stat = "F")

raw_ANOVA_null_distribution <- raw_ANOVA %>%
  specify(height ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

raw_ANOVA_null_distribution %>%
  visualize() + 
  shade_p_value(raw_ANOVA_observed_f_statistic,
                direction = "greater")

raw_ANOVA %>%
  specify(height ~ group) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") + 
  shade_p_value(raw_ANOVA_observed_f_statistic,
                direction = "greater")

raw_ANOVA_null_distribution %>%
  visualize(method = "both") + 
  shade_p_value(raw4_observed_f_statistic,
                direction = "greater")


#### 카이제곱 검정 ####

raw_chi <- read_csv("htest05.csv")
c(raw_chi)
raw_chi %>% 
  chisq_test(smoke ~ disease)
