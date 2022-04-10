
#### 1. 패키지 인스톨 및 적재 ####

## install.packages("pacman")
## install.packages("foreign")
pacman::p_load(tidyverse, magrittr, foreign) 


#### 2. 데이터 불러오기 및 살펴보기 #### 
foreign::read.spss("data_spss_Koweps2014.sav") -> raw_welfare
raw_welfare %>% 
  glimpse() # 변수 살펴보기: 총 1081개의 변수가 있음

#### 3. 데이터 전처리 ####
welfare <- as_tibble(raw_welfare) %>%
  rename(sex = h0901_4, # 성별
         birth = h0901_5, # 출생연도
         income = h09_din) %>%
  select(sex, birth, income)
welfare %>% 
  summary # 각 변수의 특성 확인하기
# sex            birth          income      
# Min.   :1.000   Min.   :1918   Min.   :-20516  
# 1st Qu.:1.000   1st Qu.:1940   1st Qu.:  1108  
# Median :1.000   Median :1952   Median :  2404  
# Mean   :1.309   Mean   :1953   Mean   :  3336  
# 3rd Qu.:2.000   3rd Qu.:1966   3rd Qu.:  4642  
# Max.   :2.000   Max.   :2002   Max.   :108888 


welfare %>% 
  mutate(sex = ifelse(sex %in% c(1,2), sex, NA)) %>% 
  summarize(sex_NA = sum(is.na(sex)),
            birth_NA = sum(is.na(birth)),
            income_NA = sum(is.na(income))) # 결측치 확인
# # A tibble: 1 x 3
# sex_NA birth_NA income_NA
# <int>    <int>     <int>
#   1      0        0         0

# 성별변수를 영어로 바꾸어보자 
welfare %<>% 
  mutate(sex = ifelse(sex == 1, "male", "female"))  # 성별변수 조정하기

#### 4. 분석 ####

# 분석1: 성별 소득 평균 분석
welfare %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income),
            median_income = median(income))
# # A tibble: 2 x 2
# sex    mean_income
# <chr>        <dbl>
# 1 female       1581.
# 2 male         4119.

# 분석2: 나이와 소득의 관계
welfare %>% 
  mutate(나이 = (2021 - birth) + 1) %>% 
  rename(소득 = income) %>% 
  group_by(나이) %>% 
  summarise(소득평균 = mean(소득)) %>% 
  ggplot(aes(x=나이, y=소득평균)) +
  geom_point()
# 분석을 하였으나, 연령별이기 때문에 난잡하다.

# 분석3: 연령대에 따른 소득
welfare %>% 
  mutate(age = (2021 - birth) + 1) %>% 
  mutate(generation = ifelse(age < 30, "junior",
                        ifelse(age < 60, "mid", "senior"))) %>%
  group_by(generation) %>% 
  summarise(mean_income = mean(income)) %>% 
  ggplot(aes(x=generation, y=mean_income)) +
  geom_col()

# 분석4: 연령대 및 성별에 따른 소득
welfare %>% 
  mutate(age = (2021 - birth) + 1) %>% 
  mutate(generation = ifelse(age < 30, "junior",
                             ifelse(age < 60, "mid", "senior"))) %>% # 각 세대를 나누기
  group_by(generation, sex) %>% # 그룹으로 묶어주기
  summarize(mean_income = mean(income)) %>% # 소득 평균 구하기
  mutate(generation_sex = paste(generation,"_",sex)) %>% # 성별과 세대 변수 생성
  ungroup() %>% # 묶어준거 풀고
  select(generation_sex, mean_income) %>% # 소득과 세대&성별 변수 선택
  ggplot(aes(x= reorder(generation_sex, -mean_income), y = mean_income)) +
  geom_col() # 그래프 그리기

# 교수님 코드
welfare %>% 
  mutate(age = (2021 - birth) + 1) %>% 
  mutate(generation = ifelse(age < 30, "junior",
                             ifelse(age < 60, "mid", "senior"))) %>% # 각 세대를 나누기
  group_by(generation, sex) %>% # 그룹으로 묶어주기
  summarize(mean_income = mean(income)) %>% 
  ungroup() %>% # 소득 평균 구하기
  ggplot(aes(x = reorder(generation, -mean_income),
             y = mean_income, fill = sex)) +
  geom_col(position = "dodge") # 그래프 그리기, 닷지로 하면 따로 나온다 

