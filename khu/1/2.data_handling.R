library(tidyverse)
require(magrittr)

history <- c(90, 80, 60 ,70)
math <- c(50, 60, 100, 20)
midterm <- data.frame(history, math)
Class <- c(1,1,2,2) 

# 외부데이터 불러오기


# install.packages("readxl")
library(readxl)
df_finalexam <- read_xlsx("finalexam.xlsx") # 데이터 불러오기
as_tibble(df_finalexam) %>%
  na.omit() %>%
  summarise(mean_history = mean(history),
            mean_math = mean(math))
write.csv(df_finalexam, file = "output_newdata.csv") # csv로 저장하기

exam <- read_csv("csv_exam.csv") # csv 파일을 불러와보자
exam %>% 
  glimpse() # 슥 보아보자

# 변수명 바꾸기
mpg -> mpg_test
mpg_test %>%
  rename(city = cty, # 데이터 변수명 바꾸기 (바뀔 변수명이 앞에 위치)
         highway = hwy)

# 파생변수 만들기
mpg_test %>% 
  rename(city = cty, # 데이터 변수명 바꾸기 (바뀔 변수명이 앞에 위치)
         highway = hwy) %>% 
  mutate(total = (city + highway)/2) # 파생변수 생성 (통합연비)

# total을 요약해서 보기
mpg_test %>%
  rename(city = cty, # 데이터 변수명 바꾸기 (바뀔 변수명이 앞에 위치)
         highway = hwy) %>%
  mutate(total = (city + highway) / 2) %>%  # 파생변수 생성 (통합연비)
  mutate(test = ifelse(total >= 20, "pass", "fail")) # 

mpg_test %>%
  rename(city = cty, # 데이터 변수명 바꾸기 (바뀔 변수명이 앞에 위치)
         highway = hwy) %>%
  mutate(total = (city + highway) / 2) %>% 
  mutate(test = ifelse(total >= 20, "pass", "fail")) %>% # 파생변수 생성 (통합연비)
  group_by(manufacturer, test) %>% #데이터를 제조사, 테스트 별로 보기
  summarize(n())

mpg_test %>%
  rename(city = cty, # 데이터 변수명 바꾸기 (바뀔 변수명이 앞에 위치)
         highway = hwy) %>%
  mutate(total = (city + highway) / 2) %>% 
  mutate(grade = ifelse (total >= 30, "A", # 등급을 나눠보자
                       ifelse(total >= 25, "B",
                              ifelse(total >= 20, "C", "D")))) %>% 
  group_by(grade) %>% # 등급별로 
  summarise(n = n()) -> grade_n # 몇 개가 있는지 보자

# 시각화
grade_n %>% 
  ggplot(aes(x = as.factor(grade), y = n)) +
  geom_bar(stat = "identity", alpha = 0.5)

ggplot2::midwest %>%
  rename(total = poptotal, # 이름 재설정
         asian = popasian) %>% 
  select(total, asian) %>% # 변수 선택
  mutate(pop_asian = asian / total) %>% # 파생변수 만들기
  mutate(size = ifelse(pop_asian > mean(pop_asian), "large", "small")) %>% # 파생변수2 만들기
  group_by(size) %>% # 등급별로 그룹 묶기
  summarise(n = n()) %>% # 등급별 갯수 찾기
  ggplot(aes(x = as.factor(size), y = n)) + # 그림을 그려보자
  geom_bar(stat = "identity", alpha = 0.7) +
  theme_light()

#############################
########### 3일차 ###########
#############################

# dplyr 패키지를 사용해보자!

exam <- read_csv("csv_exam.csv") # 데이터 불러오기
exam %>% 
  filter(class == 1) %>%  # 클래스를 추출하기 (조건에 해당하는 행을 추출한다)
  filter(math >= 50) # 두 번쨰 조건: 수학점수 50점 이상
# # A tibble: 2 x 5
# id class  math english science
# <dbl> <dbl> <dbl>   <dbl>   <dbl>
# 1     1     1    50      98      50
# 2     2     1    60      97      60

exam %>% # 이렇게도 할 수 있다
  filter(class == 1 & math >= 50) # &으로 연결할 수 있다. or: (|) 으로 하기

# 혼자서 해보기1: 배기량별 연비 비교
mpg %>% 
  mutate(배기량기준 = ifelse(displ<=4, "저배기량", "고배기량")) %>% 
  group_by(배기량기준) %>% 
  summarize(평균배기량 = mean(hwy))
# # A tibble: 2 x 2
# 배기량   평균배기량
# <chr>         <dbl>
# 1 고배기량       17.6
# 2 저배기량       26.0

# 혼자서 해보기2: 제조회사별 도시 연비 평균
mpg %>% 
  filter(manufacturer %in% c("audi", "toyota")) %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty))
# # A tibble: 2 x 2
# manufacturer mean_cty
# <chr>           <dbl>
# 1 audi             17.6
# 2 toyota           18.5

# 혼자서 해보기3: 제조사별 고속도로 연비 평균
mpg %>% 
  filter(manufacturer %in% c("chevrolet", "ford", "honda")) %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy))
# # A tibble: 3 x 2
# manufacturer mean_hwy
# <chr>           <dbl>
# 1 chevrolet        21.9
# 2 ford             19.4
# 3 honda            32.6


# select 함수 사용하기
exam %>% 
  select(class, math, english) %>% # 뺴고싶은 변수가 소수인 경우 -var 명령어를 사용
  head(5)
# exam %>% 
#   select(-id)
# A tibble: 5 x 3
# class  math english
# <dbl> <dbl>   <dbl>
# 1     1    50      98
# 2     1    60      97
# 3     1    45      86
# 4     1    30      98
# 5     2    25      80

# 혼자서 해보기1: 변수 추출하기
mpg %>% 
  select(class, cty) %>% 
  head(5)
# # A tibble: 5 x 2
# class     cty
# <chr>   <int>
# 1 compact    18
# 2 compact    21
# 3 compact    20
# 4 compact    21
# 5 compact    16

# 혼자서 해보기2: 차종별 도시연비 평균비교
mpg %>% 
  select(class, cty) %>% 
  filter(class %in% c("suv", "compact")) %>% 
  group_by(class) %>% # 차종별로 그룹핑
  summarize(mean_class = mean(cty)) # 그룹핑한 차종별 평균 비교
# # A tibble: 2 x 2
# class   mean_class
# <chr>        <dbl>
# 1 compact       20.1
# 2 suv           13.5

# arrange 함수 사용하기: 정렬하기
exam %>% 
  arrange(math) # math를 오름차순으로 정렬하자 (내림차순은 desc()함수 사용)
exam %>% 
  arrange(class, desc(math)) # 클래스별로 정렬 후 수학점수로 정렬

# 혼자서 해보기1: 특정 모델의 연비 상위데이터 추출
mpg %>% 
  filter(manufacturer == "audi") %>% # 제조사 필터링
  arrange(desc(hwy)) %>% # 고속도로 내림차순 
  head(5) # 상위 5개 데이터 출력
# # A tibble: 5 x 11
# manufacturer model      displ  year   cyl trans      drv     cty   hwy fl    class  
# <chr>        <chr>      <dbl> <int> <int> <chr>      <chr> <int> <int> <chr> <chr>  
# 1 audi         a4           2    2008     4 manual(m6) f        20    31 p     compact
# 2 audi         a4           2    2008     4 auto(av)   f        21    30 p     compact
# 3 audi         a4           1.8  1999     4 auto(l5)   f        18    29 p     compact
# 4 audi         a4           1.8  1999     4 manual(m5) f        21    29 p     compact
# 5 audi         a4 quattro   2    2008     4 manual(m6) 4        20    28 p     compact

# dplyr::mutate() 사용하기: 변수 새로 만들기
exam %>%
  mutate(total = math + english + science,
         mean = (total / 3)) %>% # 기본적인 파생변수 만들기
  head()
# # A tibble: 6 x 7
# id class  math english science total  mean
# <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl> <dbl>
# 1     1     1    50      98      50   198  66  
# 2     2     1    60      97      60   217  72.3
# 3     3     1    45      86      78   209  69.7
# 4     4     1    30      98      58   186  62  
# 5     5     2    25      80      65   170  56.7
# 6     6     2    50      89      98   237  79  

exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>% # 조건을 포함한 변수 만들기
  head()

# mutate() 혼자서 해보기1: 합산 연비 변수 추가하기
# mutate() 혼자서 해보기2: 평균 연비 변수 추가하기
# mutate() 혼자서 해보기3: 하위 3종 데이터 출력하기
# mutate() 혼자서 해보기4: 하나로 연결된 구문 출력하기
mpg %>% 
  mutate(total = (hwy + cty)/2) %>% # 합산평균연비 변수 만들기
  arrange(desc(total)) %>% # 데이터 내림차순
  head(3) # 상위 3 종 데이터 추출
# # A tibble: 3 x 12
# manufacturer model      displ  year   cyl trans      drv     cty   hwy fl    class      total
# <chr>        <chr>      <dbl> <int> <int> <chr>      <chr> <int> <int> <chr> <chr>      <dbl>
# 1 volkswagen   new beetle   1.9  1999     4 manual(m5) f        35    44 d     subcompact  39.5
# 2 volkswagen   jetta        1.9  1999     4 manual(m5) f        33    44 d     compact     38.5
# 3 volkswagen   new beetle   1.9  1999     4 auto(l4)   f        29    41 d     subcompact  35  

# dplyr::summarize() 사용해보기: 집단평균 등
exam %>% 
  group_by(class) %>% # 집단별
  summarise(mean_math = mean(math), #평균 출력
            sum_math = sum(math), # 합계 출력
            median_math = median(math), # 중위값 출력
            n = n()) # 데이터 수 출력
# # A tibble: 5 x 5
# class mean_math sum_math median_math     n
# <dbl>     <dbl>    <dbl>       <dbl> <int>
# 1     1      46.2      185        47.5     4
# 2     2      61.2      245        65       4
# 3     3      45        180        47.5     4
# 4     4      56.8      227        53       4
# 5     5      78        312        79       4

mpg %>% 
  group_by(manufacturer, drv) %>% # 제조사와 drv 변수별 평균
  summarize(mean_cty = mean(cty)) %>% 
  head(10)
# # A tibble: 10 x 3
# # Groups:   manufacturer [5]
# manufacturer drv   mean_cty
# <chr>        <chr>    <dbl>
# 1 audi         4         16.8
# 2 audi         f         18.9
# 3 chevrolet    4         12.5
# 4 chevrolet    f         18.8
# 5 chevrolet    r         14.1
# 6 dodge        4         12  
# 7 dodge        f         15.8
# 8 ford         4         13.3
# 9 ford         r         14.8
# 10 honda        f         24.4

# 혼자서 해보기1,2: 클래스별 도시연비
mpg %>% 
  group_by(class) %>% 
  summarize(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))
# # A tibble: 7 x 2
# class      mean_cty
# <chr>         <dbl>
# 1 subcompact     20.4
# 2 compact        20.1
# 3 midsize        18.8
# 4 minivan        15.8
# 5 2seater        15.4
# 6 suv            13.5
# 7 pickup         13  

# 혼자서 해보기3: 고속도로연비 상위3개 제조사
mpg %>% 
  group_by(manufacturer) %>% 
  summarize(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)
# # A tibble: 3 x 2
# manufacturer mean_hwy
# <chr>           <dbl>
# 1 honda            32.6
# 2 volkswagen       29.2
# 3 hyundai          26.9

# 혼자서 해보기4: 경차생산 차종수
mpg %>% 
  group_by(manufacturer, class) %>% # 그룹 설정
  summarize(compact_n = n()) %>% # 그룹별 수 산정
  filter(class == "compact") %>% # 이 중 클래스가 경차인 것만 선택
  arrange(desc(compact_n)) %>% # 내림차순 정렬
  select(manufacturer, compact_n) # 제조사와 수만 출력
# # A tibble: 5 x 2
# # Groups:   manufacturer [5]
# manufacturer compact_n
# <chr>            <int>
# 1 audi                15
# 2 volkswagen          14
# 3 toyota              12
# 4 subaru               4
# 5 nissan               2

# _join() 함수 배워보자
test1 <- tibble(id = c(1:5),
                mditerm = c(60, 80, 70, 90, 85))
test2 <- tibble(id = c(1:5),
                final = c(70, 83, 65, 95, 80))
total <- left_join(test1, test2, by = "id") # id 기준으로 합치자
name <- tibble(class = c(1:5),
               # 반별
               teacher = c("Kim", "Na", "Park", "Lee", "Choi")) # 선생님이름
exam_new <- left_join(exam, name, by = "class") # 알아서 결합이 된다
exam_new
group_A <- tibble(id = c(1:5),
                  test = c(60, 80, 70, 90, 85))
group_B <- tibble(id = c(6:10),
                  test = c(70, 83, 65, 95, 80))
group_all <- bind_rows(group_A, group_B)

# 혼자서 해보기1: 연료가격을 추가해보자
fuel <- tibble(fl = c("c", "d", "e", "p", "r"),
               price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22))
left_join(mpg, fuel, by = "fl")
# # A tibble: 234 x 12
# manufacturer model      displ  year   cyl trans      drv     cty   hwy fl    class   price_fl
# <chr>        <chr>      <dbl> <int> <int> <chr>      <chr> <int> <int> <chr> <chr>      <dbl>
# 1 audi         a4           1.8  1999     4 auto(l5)   f        18    29 p     compact     2.76
# 2 audi         a4           1.8  1999     4 manual(m5) f        21    29 p     compact     2.76
# 3 audi         a4           2    2008     4 manual(m6) f        20    31 p     compact     2.76
# 4 audi         a4           2    2008     4 auto(av)   f        21    30 p     compact     2.76
# 5 audi         a4           2.8  1999     6 auto(l5)   f        16    26 p     compact     2.76
# 6 audi         a4           2.8  1999     6 manual(m5) f        18    26 p     compact     2.76
# 7 audi         a4           3.1  2008     6 auto(av)   f        18    27 p     compact     2.76
# 8 audi         a4 quattro   1.8  1999     4 manual(m5) 4        18    26 p     compact     2.76
# 9 audi         a4 quattro   1.8  1999     4 auto(l5)   4        16    25 p     compact     2.76
# 10 audi         a4 quattro   2    2008     4 manual(m6) 4        20    28 p     compact     2.76
# # … with 224 more rows

# 분석 도전 !: midwest 데이터 다뤄보기

# 1. 데이터 특성 살펴보기
midwest %>% 
  glimpse() # 변수를 한 번 살펴보자

# 2. 데이터 전처리
midwest %>% 
  mutate(total = (popadults/poptotal), # 전체비율 만들기
         미성년인구 = poptotal - popadults, # 미성년인구 수
         미성년비율 = 미성년인구/poptotal) %>%  # 미성년인구 백분율
  mutate(기준 = ifelse(미성년비율 >= 0.4, "large", # 기준 만들기
                          ifelse(미성년비율 >= 0.3, "middle", "small"))) %>% 
  arrange(desc(미성년인구)) %>% # 미성년 인구 정렬
  select(county, 미성년인구, 기준) %>%  # 선택
  group_by(기준) %>%  # 기준별
  summarize(n = n()) # 카운티 수 보기
# # A tibble: 3 x 2
# 기준       n
# <chr>  <int>
# 1 large     32
# 2 middle   396
# 3 small      9

# 아시아인구비율 하위 10개 지역 출력하기
midwest %>% 
  select(state, county, popasian, poptotal) %>% 
  mutate(아시안인구비율 = popasian/poptotal) %>% 
  arrange(아시안인구비율) %>% 
  head(10)
# # A tibble: 10 x 5
# state county    popasian poptotal 아시안인구비율
# <chr> <chr>        <int>    <int>          <dbl>
# 1 WI    MENOMINEE        0     3890       0       
# 2 IN    BENTON           1     9441       0.000106
# 3 IN    CARROLL          3    18809       0.000159
# 4 OH    VINTON           3    11098       0.000270
# 5 WI    IRON             2     6153       0.000325
# 6 IL    SCOTT            3     5644       0.000532
# 7 IN    CLAY            15    24705       0.000607
# 8 MI    OSCODA           5     7842       0.000638
# 9 OH    PERRY           21    31557       0.000665
# 10 IL    PIATT           11    15548       0.000707

