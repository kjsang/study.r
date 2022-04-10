##########################
######### R 기초 #########
##########################

library(tidyverse)
library(magrittr)
dt %<>% as_tibble(iris)


#### 기초 함수 ####
getwd() #워킹 디렉토리를 확인하는 함수
# install.packages("dplyr") # 패키지를 설치
require(rstatix) # 라이브러리 적재


#### library와 require의 차이 ####

# 설치된 패키지는 R 세션으로 불러와야 사용할 수 있는데, 이 작업은 require() 함수나 library() 함수로  할 수 있다. 예를 들어 KoNLP 패키지를 설치한 다음에, KoNLP 패키지를 R 세션으로 불러오기 위해서는 require(KoNLP) 또는 library(KoNLP)를 실행시키면 된다.  require() 함수와 library() 함수의 미묘한 차이는 설치되어 있지 않은 패키지를 불러오는 경우에 library() 함수는 오류를 발생시키지만, require() 함수는 경고 메시지를 보여주는 차이가 있다. 또한 require() 함수는 패키지를 불러오는 데 성공하면 TRUE 값을, 실패하면 FALSE 값을 발생시킨다.


#### R 버전 확인 및 헬프 ####

sessionInfo() # 버전과 패키지 등 버전 확인 가능
help(tidyverse) # 패키지 설명서 알려줘
?tidyverse # help()와 같다
# 예시
install.packages("rstatix")
library(rstatix)
?rstatix # 안 나오는 걳도 있다..

as_tibble(iris) %>%
  head()

as_tibble(iris) %>%
  plot() # 기본적인 플랏 함수

#  응용해보자
as_tibble(iris) %>% # 데이터를 티블로 변환
  select(Sepal.Length, Sepal.Width) %>% # 변수 선택
  plot(col=iris$Species) # 색상별 산점도

#### 연습 ####
read.csv("tips.csv") %>% # 데이터 불러오기
  as_tibble() -> tips # 데이터를 티블로 변환

tips %>% 
  get_summary_stats() # rstatix 함수 

tips %>% 
  head()

tips %>% 
  ggplot(aes(size)) +
  geom_histogram()

tips %>%
  ggplot(aes(total_bill, tip)) +
  geom_point(aes(col = day,
                 pch = sex,
                 size = 3))


#### 연산자 ####
?Syntax # 연산자의 우선순위

# <- 와 = 의 차이
A <- B = 25 # A를 찾을 수 없다고 나온다.
# 왜냐하면 = 보다 <- 가 먼저 수행이 될 텐데 B 객체에는 세상에 존재하지 않는 친구이다.
# 따라서 A 에 할당시켜야 하는 값을 R은 알지 못하여 에러가 난다.
B <- 25
A <- B
A <- (B = 25) # 연산자 우선순위를 고려한 코딩
A = B <- 25 # 할당 연산자 통일

# 음의 정수값 연산
-10^2 # 이건 안 되고
(-10)^2 # 이건 된다


#### 자료형 확인 ####
name <- "홍길동"
mode(name)
T+T+T+T+T+T+T+T  # T 는 1이다.
123+(10+4i) # 복소수는 complex이다.

iris %>% 
  names # 데이터 값의 변수를 알아보자.

x <- seq(0,1,0.1) # 0부터 1까지 0.1 간격으로 벡터 생성
y <- seq(0,1,length=11) # 0부터 1까지 11개 만들기
replace(x, 2, 0.4) # 바꿀 데이터셋, 순서, 바뀔 레코드)
?append
append(1:5, 0:1, after = 3) # 데이터를 추가
?rep
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
rep(1:4, c(2,1,2,1))
rep(1:4, each = 2, length.out = 4)    # first 4 only.
rep(1:4, each = 2, length.out = 10)   # 8 integers plus two recycled 1's.
rep(1:4, each = 2, times = 3) 


#### 매트릭스 구조 ####

matrix(1:6, 2, 3, byrow = T) %>% # 가로로 채워줘
  print()

# cbind와 rbind
?cbind
m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
m
cbind(1:7, diag(3)) # vector is subset -> warning

cbind(0, rbind(1, 1:3))
cbind(I = 0, X = rbind(a = 1, b = 1:3))  # use some names
xx <- data.frame(I = rep(0,2))
cbind(xx, X = rbind(a = 1, b = 1:3))   # named differently

cbind(0, matrix(1, nrow = 0, ncol = 4)) #> Warning (making sense)
dim(cbind(0, matrix(1, nrow = 2, ncol = 0))) #-> 2 x 1

## deparse.level
dd <- 10
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 0) # middle 2 rownames
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 1) # 3 rownames (default)
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 2) # 4 rownames

###############################
########### 2일차 #############
###############################

# 데이터형태를 확인하는 방법

is_character("홍길동")
is.numeric(pi)
x <- pi:10
is.numeric(x)

# 데이터를 만드는 방법
array(data = 1:5,
      dim = c(2,4))
# > array(data = 1:5,
#         +       dim = c(2,4))
# [,1] [,2] [,3] [,4]
# [1,]    1    3    5    2
# [2,]    2    4    1    3
matrix(data = 1:12,
       nrow = 6,
       ncol = 2,
       byrow = F)
# > matrix(data = 1:12,
#          +        nrow = 6,
#          +        ncol = 2,
#          +        byrow = F)
# [,1] [,2]
# [1,]    1    7
# [2,]    2    8
# [3,]    3    9
# [4,]    4   10
# [5,]    5   11
# [6,]    6   12

# 행렬의 계산
x <- array(1:4,
           dim = c(2,2))
y <- array(5:8,
           dim = c(2,2))
x %*% y # 행렬곱셈
t(x) # 전치행렬
solve(x) # 역행렬
det(x) # 행렬식 


# apply 계열의 함수

# sample 추출
as_tibble(Titanic) -> td_Titanic
td_Titanic %>% 
  sample(10, replace = T) # 오류가 난다. 왜?

as_tibble(iris) %>% 
  sample(10, replace = T) # 이상하게 나오는 이유는 벡터나 배열에서 샘플을 추출하는 함수이기 때문이다!
# 즉, sample을 바로 적용해 사용하지는 말 것 (인덱스를 사용)

# 데이터 다루기\
as_tibble(Titanic) -> td_Titanic
td_Titanic %>% 
  filter(Class == "1st") %>% 
  summary

# 데이터 프레임 만들기
name <- c("김", "나", "박", "이")
age <- c(20:23)
gender <- as.factor(c("M","F","M","M"))
blood.type <- as.factor(c("A","B","O","AB"))
patients1 <- data.frame(names, age, gender, blood.type)
patients1

as_tibble(cars) %>% # cars를 tibble 데이터로 변환
  select(speed, dist) %>%  # 변수 중 speed와 dist를 선택한다
  filter(speed > 20) %>% # speed가 20을 초과하는 데이터만 추출
  select(-dist) # 추출된 데이터 중 dist 변수를 제거

as_tibble(airquality) %>% 
  na.omit() # 데이터 중 결측값을 무시하자

# lapply 대신 사용할 수 있는 것
as_tibble(airquality) %>% 
  na.omit() %>% # 결측치 제거 
  dplyr::group_by(Month) %>% #월별 데이터를 보고싶어요
  summarise(mean_Ozone = mean(Ozone), # 오존량 평균 
            mean_Solar = mean(Solar.R)) # 일조량 평균
