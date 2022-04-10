
# 패키지 로드 ------------------------------------------------------------------

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(tidyverse, magrittr, # helper packages
               rstatix, # statistics tool
               nycflights13 # dataset
)
# arrange -------------------------------------------------------------------------------------


# 결측치 확인
nycflights13::flights %>%
  arrange(dep_time) %>%
  tail()

nycflights13::flights %>%
  arrange(desc(is.na(dep_time)), dep_time) %>%
  summarize(NA_dep_time = sum(is.na(dep_time)))

# filter --------------------------------------------------------------------------------------


# 데이터 중간에 껴있는 거만 뽑아내기
nycflights13::flights %>%
  filter(between(month, 7, 9))
# The expression between(x, left, right) is equivalent to x >= left & x <= right


# select --------------------------------------------------------------------------------------


# select 다루기 (특정 조건 만족하는 변수명만 추출)
nycflights13::flights %>%
  select(starts_with("dep_"), starts_with("arr_"))
nycflights13::flights %>%
  select(matches("^(dep|arr)_(time|delay)$"))
variables <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
nycflights13::flights %>%
  select(!!variables) # bang-bang operator
variables <- syms(c("dep_time", "dep_delay", "arr_time", "arr_delay"))
variables
nycflights13::flights %>%
  select(!!!variables) # bang-bang-bang operator
nycflights13::flights %>%
  select(contains("_time"), contains("arr_"))
nycflights13::flights %>%
  select(arr_delay, everything())


# mutate --------------------------------------------------------------------------------------
# %/% 정수 나누기
# %% 나머지
nycflights13::flights %>%
  mutate(dep_time_mins = (dep_time %/% 100*60 + dep_time %% 100) %% 1440,
         sched_dep_time_mins = (sched_dep_time %/% 100*60 + sched_dep_time %% 100) %% 1440) %>%
  select(dep_time, dep_time_mins, sched_dep_time,
  sched_dep_time_mins)

time2mins <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}
nycflights13::flights %>%
  mutate(dep_time_mins = time2mins(dep_time),
         sched_dep_time_mins = time2mins(sched_dep_time))

# 새 변수만 남기고 싶을 땐
flights %>%
  transmute(gain = arr_delay - dep_delay,
            hours = air_time / 60,
            gain_per_hours = gain /hours)


