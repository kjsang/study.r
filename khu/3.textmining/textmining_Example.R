if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, magrittr, # helper packages
  tidytext # 분석 패키지
  )

# 1. 파일 내용 중 ORA-XXXXX 변수 추출
read_table("reg_ex.txt") -> reg
reg %>% 
  rename(text = `Tue Oct 30 17:43:46 2012`) %>% 
  mutate(id = c(1:length(text))) %>% 
  select(id, text) %>% 
  group_by(id) %>% 
  mutate(reg_1 = text %>% 
           str_extract_all(regex("^ORA-[0-9]{5}")) %>% 
           unlist() %>% 
           paste(collapse = " ")) %>% 
  ungroup() %>% 
  filter(reg_1 != "") %>% 
  select(id, reg_1, text) -> reg_1
reg_1 %>% head(3)


# 2. 변수 중 ORA-XXXXX 만 추출한 변수 생성
reg_1 %>% 
  group_by(id) %>% 
  mutate(reg_2 = text %>% 
           str_extract_all(regex("^ORA-[0-9]{5}")) %>% 
           unlist() %>% 
           paste(collapse = " ")) %>% 
  ungroup() -> reg_2
reg_2 %>% 
  select(reg_2) %>% 
  head(3)

# # A tibble: 84 x 3
#       id reg_1     text                                                    
#    <int> <chr>     <chr>                                                   
# #  1   252 ORA-00313 ORA-00313: open failed for members of log group 1 of th…
#  2   253 ORA-00312 ORA-00312: online log 1 thread 1: '/app/oracle/oradata/…
#  3   254 ORA-27037 ORA-27037: unable to obtain file status                 
#  4   260 ORA-00313 ORA-00313: open failed for members of log group 1 of th…
#  5   261 ORA-00312 ORA-00312: online log 1 thread 1: '/app/oracle/oradata/…
#  6   262 ORA-27037 ORA-27037: unable to obtain file status                 
#  7   266 ORA-00313 ORA-00313: open failed for members of log group 1 of th…
#  8   267 ORA-00312 ORA-00312: online log 1 thread 1: '/app/oracle/oradata/…
#  9   268 ORA-27037 ORA-27037: unable to obtain file status                 
# 10   273 ORA-00313 ORA-00313: open failed for members of log group 2 of th…
# # … with 74 more rows

# 총 84개의 데이터가 있는 것을 확인할 수 있다.

# 3. ORA 키워드 빈도수 추출
reg %>% 
  rename(text = `Tue Oct 30 17:43:46 2012`) %>% 
  mutate(id = c(1:length(text))) %>% 
  unnest_tokens(text, text) %>% 
  count(text) %>% 
  arrange(desc(n)) %>% 
  filter(text == "ora")
# # A tibble: 1 x 2
#   text      n
#   <chr> <int>
# 1 ora     108

# ora 라는 글자는 총 108번 나온 것을 확인할 수 있다.

