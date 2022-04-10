if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, magrittr, # helper packages
  tidytext, KoNLP # 분석 패키지
  )

temp.text <- 
  tibble(lineid = 1:3,
         text=c("한국(韓國), 조선(朝鮮), 또는 코리아(Korea)는 동아시아에 위치한 지역 또는 헌법상의 국가로, 현대사에서는 한반도의 대한민국과 조선민주주의인민공화국을 통틀어 이르는 말이다.",
         "근현대사에서 한국은 고종이 수립한 대한제국을 일컫는 말이었다.",
         "넓은 의미로 한국은 고조선 이후 한반도에서 설립된 여러 한민족의 국가를 통칭하는 말이다. 한국의 역사를 한국사라고 한다."))
temp.text

stopping_ko_end=regex("입니다$|이다$")
stopping_ko=tibble(word=c('이','가','은','는'))

temp.text %>%
  group_by(lineid) %>%
  mutate(
    text_matched = SimplePos09(text) %>%
      unlist() %>%
      paste(collapse = ' ') %>%
      str_extract_all(regex('[^\\s]+/N')) %>% #POS에서 명사부만 뽑겠습니다.
      unlist() %>%
      paste(collapse = ' ') %>%
      str_remove_all('/N') %>% #태그는 지웁니다.
      str_remove_all(stopping_ko_end)
  ) %>%
  ungroup() %>%
  unnest_tokens(word, text_matched) %>%
  select(-text) %>%
  anti_join(stopping_ko)



# 수업내용 ------------------------------------------------


read_table("reg_ex.txt") -> reg
reg %>% 
  rename(text = `Tue Oct 30 17:43:46 2012`) %>% 
  mutate(id = c(1:4279)) %>% 
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

reg %>% 
  rename(text = `Tue Oct 30 17:43:46 2012`) %>% 
  mutate(id = c(1:4279)) %>% 
  unnest_tokens(text, text) %>% 
  count(text) %>% 
  arrange(desc(n)) %>% 
  filter(text == "ora")
# # A tibble: 1 x 2
#   text      n
#   <chr> <int>
# 1 ora     108

# ora 라는 글자는 총 108번 나온 것을 확인할 수 있다.
