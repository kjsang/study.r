# API key: 

pacman::p_load(tidyverse, magrittr,
               rvest, curl,
               N2H4,
               rtweet, igraph,
               RCurl
               )


# 박박사님 패키지 --------------------------------------------

N2H4::getAllComment("https://news.naver.com/main/read.naver?m_view=1&mode=LSD&mid=shm&sid1=100&oid=123&aid=0002250409") %>% 
  write_csv("comment.csv")
read_csv("comment.csv") -> comm
comm %>%
  mutate(id = 1:length(contents),
         text = contents) %>%
  select(id, text) %>%
  mutate(
    word = KoNLP::SimplePos09(text) %>%
      unlist() %>%
      paste(collapse = ' ') %>%
      str_extract_all(regex('[^\\s]+/N')) %>% #POS에서 명사부만 뽑겠습니다.
      unlist() %>%
      paste(collapse = ' ') %>%
      str_remove_all('/N')
  ) %>% 
  ungroup() %>%
  tidytext::unnest_tokens(word, word) %>%
  select(-text) 
  

# 영화 평점 -----------------------------------------------
pacman::p_load(tidyverse, magrittr,
               rvest, curl,
               N2H4,
               httr
               )
url <- "https://movie.naver.com/movie/sdb/rank/rmovie.naver?sel=pnt&date=20210715"


httr::GET(url) %>%
  rvest::read_html() %>%
  html_nodes(".tit5") %>%
  html_text(trim = T) %>%
  bind_cols(httr::GET(url) %>%
              rvest::read_html() %>%
              html_nodes(".point") %>%
              html_text(trim = T)) %>% 
  rename(title = ...1, 
         point = ...2)

url <- "https://movie.naver.com/movie/bi/mi/point.naver?code=134963#pointAfterTab"


httr::GET(url) %>%
  rvest::read_html() %>%
  html_nodes(".main_score") %>%
  html_text(trim = T)


httr::GET(url) %>%
  rvest::read_html() %>%
  html_nodes(".st_off") %>%
  html_text(trim = T) %>%
  as_tibble() %>%
  mutate(id = 1:length(value)) -> 뭐지
뭐지


# rvest
for (i in 1:40) {
  url = paste0(
    "https://movie.naver.com/movie/sdb/rank/rmovie.naver?sel=pnt&date=20210715&page=",
    i
  )
httr::GET(url) %>%
  rvest::read_html() %>%
  html_nodes(".tit5") %>%
  html_text(trim = T) %>%
  bind_cols(httr::GET(url) %>%
              rvest::read_html() %>%
              html_nodes(".point") %>%
              html_text(trim = T)) %>% 
  rename(title = ...1, 
         point = ...2) -> result
}

for (i in 1:2) {
  url = paste0(
    "https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=168074&target=after&page=",
    i
  )
httr::GET(url) %>%
  rvest::read_html() %>%
  html_nodes(".tit5") %>%
  html_text(trim = T) %>%
  bind_cols(httr::GET(url) %>%
              rvest::read_html() %>%
              html_nodes(".point") %>%
              html_text(trim = T)) %>% 
  rename(title = ...1, 
         point = ...2) -> result
}



html <- "https://movie.naver.com/movie/sdb/rank/rmovie.naver?sel=pnt&date=20210715"

html %>% 
  html_elements(".a") %>% 
  html_text2()



# RCurl -----------------------------------------------

page <- 
