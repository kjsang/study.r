if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  httr, urltools, RSelenium,
  rvest, jsonlite
  )

url <- "https://movie.naver.com/movie/bi/mi/basic.naver?code=168074"
url %>% 
  httr::GET() %>% 
  read_html() ->  my_html
my_html %>% 
  html_nodes("div.sc_view div.star_score em") %>% 
  html_text(trim = T)



# 코스피 가져오기 --------------------------------------------
if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  httr, xts,
  rvest, lubridate
  )

data = list()
tryCatch({
  # url 생성
  for (j in 1:20)
    #j=1
  {
    url = paste0('https://finance.naver.com/sise/sise_index_day.nhn?code=KOSPI&page=',
                 j)
    down_table = GET(url)
    table = read_html(down_table, encoding = "EUC-KR") %>%
      html_table(fill = TRUE)
    mytable = table[[1]]
    
    #NA 행 제거
    mytable = na.omit(mytable)
    rownames(mytable) = NULL
    data[[j]] = mytable
    Sys.sleep(0.01)
  }
}, error = function(e) {
  warning(paste0("Error"))
})

data2 = do.call(rbind, data)
KS11 = data2[order(data2$날짜),][,1:2]
KS11 %>% 
  mutate(날짜 = ymd(KS11$날짜)) %>% 
  mutate(체결가 = as.numeric(gsub("[+-,]","",
                               체결가)))


# 카카오 api 활용 ------------------------------------------

site_url = "https://dapi.kakao.com/v3/search/book"
query = "AI"  # 검색할 키워드 입력

query = URLencode(iconv(query,to="UTF-8"))  #URL 인코딩함수를 이용하여 iconv 함수를 통해 한글을 인코딩
query_str = sprintf("%s?target=title&query=%s",site_url,query)
query_str

kakao_api_key = "1ffe8c8878537d227cb71d25b984e9c5"  # 본인 키 입력
auth_key = sprintf("KakaoAK %s",kakao_api_key)


library(httr)
resp = GET(query_str,add_headers("Authorization"=auth_key))
resp

resp.str = as.character(resp)
resp.str %>% 
  fromJSON() %>% 
  data.frame() -> df
for (i in 1:length(df)){
  print(names(df[i]))
}
df2 = as.matrix(df)
write.csv(df2, "data.csv")

read_csv("data.csv") %>% 
  as_tibble() -> book
book
