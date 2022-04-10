
# 패키지 설치 ----------------------------------------------


if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr, wordcloud2,
  KoNLP, hrbrthemes
  )


# 데이터 로드 ----------------------------------------------

readxl::read_xls("article_platform.xls") -> raw

# 키워드 살펴보기
raw %>% select(keyword)
# # A tibble: 53 x 1
#    keyword                                                      
#    <chr>                                                        
#  1 보건의료 빅데이터, 의료 플랫폼, 4차 산업혁명,                
#  2 공유경제, 승차공유, 플랫폼운송기업, 카풀, 우버,              
#  3 COVID-19, 플랫폼 정부, 공공-민간 협력, 공공데이터 개방,      
#  4 도시재생, 뉴딜사업, 플랫폼 설계, 거버넌스, 모듈화, 네트워크, 
#  5 스마트시티, 플랫폼, 플랫폼 거버넌스, 사례연구,               
#  6 네트워크 거버넌스, 플랫폼 정부, 광화문 1번가,                
#  7 딜레마 이론, 무시된 딜레마, 여객운송 규제, 모빌리티 플랫폼,  
#  8 타다, 모빌리티, 4차 산업, 택시, 규제, 플랫폼,                
#  9 도시재생, 젠트리피케이션, 예술경관, 예술가지원플랫폼, 혹스턴,
# 10 팬데믹, 코로나 19, 물리보안, PSIM, PPS Timing Diagram,       
# # … with 43 more rows

# 키워드 전처리
raw %>%
  select(NO, title, keyword, year, field) %>%
  mutate(keyword_word = keyword %>%
           str_split(pattern = ", ")) %>% # 콘마로 키워드 분할
  unnest(keyword_word) %>%
  mutate(keyword_word = keyword_word %>%
           str_replace_all(",", "") %>% # 남아있는 콘마 제거
           str_replace_all(" ", "")) %>%  # 띄어쓰기 없애기 (일례로 코로나19, 코로나 19 를 같은 키워드로 취급하기 위함)
  mutate(keyword_word = keyword_word %>%
           str_replace_all("COVID-19", "코로나19")) -> data
data %>%
  select(NO, keyword_word) %>%
  count(keyword_word) %>%
  arrange(desc(n)) -> view
view
# # A tibble: 231 x 2
#    keyword_word     n
#    <chr>        <int>
#  1 네트워크         4
#  2 코로나19         3
#  3 4차산업혁명      2
#  4 거버넌스         2
#  5 공유경제         2
#  6 도시재생         2
#  7 디지털성범죄     2
#  8 사물인터넷       2
#  9 스마트시티       2
# 10 플랫폼           2
# # … with 221 more rows

# 시각화 -------------------------------------------------

par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))

# 연도별 출간 논문 수
raw %>%
  select(NO, title, keyword, year, field) %>%
  mutate(year = year %>% 
           as.Date("%Y") %>% 
           lubridate::year(.)) %>% 
  group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) + 
  ggtitle("연도별 출판논문 수") +
  xlab("연도") +
  ylab("출판논문수") +
  ylim(0, 30) +
  geom_text(aes(label = n, vjust = -1))


# 분야별 출간논문수
raw %>%
  select(NO, title, keyword, year, field) %>% 
  group_by(field) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = fct_reorder(field, n), y = n)) +
  geom_col(aes(fill = field)) +
  coord_flip() +
  xlab("세부분야") +
  ylab("출간논문수") +
  geom_text(aes(label = n, vjust = 0.1,hjust = -0.1))


# 키워드 분석=
raw %>%
  select(NO, title, keyword, year, field) %>%
  mutate(keyword_word = keyword %>% 
           str_split(pattern = ", ")) %>% # 콘마로 키워드 분할
  unnest(keyword_word) %>%
  mutate(keyword_word = keyword_word %>%
           str_replace_all(",", ""))  -> data2

data2 %>%
  mutate(word = keyword_word %>%
           str_split(pattern = " ")) %>%
  unnest(word) %>%
  count(word) %>%
  arrange(desc(n)) -> data_visual
data_visual
# # A tibble: 278 x 2
#    word         n
#    <chr>    <int>
#  1 플랫폼      11
#  2 네트워크     8
#  3 거버넌스     7
#  4 디지털       5
#  5 4차          3
#  6 공유경제     3
#  7 도시         3
#  8 보호         3
#  9 산업혁명     3
# 10 테러         3
# # … with 268 more rows


# 빈도표 생성
data_visual %>%
  filter(n >= 3) %>%
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_col(aes(fill = word)) +
  coord_flip() +
  xlab("키워드") +
  ylab("빈도수") +
  geom_text(aes(label = n, vjust = 0.1,hjust = -0.2))
# # A tibble: 11 x 2
#    word         n
#    <chr>    <int>
#  1 플랫폼      11
#  2 네트워크     8
#  3 거버넌스     7
#  4 디지털       5
#  5 4차          3
#  6 공유경제     3
#  7 도시         3
#  8 보호         3
#  9 산업혁명     3
# 10 테러         3
# 11 해킹         3

# 워드클라우드
data_visual %>% 
  filter(n >= 2) %>% 
  wordcloud2()

# 분야별 키워드
data2 %>% 
  select(NO, keyword_word, field) %>% 
  group_by(field) %>% 
  mutate(word = keyword_word %>%
           str_split(pattern = " ")) %>%
  unnest(word) %>%
  count(word) %>% 
  arrange(desc(n)) %>%
  filter(n >= 2) %>% 
  filter(field == "기타행정학") %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n, fill = word)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  xlab("키워드") +
  ylab("출간논문수(기타행정학)")

# 파이차트
data2 %>% 
  select(NO, keyword_word, field) %>% 
  group_by(field) %>% 
  mutate(word = keyword_word %>%
           str_split(pattern = " ")) %>%
  unnest(word) %>%
  count(word) %>% 
  arrange(desc(n)) %>%
  filter(n >= 3) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n, fill = word)) +
  geom_bar(stat = "identity") +
  coord_polar() +
  xlab("키워드") +
  ylab("출간논문수")

