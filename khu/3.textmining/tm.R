
# 텍스트 데이터 전처리 -----------------------------------------

# 학습목표: 텍스트 데이터 전처리 방법에 대해 학습
# 기본 키워드 분석 및 워드클라우드 분석
# tm 라이브러리 사용법 
# 텍스트를 DTM 으로 변형하는 방법

.libPaths()
# 패키지 적재 및 설정 -----------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr, wordcloud,
  KoNLP
  )
# 패키지 설정: KoNLP
buildDictionary(ext_dic = "woorimalsam")
useNIADic()
# 데이터 불러오기 --------------------------------------------

readLines("practice.txt") -> news


# 데이터 구성하기 --------------------------------------------

news %>% as_tibble() %>% # 티블 형태로 변경
  rename(text = value) %>% # 데이터 이름 붙여주기
  mutate(id = 1:length(text)) -> news # 기사 id 붙여주기

# 명사만 추출하였다.
news %>% 
  select(id, text) %>% 
  mutate(noun = text %>% 
           extractNoun()) %>% # 명사만 추출한 데이터를 noun 변수에 적재
  unnest(noun) %>% # 리스트를 벡터형태로 변환
  select(id, noun) -> news_noun

# 빈도표 생성
theme_set(theme_gray(base_family='AppleGothic'))
news_noun %>% 
  count(noun) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  ggplot(aes(x = fct_reorder(noun, -n), y = n)) +
  geom_bar(aes(fill = noun), stat = "identity") +
  xlab("명사") +
  ylab("빈도")


# 경주여행 지식인 분석 -------------------------------------------
# 데이터 적재 및 설정

readLines("경주여행_지식인_2016.txt") -> raw_wordcloud
KoNLP::useSejongDic()
raw_wordcloud %>% 
  as_tibble() -> data1
data1 %>% 
  rename(text = value) %>% 
  mutate(pretext = text %>% 
           str_replace_all("[^[:alpha:][:digit:]]", " ") %>% 
           str_replace_all("\\d+", "") %>% 
           str_replace_all("스프링", "스프링돔") %>% 
           str_replace_all("파크", "워터파크") %>% 
           str_replace_all("\\^", "") %>% 
           str_replace_all("교촌|마을|한옥|교촌한옥마을", "교촌한옥마을") %>% 
           str_replace_all("주상|절리|주상절리", "주상절리") %>% 
           str_replace_all("보문|보문단지|보문관광단지", "보문관광단지") %>% 
           str_replace_all(paste(c("달동네", "추억"), collapse = "|"), "추억의 달동네") %>% 
           str_replace_all(paste(c("한우", "떡갈비"), collapse = "|"), "한우수제떡갈비") %>% 
           str_replace_all(paste(c("게스트", "하우스"), collapse = "|"), "게스트하우스") %>% 
           str_replace_all(paste(c("월성", "반월성"), collapse = "|"), "반월성") %>% 
           str_replace_all(paste(c("맛집이", "맛집"), collapse = "|"), "맛집") %>% 
           str_replace_all("교리|계란지단|김밥|교리김밥", "교리김밥") %>% 
           str_replace_all(paste(c("천마", "천마총"), collapse = "|"), "천마총") %>% 
           str_replace_all(paste(c("테디베어박물관", "테디베어", "박물관"), collapse = "|"), "테디베어박물관") %>% 
           str_replace_all("월드", "월드엑스포") %>% 
           str_replace_all("순두부", "멧돌순두부") %>% 
           str_replace_all(paste(c("현대", "밀면"), collapse = "|"), "현대밀면") %>% 
           str_replace_all("이조", "이조한정식") %>% 
           str_replace_all("블루", "블루워터파크") %>% 
           extractNoun()) %>% 
  unnest(pretext) %>% 
  select(pretext) -> pretext
pretext %>% 
  count(pretext) %>% 
  arrange(desc(n)) %>% 
  filter(n > 5) %>% 
  head(100) %>% 
  as.data.frame() -> wc_data
RColorBrewer::brewer.pal(7, "Set2") -> palete
par(family='AppleGothic')
wordcloud(wc_data$pretext, freq = wc_data$n,
          scale = c(5,1), rot.per = 0.25,
          min.freq = 28, random.order = F,
          random.color = T, colors = palete)


# 힙합노래 ------------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr, wordcloud,
  KoNLP
  )

# 패키지 설정: KoNLP
buildDictionary(ext_dic = "woorimalsam")
useNIADic()
# 데이터 불러오기 --------------------------------------------

readLines("hiphop11.txt", encoding = "UTF-8") -> hiphop


# 데이터 구성하기 --------------------------------------------

hiphop %>% as_tibble() %>% # 티블 형태로 변경
  rename(text = value) %>% # 데이터 이름 붙여주기
  mutate(id = 1:length(text)) -> hiphop # 기사 id 붙여주기

# 명사만 추출하였다.
hiphop %>% 
  select(id, text) %>% 
  mutate(noun = text %>% 
           extractNoun()) %>% # 명사만 추출한 데이터를 noun 변수에 적재
  unnest(noun) %>% # 리스트를 벡터형태로 변환
  select(id, noun) -> hiphop_noun

# 빈도표 생성
theme_set(theme_gray(base_family='AppleGothic'))
hiphop_noun %>% 
  count(noun) %>% 
  arrange(desc(n)) %>% 
  filter(str_length(noun) > 1) %>% 
  head(20) %>% 
  ggplot(aes(x = fct_reorder(noun, -n), y = n)) +
  geom_bar(aes(fill = noun), stat = "identity") +
  xlab("명사") +
  ylab("빈도")

hiphop_noun %>% 
  count(noun) %>% 
  arrange(desc(n)) %>% 
  filter(str_length(noun) > 1) -> wc_hiphop

wordcloud(wc_hiphop$noun, freq = wc_hiphop$n,
          scale = c(5,1), rot.per = 0.25,
          min.freq = 28, random.order = F,
          random.color = T, colors = palete)


# tm 패키지 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr, wordcloud,
  KoNLP, tm, SnowballC,
  tidytext
  )

crude %>% 
  tidy() %>% 
  mutate(author = ifelse(id == 127, "홍길동", author))
           

  
