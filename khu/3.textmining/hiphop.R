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
  mutate(id = 1:length(text)) -> hiphop # 가사 id 붙여주기

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
par(family = "AppleGothic")
wordcloud(wc_hiphop$noun, freq = wc_hiphop$n,
          scale = c(5,1), rot.per = 0.25,
          min.freq = 28, random.order = F,
          random.color = T, colors = palete)
crude %>% 
  tidy()
