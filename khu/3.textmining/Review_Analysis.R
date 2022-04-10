
# 패키지 로드 ----------------------------------------------

if (require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  tidytext,
  KoNLP,
  textclean
  )


# 데이터 불러오기 --------------------------------------------

read_csv("reviews_boss.csv") -> raw1
read_csv("reviews_rj.csv") -> raw2


# 사전 로드 ---------------------------------------------
useNIADic()
dic <- read_csv("knu_sentiment_lexicon.csv")

# 사전 단어별 태그 달기
dic %<>% # 사전 파일에 수정된 값을 저장
  mutate(sentiment = ifelse( # 태그가 들어갈 변수 지정
    polarity >= 1, "pos", # 긍정에는 pos 태그
    ifelse(polarity <= -1, "neg", "neu") # 부정 및 중립에는 각각 neg, neu를 태그한다.
    ))

# 감성사전의  sentiment 시각화
dic %>% 
  count(sentiment) %>% # 감성수준 별 카운트
  ggplot(aes(x = sentiment, y= n, fill = sentiment)) + # 시각화
  geom_col() +
  geom_text(aes(label = n), vjust = -1) + # 단어수 표시
  ylim(0, 10500) + # y축 범위 설정
  ylab("감성수준 별 빈도") +
  theme_gray(base_family = "AppleGothic")


# 데이터 전처리 ---------------------------------------------
raw1
# # A tibble: 997 x 2
#       X1 x                                                                                       
#    <dbl> <chr>                                                                                   
#  1     1 보스 베이비 2별점 - 총 10점 중10볼수록 보고싶어지는 애니ㅋㅋㅋ 신고                     
#  2     2 보스 베이비 2별점 - 총 10점 중10가족들이랑 보기 좋은 것 같아요^^ 신고                   
#  3     3 보스 베이비 2별점 - 총 10점 중10오랜만에 정말 재미있었던 영화 신고                      
#  4     4 보스 베이비 2별점 - 총 10점 중10이번 편도 역시 흥미진진ㅎㅎ 역시 보스베이비! 신고       
#  5     5 보스 베이비 2별점 - 총 10점 중10가족의 소중함이 무엇인지 가슴 깊이 느낄 수 있었던 영화 …
#  6     6 보스 베이비 2별점 - 총 10점 중6조금 지루하고 졸렸음.아이들은 한 명은 재미 있어했고한 명…
#  7     7 보스 베이비 2별점 - 총 10점 중10생각보다 더 재밌고 보는내내 흐뭇하게 봤습니다 신고      
#  8     8 보스 베이비 2별점 - 총 10점 중10아기들이 귀엽고 사랑스러워요~~ 신고                     
#  9     9 보스 베이비 2별점 - 총 10점 중93편도 기대가 되는 보스베이비! 신고                       
# 10    10 보스 베이비 2별점 - 총 10점 중10귀엽고 재밌고 스토리 탄탄하고 굿! 신고                  
# # … with 987 more rows

# 보스베이비 전처리
raw1 %>% 
  mutate(text = x %>% 
           str_replace_all("보스 베이비 2별점 - 총 10점 중", "") %>% # 불필요한 내용 삭제
           str_replace_all("^[0-9]", "") %>% # 별점 삭제
           str_replace_all(pattern = "신고", "") %>% # 맨 뒤 신고 삭제
           str_replace_all(pattern = "0", "")) %>% # 남은 숫자 삭제
  select(X1, text) -> boss_reply
boss_reply %>% 
  mutate(movie = "BossBaby") %>% 
  unnest_tokens(input = text,
                output = words,
                token = extractNoun,
                drop = F) %>% 
  select(movie, words) -> BossBaby
BossBaby
# # A tibble: 5,784 x 2
#    movie    words           
#    <chr>    <chr>           
#  1 BossBaby 재미있어요~♡    
#  2 BossBaby 나중            
#  3 BossBaby 겁              
#  4 BossBaby 웃김^ㅋ^ㅋ^ㅋ^ㅋ
#  5 BossBaby 기분전환        
#  6 BossBaby 겸              
#  7 BossBaby 친구            
#  8 BossBaby 한              
#  9 BossBaby 번              
# 10 BossBaby 꿀잼            
# # … with 5,774 more rows

# 랑종 전처리
raw2 %>% 
  mutate(text = x %>% 
           str_replace_all("랑종별점 - 총 10점 중", "") %>% 
           str_replace_all("^[0-9]", "") %>% 
           str_replace_all(pattern = "신고", "") %>% 
           str_replace_all(pattern = "0", "")) %>% 
  select(X1, text) -> RangJong_reply
RangJong_reply %>% 
  mutate(movie = "RangJong") %>% 
  unnest_tokens(input = text,
                output = words,
                token = extractNoun,
                drop = F) %>% 
  select(movie, words) -> RangJong
RangJong
# # A tibble: 13,764 x 2
#    movie    words               
#    <chr>    <chr>               
#  1 RangJong 랑종                
#  2 RangJong 나홍진              
#  3 RangJong 감독                
#  4 RangJong 님                  
#  5 RangJong 다음                
#  6 RangJong 작품                
#  7 RangJong 지루75놀램5웃김1답답
#  8 RangJong 1                   
#  9 RangJong 영화                
# 10 RangJong 왜만듦..            
# # … with 13,754 more rows

# 데이터 분석 ----------------------------------------------

# 각 영화별 단어 빈도수 추출 및 시각화
RangJong %>% 
  bind_rows(BossBaby) %>% 
  group_by(movie) %>% 
  dplyr::count(words) %>% 
  arrange(desc(n)) %>% 
  filter(str_count(words) >= 2) %>% 
  filter(!words %in% c("^ㅋ", "^ㅎ")) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
  ggplot(aes(x = reorder(words, n),
             y = n,
             fill = movie)) +
  geom_col() +
  coord_flip() + 
  theme_gray(base_family = "AppleGothic") +
  facet_wrap(~movie, scales = "free_y") +
  ylab("단어빈도수") +
  xlab("출현단어(명사)")


# 오즈비 분석 ------------------------------------------------

RangJong %>%
  bind_rows(BossBaby) %>%
  group_by(movie) -> movie_senti
movie_senti %>%
  count(words) %>%
  pivot_wider(
    names_from = movie,
    values_from = n,
    values_fill = list(n = 0)
  ) %>%
  mutate(
    ratio_BossBaby = ((BossBaby + 1) / (sum(BossBaby + 1))),
    ratio_RangJong = ((RangJong + 1) / (sum(RangJong + 1))),
    odds_ratio = ratio_BossBaby / ratio_RangJong
  ) -> ratio_movie
ratio_movie %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10) %>%
  arrange(-odds_ratio) %>%
  print(n = Inf)
# # A tibble: 20 x 6
#    words      BossBaby RangJong ratio_BossBaby
#    <chr>         <int>    <int>          <dbl>
#  1 베이비          115        0      0.0111   
#  2 보스            107        0      0.0103   
#  3 어른             58        0      0.00562  
#  4 아이들           49        0      0.00476  
#  5 귀여움           40        0      0.00391  
#  6 유쾌             23        0      0.00229  
#  7 드림웍스         21        0      0.00210  
#  8 애니메이션       36        1      0.00353  
#  9 귀엽             17        0      0.00171  
# 10 애니             16        0      0.00162  
# 11 연기              1       92      0.000191 
# 12 기괴              0       46      0.0000953
# 13 태국              0       57      0.0000953
# 14 나홍진            0       62      0.0000953
# 15 밍                0       64      0.0000953
# 16 다큐              0       81      0.0000953
# 17 곡성              0       91      0.0000953
# 18 좀비              0       94      0.0000953
# 19 공포영화          0      108      0.0000953
# 20 공포              0      122      0.0000953
# # … with 2 more variables: ratio_RangJong <dbl>,
# #   odds_ratio <dbl>

ratio_movie %>% 
  mutate(log_odds_ratio = log(odds_ratio)) %>% 
  group_by(Movie = ifelse(log_odds_ratio > 0, "BossBaby", "RangJong")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F) -> top10_log
top10_log %>% 
  arrange(-log_odds_ratio) %>% 
  select(words, log_odds_ratio, Movie) %>% 
  ggplot(aes(x = reorder(words, log_odds_ratio),
             y = log_odds_ratio,
             fill = Movie)) +
  theme_gray(base_family = "AppleGothic") +
  geom_col() +
  coord_flip() +
  ylab("odds비") +
  xlab("출현단어(명사)") 



# 감성분석 -------------------------------------------------
RangJong %>% 
  bind_rows(BossBaby) %>% 
  filter(str_count(words) >= 2) %>% 
  filter(!words %in% c("^ㅋ", "^ㅎ")) -> movie_sentiment
movie_sentiment %>% 
  rename(word = words) %>% 
  left_join(dic, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>%
  filter(sentiment %in% c("neg", "pos")) %>%
  filter(movie == "RangJong") %>% 
  group_by(sentiment) %>%
  count(word) %>%
  arrange(desc(n)) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
    ggplot(aes(
    x = fct_reorder(word, n),
    y = n,
    fill = sentiment
  )) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab("랑종 감성분석") +
  xlab(NULL) +
  ylim(0, 150) +
  theme(text = element_text(family = "AppleGothic")) +
  ggtitle("영화 \"랑종\"에 대한 감성분석 결과")

# 보스베이비 감성분석 결과
movie_sentiment %>% 
  rename(word = words) %>% 
  left_join(dic, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>%
  filter(sentiment %in% c("neg", "pos")) %>%
  filter(movie == "BossBaby") %>% 
  group_by(sentiment) %>%
  count(word) %>%
  arrange(desc(n)) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
    ggplot(aes(
    x = fct_reorder(word, n),
    y = n,
    fill = sentiment
  )) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab("보스베이비 감성분석") +
  xlab(NULL) +
  ylim(0, 100) +
  theme(text = element_text(family = "AppleGothic")) +
  ggtitle("영화 \"보스베이비\"에 대한 감성분석 결과")

# 2. 감정분석: 로그 오즈비 값으로 비교
# 3. 막대그래프

