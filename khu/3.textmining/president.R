pacman::p_load(
  tidyverse, magrittr,
  tidytext, KoNLP,
  textclean
)
useNIADic()

raw_moon <- readLines("speech_moon.txt")
raw_moon %<>% 
  as_tibble() %>% 
  mutate(president = "moon")
raw_park <- readLines("speech_park.txt")
raw_park %<>%
  as_tibble() %>% 
  mutate(president = "park")
raw_moon %>% 
  bind_rows(raw_park) %>% 
  rename(text = value) %>% 
  select(president, text) %>% 
  mutate(words = text %>% 
           str_replace_all("[^가-힣]", " ") %>% 
           str_squish() %>% 
           extractNoun()) %>% 
  unnest(words) %>% 
  select(president, words) %>% 
  group_by(president) %>% 
  count(words) %>% 
  filter(str_count(words) > 1) -> freq_president
freq_president %>% 
  slice_max(n, n = 10, with_ties = F) -> top10

top10 %>% 
  ggplot(aes(x = reorder(words, n),
             y = n,
             fill = president)) +
  geom_col() +
  theme_gray(base_family = "AppleGothic") +
  coord_flip() +
  facet_wrap(~president, scales = "free_y") +
  ylab("단어빈도수") +
  xlab("출현단어(명사)") +
  ylim(0, 80)



freq_president %>%
  pivot_wider(
    names_from = president, # col 이름
    values_from = n, # 값을 어디서 가져올 것인가?
    values_fill = list(n = 0)  # NA 값을 0으로 채우기
  ) -> freq_wide_president

# 오즈비 구하기
freq_wide_president %>%
  mutate(ratio_moon = ((moon + 1) / (sum(moon + 1))),
         ratio_park = ((park + 1) / (sum(park + 1))),
         odds_ratio = ratio_moon/ratio_park)

# 오즈비 높거나 낮은 단어 추출
freq_wide_president %>%
  mutate(ratio_moon = ((moon + 1) / (sum(moon + 1))),
         ratio_park = ((park + 1) / (sum(park + 1))),
         odds_ratio = ratio_moon/ratio_park) %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10) %>% 
  arrange(-odds_ratio) %>% 
  print(n = Inf)

freq_wide_president %>% 
    mutate(ratio_moon = ((moon + 1) / (sum(moon + 1))),
         ratio_park = ((park + 1) / (sum(park + 1))),
         odds_ratio = ratio_moon/ratio_park) %>% 
  filter(moon >= 5 & park >= 5) %>% 
  arrange(abs(1 - odds_ratio)) %>% 
  head(10)

freq_wide_president %>%
  mutate(
    ratio_moon = ((moon + 1) / (sum(moon + 1))),
    ratio_park = ((park + 1) / (sum(park + 1))),
    odds_ratio = ratio_moon / ratio_park
  ) %>%
  mutate(log_odds_ratio = log(odds_ratio)) %>% 
  arrange(abs(log_odds_ratio))# 비중이 비슷한 단어

freq_wide_president %>%
  mutate(
    ratio_moon = ((moon + 1) / (sum(moon + 1))),
    ratio_park = ((park + 1) / (sum(park + 1))),
    odds_ratio = ratio_moon / ratio_park
  ) %>%
  mutate(log_odds_ratio = log(odds_ratio)) %>% 
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F) -> top10_log
top10_log %>% 
  arrange(-log_odds_ratio) %>% 
  select(words, log_odds_ratio, president) %>% 
  ggplot(aes(x = reorder(words, log_odds_ratio),
             y = log_odds_ratio,
             fill = president)) +
  theme_gray(base_family = "AppleGothic") +
  geom_col() +
  coord_flip() +
  ylab("odds비") +
  xlab("출현단어(명사)") 
  
# tf-idf 구하기
read_csv("speeches_presidents.csv") -> raw_speeches
raw_speeches %>% 
  mutate(value = value %>% 
           str_replace_all("[^[가-힣]]", " ") %>% 
           str_squish() %>%
           extractNoun()) %>% 
  unnest(value) %>%
  filter(str_count(value) > 1) %>% 
  group_by(president) %>% 
  count(value) %>% 
  arrange(desc(n)) -> freq
freq %>% 
  bind_tf_idf(term = value,           # 단어
              document = president,  # 텍스트 구분 변수
              n = n) %>%             # 단어 빈도
  arrange(tf_idf) %>% 
  filter(president == "문재인")


# 감성분석 ------------------------------------------------
dic <- read_csv("knu_sentiment_lexicon.csv")
dic %>% 
  filter(!str_detect(word, "[가-힣]")) %>% 
  arrange(word)
dic %<>%
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu")))
dic %>% 
  count(sentiment)
raw <- read_csv("news_comment_parasite.csv")
raw %>% 
  mutate(id = 1:length(reply)) %>% 
  mutate(reply = reply %>%
           replace_html() %>% 
           str_squish()) %>% 
  select(id, everything())-> new_comment
new_comment %>% glimpse()

# 토큰화
new_comment %>% 
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F) %>% 
  select(id, word) -> word_senti
word_senti %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>%
  filter(sentiment %in% c("neg", "pos")) %>%
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
  labs(x = NULL) +
  ylim(0, 750) +
  theme(text = element_text(family = "AppleGothic"))

word_senti %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>%
  filter(sentiment %in% c("neg", "pos")) %>%
  group_by(sentiment) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 10, with_ties = F) %>% 
  ggplot(aes(x = sentiment, y = n,
             fill = sentiment)) +
  geom_col()

# 감정 범주별 단어 빈도
word_senti %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  filter(str_count(word) >= 2) %>% 
  filter(sentiment %in% c("neg", "pos")) %>% 
  count(sentiment, word, sort = T)

# 감성사전 수정
dic %<>%
  mutate(polarity = ifelse(word %in% c("소름", "미친"), 2, polarity)) %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu")))
dic %>% 
  filter(word == "소름")
new_comment %>% 
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F) %>% 
  select(id, word) -> word_senti
word_senti %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_senti %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  filter(str_count(word) >= 2) %>% 
  filter(sentiment %in% c("neg", "pos")) %>% 
  count(sentiment, word, sort = T)
