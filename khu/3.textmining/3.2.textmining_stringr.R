if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, magrittr, # helper packages
  tidytext # 분석 패키지
  )
myword <- c("1번째는 통일 입니다.", "2번째는 가족 입니다.", "3번째는 사랑입니다.")
myword %>% 
  as_tibble() %>% 
  rename(text = value) %>% 
  mutate(word = text %>% 
           str_extract_all("입니다")) %>% #
  unnest(word)

myword %>% 
  as_tibble() %>% 
  rename(text = value) %>% 
  mutate(word = text %>%
           str_extract_all("입니다", simplify = T)) # 바로 행렬로 뽑고싶을 때

myword %>%
  as_tibble() %>%
  rename(text = value) %>%
  mutate(word = text %>%
           str_extract_all("[[:alpha:]]{1}(니다)")) %>% 
  unnest(word)

myword %>%
  as_tibble() %>%
  rename(text = value) %>%
  mutate(word = text %>%
           str_extract_all("[[:alpha:]]{1}(니다)", simplify = T),
         locate = text %>% 
           str_locate_all("입니다"))

myword %>%
  as_tibble() %>%
  rename(text = value) %>%
  mutate(word = text %>%
           str_extract_all("[[:alpha:]]{1}(니다)", simplify = T),
         change = text %>% 
           str_replace_all("입니다", "일까요"),
         n = text %>% 
           str_length(),
         number = 1:length(text),
         alphabet = c("A", "B", "C"),
         connect = str_c(number, alphabet, sep = ""))


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, magrittr, # helper packages
  tidytext # 분석 패키지
  )

c("R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing.[6] The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, data mining surveys, and studies of scholarly literature databases show substantial increases in popularity; as of June 2019, R ranks 22nd in the TIOBE index, a measure of popularity of programming languages.",
  "A GNU package, source code for the R software environment is written primarily in C, Fortran and R itself,[12] and is freely available under the GNU General Public License. Pre-compiled binary versions are provided for various operating systems. Although R has a command line interface, there are several graphical user interfaces, such as RStudio, an integrated development environment.") -> raw

# 정규표현식을 이용해 첫 문자가 대문자로 시작되는 단어 찾아내기
raw %>% 
  as_tibble() %>% 
  rename(text = value) %>% 
  mutate(paragraph = 1:length(text),
         word = text %>%
           str_split(" ", simplify = F)) %>% 
  unnest(word) %>% 
  select(-text) %>% 
  mutate(LETTER = word %>% 
           str_extract_all("^[A-Z]+")) %>% 
  unnest(LETTER) %>% 
  select(word, paragraph)
# # A tibble: 25 x 2
#    word          paragraph
#    <chr>             <int>
#  1 R                     1
#  2 R                     1
#  3 Foundation            1
#  4 Statistical           1
#  5 Computing.[6]         1
#  6 The                   1
#  7 R                     1
#  8 Polls,                1
#  9 June                  1
# 10 R                     1
# # … with 15 more rows

# 문단1과 문단2에 해당하는 단어 빈도 조사
raw %>% 
  as_tibble() %>% 
  rename(text = value) %>%  
  mutate(paragraph = 1:length(text),
         word = text %>%
           str_split(" ", simplify = F)) %>% 
  unnest(word) %>% 
  group_by(paragraph) %>% 
  count(word) %>% 
  filter(!is.na(word)) %>% 
  arrange(desc(n))
# # A tibble: 91 x 2
#    word         n
#    <chr>    <int>
#  1 and          7
#  2 R            7
#  3 for          5
#  4 is           4
#  5 of           4
#  6 the          4
#  7 a            3
#  8 data         3
#  9 in           3
# 10 software     3
# # … with 81 more row



# 첫 문자가 대문자로 시작되는 단어 위치 파악 후 시작위치와 종료위치를 데이터로 정리하기
raw %>% 
  as_tibble() %>% 
  rename(text = value) %>% 
  mutate(paragraph = 1:length(text),
         word = text %>%
           str_split(" ", simplify = F)) %>% 
  unnest(word) %>% 
  select(-text) %>% 
  mutate(LETTER = word %>% 
           str_extract_all("^[A-Z]+"),
         sequence = 1:length(word)) %>% 
  select(paragraph, sequence, everything()) %>% 
  unnest(LETTER) %>% 
  select(-LETTER)
# # A tibble: 25 x 3
#    paragraph sequence word         
#        <int>    <int> <chr>        
#  1         1        1 R            
#  2         1       18 R            
#  3         1       19 Foundation   
#  4         1       21 Statistical  
#  5         1       22 Computing.[6]
#  6         1       23 The          
#  7         1       24 R            
#  8         1       41 Polls,       
#  9         1       58 June         
# 10         1       60 R            
# # … with 15 more rows

# 각 단어의 위치를 표기하였다.

raw %>% 
  as_tibble() %>% 
  rename(text = value) %>% 
  mutate(paragraph = 1:length(text),
         word = text %>%
           str_split(" ", simplify = F)) %>% 
  unnest(word) %>% 
  select(-text) %>% 
  mutate(LETTER = word %>% 
           str_extract_all("^[A-Z]+"),
         sequence = 1:length(word)) %>% 
  select(paragraph, sequence, everything()) %>% 
  unnest(LETTER) %>% 
  select(-LETTER) -> raw_word
raw %>% 
  as_tibble() %>% 
  rename(text = value) %>% 
  str_locate(pattern = raw_word$word) -> raw_locate
raw_word %>% 
  bind_cols(as_tibble(raw_locate))

# 단어리스트 변수화 후 데이터에 포함하고 문자수 계산하여 데이터에 포함하기

raw %>% 
  as_tibble() %>% 
  rename(text = value) %>% 
  mutate(paragraph = 1:length(text),
         word = text %>%
           str_split(" ", simplify = F)) %>% 
  unnest(word) %>% 
  select(-text) %>% 
  mutate(LETTER = word %>% 
           str_extract_all("^[A-Z]+"),
         sequence = 1:length(word)) %>% 
  select(paragraph, sequence, everything()) %>% 
  unnest(LETTER) %>% 
  select(-LETTER) %>% 
  mutate(count_word_length = str_length(word))
# # A tibble: 25 x 4
#    paragraph sequence word          count_word_length
#        <int>    <int> <chr>                     <int>
#  1         1        1 R                             1
#  2         1       18 R                             1
#  3         1       19 Foundation                   10
#  4         1       21 Statistical                  11
#  5         1       22 Computing.[6]                13
#  6         1       23 The                           3
#  7         1       24 R                             1
#  8         1       41 Polls,                        6
#  9         1       58 June                          4
# 10         1       60 R                             1
# # … with 15 more rows