pacman::p_load(tidyverse)

a <- 3
if (a == 4) {
  print("에이는 삼입니다.")
} else {
  print(a)
}
a <- 5
if (a == 4) {
  print("A is four")
} else if (a == 3) {
  print("A is Three")
} else {
  print("A is neither 3 nor 4")
}
Score <- c(85, 90, 45, 76)
Score %>% 
  as_tibble() %>% 
  rename(score = value) %>% 
  mutate(GPA = ifelse(score >= 90, "A",
                      ifelse(score >= 80, "B",
                             ifelse(score >= 70, "C",
                                    "F")))) %>% 
  mutate(id = 1:length(score)) %>% 
  select(id, everything())

answer <- "여"
if (answer == "여") {
  print("안녕하십니까 여성회원님!")
} else if (answer == "남") {
  print("안녕하십니까 남성회원님!")
} else {
  print("안녕하십니까 회원님!")
}

ifelse(answer == "여", "안녕하십니까 여성회원님!", "안녕하십니까 남성회원님!")

fruit <- c("banana", "apple", "grape", "kiwi", "tomato")
fruit %>% 
  as_tibble() %>% 
  rename(fruit = value) %>% 
  mutate(id = 1:length(fruit)) %>% 
  select(id, everything())

for (i in 1:length(fruit)) {
  result = i
  print(result)
}
for (i in 1:length(fruit)) {
  print(fruit[i])
}

# 김주상 답안
AskAnswer <- function()
{
  answer <- readline("이름을 입력하십시오:")
  print(paste0(answer, "님이 입장하셨습니다."))
}
AskAnswer()
