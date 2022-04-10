# 10.연관분석(텍스트마이닝)
# install.packages("arules") 
library(arules) # 연관분석 패키지 설치




# 10.1 전리작업: dtm을  matrix 형식으로 변환
bible_dtm_m <- as.matrix(bible_dtm)
bible_dtm_m <- ifelse(bible_dtm_m > 0, 1, 0) # 단어가 1이상인 것을 0,1로 변환함
bible_dtm_m[1:5, 1:5]






# 10.2 바이너리 코드를 transactions으로 변환
# inspect는 여러 패키지에서 사용하므로 동시에 
# 사용할때는 특정패키지 명을 지정
bible_trans <-
  bible_dtm_m %>%
  as("transactions")

arules::inspect(bible_trans)






# 10.3 연관규칙 실행
# 그래프 확인
itemFrequencyPlot(bible_trans, 
                  support = 0.5,
                  topN = 20)

# apriori(data, minimum support, minimum confidence, and target)
bible_rules <- 
  apriori(bible_trans,
          parameter = list(supp= 0.7, 
                           conf = 0.8, 
                           target = "rules"))

bible_rules






# 10.4 규칙확인: lift가 높은 순서로 sorting
bible_rules_tb <- 
  bible_rules %>%
  inspect() %>%
  as_tibble(.name_repair = "unique")

str(bible_rules_tb)

# 특정 조건에 맞는 규칙 찾기

bible_rules_tb %>%
  arrange(desc(lift)) %>%
  filter (support >= 0.7, 
          confidence >= 0.8,
          lift >=1)

# 특정 items 찾기

bible_rules_tb %>%
  arrange(desc(lift)) %>% 
  filter(rhs == "{예수}")

bible_rules_tb %>%
  arrange(desc(lift)) %>% 
  filter(lhs == "{예수}")






# 10.5 그래프 그리기

# install.packages("arulesViz")
# install.packages("visNetwork")
library(visNetwork)
library(arulesViz)

plot(sort(bible_rules, 
          by = "support"), 
     method = "grouped")

# {item} → {item}: 지지도(support)
# color: 향상도(Lift)
plot(sort(bible_rules, 
          by = "lift")[1:20], 
     method = "graph", 
     engine = "htmlwidget")

