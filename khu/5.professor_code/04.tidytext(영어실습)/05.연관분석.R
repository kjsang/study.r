# 10.연관분석(텍스트마이닝)
# install.packages("arules") 
library(arules) # 연관분석 패키지 설치






# 10.1 전리작업: dtm을  matrix 형식으로 변환
dtm_m <- as.matrix(dtm)
dtm_m <- ifelse(dtm_m > 0, 1, 0) # 단어가 1이상인 것을 0,1로 변환함
dtm_m[1:5, 1:5]





# 10.2 바이너리 코드를 transactions으로 변환
# inspect는 여러 패키지에서 사용하므로 동시에 
# 사용할때는 특정패키지 명을 지정
youth_trans <-
  dtm_m %>%
  as("transactions")

arules::inspect(youth_trans)






# 10.3 연관규칙 실행
# 그래프 확인
itemFrequencyPlot(youth_trans, 
                  support = 0.5,
                  topN = 20)

# apriori(data, minimum support, minimum confidence, and target)
youth_rules <- 
  apriori(youth_trans,
          parameter = list(supp= 0.8, 
                           conf = 0.9, 
                           target = "rules"))

youth_rules






# 10.4 규칙확인: lift가 높은 순서로 sorting
rules_tbl <- 
  youth_rules %>%
  inspect() %>%
  as_tibble(.name_repair = "unique")

str(rules_tbl)

# 특정 조건에 맞는 규칙 찾기

rules_tbl %>%
  arrange(desc(lift)) %>%
  filter (support >= 0.5, 
          confidence >= 0.7,
          lift >=1)

# 특정 items 찾기

rules_tbl %>%
  arrange(desc(lift)) %>% 
  filter(rhs == "{youth}")






# 10.5 그래프 그리기

# install.packages("arulesViz")
# install.packages("visNetwork")
library(visNetwork)
library(arulesViz)

plot(sort(youth_rules, 
          by = "support"), 
     method = "grouped")

# {item} → {item}: 지지도(support)
# color: 향상도(Lift)
plot(sort(youth_rules, 
          by = "lift")[1:20], 
     method = "graph", 
     engine = "htmlwidget")

