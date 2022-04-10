# 9.Co-occurence
library(widyr)

# 9.1 동시출현단어 분석 (같은 문서)
youth_pairs <- youth_tb %>%
  pairwise_count(item = word, 
                 feature = no, 
                 sort = TRUE)
youth_pairs






# 9.2 단어 상관관계분석(같은 문서)
# 동시출현단어는 가장 많은 빈도수를 의미하는 반면,
# 상관관계(phi)는 두개 단어가 동시에 나오거나, 나오지 않을 확률
# ppt 설명

youth_cors <- youth_tb %>%
  pairwise_cor(item = word, 
               feature = no, 
               sort = TRUE)
youth_cors


# 추가: correlation = 1 제거
youth_cors <- youth_cors %>%
  filter(correlation < 1)

youth_cors






# 9.3 youth와 가장 많이 나타나는 단어와 상관관계가 높은 단어
youth_pairs %>%
  filter(item1 == "youth")

youth_cors %>%
  filter(item1 == "youth")






# 9.4 SNA 그래프
library(igraph)
library(ggraph)

set.seed(123)

# frequency(n)
youth_pairs %>%
  filter(n > 1) %>% 
  igraph::graph_from_data_frame()

# 그래프1
youth_pairs %>%
  filter(n > 10) %>% 
  igraph::graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# correlation
youth_cors %>%
  filter(correlation > 0.80) %>%
  igraph::graph_from_data_frame()


# 그래프2
arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))

youth_cors %>%
  filter(correlation > 0.80) %>%
  igraph::graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation,
                     edge_width = correlation), 
                 edge_color = "darkred",
                 show.legend = FALSE,
                 arrow = arrow, 
                 end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "lightblue", 
                  size = 5) +
  geom_node_text(aes(label = name), 
                 repel = TRUE,
                 point.padding = unit(0.2, "lines")) +    # repel =TRUE: 텍스트가 겹치지 않게 
  theme_void() # 격자 없애기


