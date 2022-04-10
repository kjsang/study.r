# 9.Co-occurence
library(widyr)






# 9.1 동시출현단어 분석 (같은 문서)
bible_pairs <- bible_tb %>%
  pairwise_count(item = word, 
                 feature = 장,   # 장으로 수정
                 sort = TRUE)
bible_pairs






# 9.2 단어 상관관계분석(같은 문서)
# 동시출현단어는 가장 많은 빈도수를 의미하는 반면,
# 상관관계(phi)는 두개 단어가 동시에 나오거나, 나오지 않을 확률
# ppt 설명

bible_cors <- bible_tb %>%
  pairwise_cor(item = word, 
               feature = 장, # 장으로 수정
               sort = TRUE)
bible_cors

###### correlation = 1 제거
bible_cors <- bible_cors %>%
  filter(correlation < 0.9999 & correlation > 0.3)

bible_cors







# 9.3 학습과 가장 많이 나타나는 단어와 상관관계가 높은 단어
bible_pairs %>%
  filter(item1 == "예수")

bible_cors %>%
  filter(item1 == "예수")







# 9.4 centrality 확인 (***추가***)
library(tidygraph)
library(igraph)
library(ggraph)

set.seed(123)

bible_pairs_graph <- bible_pairs %>%
  filter(n >= 15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(cent_dgr = centrality_degree(),
         cent_btw = centrality_betweenness(),
         cent_cls = centrality_closeness(),
         cent_egn = centrality_eigen(),
         cent_wgt = centrality_pagerank(weights=n),
         group = as.factor(group_infomap()))

bible_pairs_graph

# table로 저장시
bible_pairs_graph_tb <- as_tibble(bible_pairs_graph)
bible_pairs_graph_tb

# 평균거리
bible_pairs %>%
  as_tbl_graph(directed = F) %>%
  with_graph(graph_mean_dist())

# 기본 그래프
bible_pairs %>%
  filter(n > 15) %>% 
  as_tbl_graph(directed = F) %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# frequency(n) 그래프1
arrow <- grid::arrow(type = "closed", 
                     length = unit(.15, "inches"))

bible_pairs_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), 
                 edge_color = "darkred",
                 show.legend = FALSE) +
  geom_node_point(aes(size = cent_wgt,   # 노드 크기
                      color = group)) +
  geom_node_text(aes(label = name), 
                 repel = TRUE,          # repel =TRUE: 텍스트가 겹치지 않게
                 point.padding = unit(0.2, "lines")) +    
  theme_void() # 격자 없애기


# correlation

bible_cors_graph <- bible_cors %>%
  filter(correlation > 0.80) %>%
  as_tbl_graph(directed = F) %>%
  mutate(cent_dgr = centrality_degree(),
         cent_btw = centrality_betweenness(),
         cent_egn = centrality_eigen(),
         group = as.factor(group_infomap()))

bible_cors_graph

bible_cors_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation,
                     edge_width = correlation), 
                 edge_color = "darkred",
                 show.legend = FALSE) +
  geom_node_point(aes(size = cent_dgr,   # 노드 크기
                      color = group)) +
  geom_node_text(aes(label = name), 
                 repel = TRUE,
                 point.padding = unit(0.2, "lines")) +    # repel =TRUE: 텍스트가 겹치지 않게 
  theme_void() # 격자 없애기

















