# 9.Co-occurence
library(widyr)

# 9.1 동시출현단어 분석 (같은 문서)
study_pairs <- study_long %>%
  pairwise_count(item = word, 
                 feature = no, 
                 sort = TRUE)
study_pairs






# 9.2 단어 상관관계분석(같은 문서)
# 동시출현단어는 가장 많은 빈도수를 의미하는 반면,
# 상관관계(phi)는 두개 단어가 동시에 나오거나, 나오지 않을 확률
# ppt 설명

study_cors <- study_long %>%
  pairwise_cor(item = word, 
               feature = no, 
               sort = TRUE)
study_cors

###### correlation = 1 제거
study_cors <- study_cors %>%
  filter(correlation < 0.9999 & correlation > 0.3)

study_cors


study_cors %>%
  filter(correlation < 0.3 )




# 9.3 학습과 가장 많이 나타나는 단어와 상관관계가 높은 단어
study_pairs %>%
  filter(item1 == "학습")

study_cors %>%
  filter(item1 == "소규모")






# 9.4 centrality 확인 (***추가***)
library(tidygraph)
library(igraph)
library(ggraph)


# ppt 설명

set.seed(123)

study_pairs_graph <- study_pairs %>%
  filter(n >= 15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(cent_dgr = centrality_degree(),
         cent_btw = centrality_betweenness(),
         cent_cls = centrality_closeness(),
         cent_egn = centrality_eigen(),
         cent_wgt = centrality_pagerank(weights=n),
         group = as.factor(group_infomap()))

study_pairs_graph




# table로 저장시
study_pairs_graph_tb <- as_tibble(study_pairs_graph)
study_pairs_graph_tb




# 평균거리
study_pairs %>%
  as_tbl_graph(directed = F) %>%
  with_graph(graph_mean_dist())






# 기본 그래프
study_pairs %>%
  filter(n > 15) %>% 
  as_tbl_graph(directed = F) %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)






# frequency(n) 그래프1
arrow <- grid::arrow(type = "closed", 
                     length = unit(.15, "inches"))

study_pairs_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n,
                     edge_width = n), 
                 edge_color = "darkred",
                 show.legend = FALSE) +
  geom_node_point(aes(size = cent_wgt,   # 노드 크기
                      color = group)) +
  geom_node_text(aes(label = name), 
                 repel = TRUE,          # repel =TRUE: 텍스트가 겹치지 않게
                 point.padding = unit(0.2, "lines")) +    
  theme_void() # 격자 없애기


# correlation

study_cors_graph <- study_cors %>%
  filter(correlation > 0.8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(cent_dgr = centrality_degree(),
         cent_btw = centrality_betweenness(),
         cent_egn = centrality_eigen(),
         group = as.factor(group_infomap()))

study_cors_graph

study_cors_graph %>%
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

















