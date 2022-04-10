# 8.워드 클라우드
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

# 8.1 단순 워드 클라우드
# install.packages("ggwordcloud")
library(wordcloud)
library(ggwordcloud)
library(reshape2)

# wordcloud 그래프(n)
study_group_tf_idf %>%
  acast(word ~ 구분, 
        value.var = "n", 
        fill = 0) %>%
  comparison.cloud(colors = c("black", "green", 
                              "red","blue","purple"),
                   max.words = 200,
                   min.freq=10,
                   scale=c(4,1), 
                   title.size=1.4)

# ggwordcloud 그래프 (n)
set.seed(123)
study_group_tf_idf %>%
  group_by(구분) %>%
  filter(n > 10) %>%
  ggplot(mapping = aes(label = word,
                       size = n,
                       color = 구분)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  facet_wrap(~ 구분)

# tf_idf
set.seed(123)
study_group_tf_idf %>%
  group_by(구분) %>%
  slice_max(tf_idf, n=50) %>%
  ggplot(mapping = aes(label = word,
                       size = tf_idf,
                       color = tf_idf)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15) +
  scale_color_gradient(low = "darkred", high = "red") +
  facet_wrap(~ 구분)


# log_odds
set.seed(123)
study_group_tf_idf %>%
  group_by(구분) %>%
  slice_max(log_odds, n=50) %>%
  ggplot(mapping = aes(label = word,
                       size = log_odds,
                       color = log_odds)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15) +
  scale_color_gradient(low = "darkred", high = "red") +
  facet_wrap(~ 구분)















