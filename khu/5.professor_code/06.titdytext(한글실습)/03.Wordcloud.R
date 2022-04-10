# 8.워드 클라우드
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

# 8.1 단순 워드 클라우드
# install.packages("ggwordcloud")
library(ggwordcloud)

# n
set.seed(123)
bible_count %>%
  filter(n > 10) %>%
  ggplot(mapping = aes(label = word,
                       size = n)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal()

# tf_idf
set.seed(123)
bible_count %>%
  slice_max(tf_idf, n=50) %>%
  ggplot(mapping = aes(label = word,
                       size = tf_idf,
                       color = tf_idf)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20)

# log_odds
set.seed(123)
bible_count %>%
  slice_max(log_odds, n=50) %>%
  ggplot(mapping = aes(label = word,
                       size = log_odds,
                       color = log_odds)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15)






