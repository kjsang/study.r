# 8.워드 클라우드

# 8.1 단순 워드 클라우드 (n)
# install.packages("wordcloud")
library(wordcloud)

text_count %>%
  with(wordcloud(words=word,
                 random.order=FALSE, 
                 freq=n, 
                 min.freq=10, 
                 max.words=50))


# 8.2 색깔 워드 클라우드 (tf_idf)

palete <- brewer.pal(6,"Dark2")

windowsFonts(malgun=windowsFont("맑은 고딕"))

text_count %>%
  with(wordcloud(words=word,
                 freq=tf_idf,
                 max.words=100,
                 random.order=FALSE, 
                 colors=palete, 
                 family="malgun"))
