  # 패키지
library(dplyr)
library(readxl)
library(rJava)
library(memoise)
library(multilinguer)
library(KoNLP)
library(stringr)
useNIADic()
library(tm)

  # 데이터 불러오기
keyword = read_excel("논문검색리스트Excel.xls", col_names = T)
View(keyword)

keyword <- rename(keyword, "key" = "키워드(한국어)")
keyword <- keyword$key

  # 데이터 프레임 변환 
keyword.df <- as.data.frame(keyword) 
View(keyword.df)

#corpus 생성
keyword1 <- Corpus(VectorSource(keyword))

keyword2 <- keyword1$content

#전처리
keyword2 <- str_replace_all(keyword2, "[[:digit:]]","")  #숫자제거  
keyword2 <- str_replace_all(keyword2, "[[:lower:]]","")  #소문자 알파벳 제거 
keyword2 <- str_replace_all(keyword2, "[[:upper:]]","") #대문자 알파벳 제거
keyword2 <- str_replace_all(keyword2, "[^[:alpha:][:digit:]]"," ") 
#특수문자제거  
keyword2 <- str_replace_all(keyword2, "[一-龥]","")  #한자제거
keyword2 <-  Filter(function(x) {nchar(x) >= 2}, keyword2)
head(keyword2, 20)

#명사 추출
Noun <- lapply(keyword2, extractNoun)
Noun

nounVec <- unlist(Noun)
nounFreq <- table(nounVec)
names(head(sort(nounFreq, decreasing = T), 20))

word <- names(head(sort(nounFreq, decreasing = T), 20))
Freq <- as.vector(head(sort(nounFreq, decreasing = T), 20))

sum <- sum(nounFreq)
percent <- round(Freq/sum*100, digits = 2)

#barplot 제목 생성
mainTxt <- "고빈도 단어"

#barplot 생성하기
bp <- barplot(percent, main=mainTxt, las = 2, Ylim=c(0,5), ylab = "%",
              names.arg = word, col = "black")
text(x=bp, y=percent+0.1, labels=paste(Freq), col="black", cex=0.8)
par(family='AppleGothic')


## 워드클라우드
install.packages("wordcloud2")
library(wordcloud2)

Wc_data <- data.frame(word, freq)  
names(Wc_data) <- c('word', 'freq')  

wordcloud2(Wc_data, size = 2,
           fontFamily = "Times New Roman", 
           shuffle=F, 
           rotateRatio = 0.3)   
