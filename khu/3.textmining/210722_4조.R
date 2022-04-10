# 연습문제 관심있는 논문주제 검색후 키워드만 가져와서 분석, (2개 이상인 단어만)
library(KoNLP)
useSejongDic()

socialD <- read.csv("socialdistancing.csv", header =T)

socialD.df <- data.frame(doc_id=socialD$NO, text=socialD$키워드)
socialD.ds <- DataframeSource(socialD.df)
socialD.cp <- Corpus(socialD.ds)
inspect(socialD.cp)
class(socialD.cp)


# 특별한 의미의 숫자가 없으면 숫자표현 모두 삭제
library(tm)

mycorpus <- tm_map(socialD.cp, removeNumbers) # 숫자표현 제거
mycorpus <- tm_map(mycorpus, stripWhitespace) #공란처리 
mycorpus <- tm_map(mycorpus, removePunctuation) #특수문자 처리
mycorpus[[1]]$content


# 특수문제 제거, 새로 함수를 설정하여 처리
mytempfunct <- function(myobject, oldexp, newexp){
  newobect <- tm_map(myobject,
                     content_transformer(function(x, pattern) gsub(pattern, newexp, x)), oldexp)
  newobect
}
mycorpus1 <- mytempfunct(mycorpus, "COVID", "코로나19")
mycorpus1 <- mytempfunct(mycorpus, c("코로나바이러스감염증"), "코로나19")
mycorpus1 <- mytempfunct(mycorpus, c("사회적 거리두기"), "코로나19")
mycorpus1 <- mytempfunct(mycorpus, "[[:lower:]]", "")
mycorpus1 <- mytempfunct(mycorpus1, "[[:upper:]]", "")
mycorpus1 <- mytempfunct(mycorpus1, "\\,", "")
mycorpus1 <- mytempfunct(mycorpus1, "\\.", "")
mycorpus1 <- mytempfunct(mycorpus1, "\\)", "")

mycorpus1 <- mytempfunct(mycorpus1, "[^[:alpha:][:digit:]]"," ")
mycorpus1 <- mytempfunct(mycorpus1, "[一-???]","")  #한자제거
mycorpus1 <- mytempfunct(mycorpus1, "[[:digit:]]","")  #숫자제거
mycorpus1[[1]]$content


## 명사 추출 
#extractNoun()함수를 적용하면 tm_map함수의 형식 적용이 안되므로
#extractNoun함수를 적용한 결과가 단일 오브젝트 문서 형태를 갖도록 변환하는 함수 설정 

myNoun <- function(mytextNoun){
  myNounList <- paste(extractNoun(mytextNoun), collapse = ' ') 
  myNounList[[i]]$content <- sapply(myNounList[[i]]$content, function(x) {nchar(x) >= 2})
}  #collapse옵션 이용 명사들을 공란으로 구분해둔 문서로 전환하는 mynoun함수 새로 설정 
# 2글자 이상 명사만 추출

myNounCorpus <- mycorpus1
for (i in 1:length(mycorpus1)) {
  myNounCorpus[[i]]$content <- myNoun(mycorpus1[[i]]$content)
}

# 말뭉치를 구성하는 각각의 문서텍스트에서 명사 추출후, myNounCorpus 오브젝트로 저장

library(SnowballC)
myNounCorpus <- tm_map(myNounCorpus, stemDocument)

library(stringr)
# 전체 말뭉치 단어 확인
table(unlist(lapply(myNounCorpus, function(x) str_extract_all(x, boundary("word")))))


#DTM 구축
dtm.k <- DocumentTermMatrix(myNounCorpus)
dtm.k

inspect(dtm.k[1:11, 1:5])

colnames(dtm.k[,])

dtm.mx <- as.matrix(dtm.k)
dtm.df <- as.data.frame(dtm.mx)
class(dtm.df)

dtm.label.df <- cbind(dtm.df, LABEL=c("news"))

write.csv(dtm.label.df, file="myNoun.csv", row.names = F)

#tf-idf로 빈도 계산
dtm.k2 <- DocumentTermMatrix(myNounCorpus, control = list(weighting = weightTfIdf))

insepect(dtm.k2[1:10, 1:5])


# 자주 출현하는 단어 찾기
findFreqTerms(dtm.k2, highfreq = 10)
findAssocs(dtm.k2, "요인", 0.5)


# dtm 행렬 형태 변환 후 단어빈도 목록와 총단어의 길이 계산
word.freq <- apply(dtm.k[,], 2, sum)
length(word.freq): sum(word.freq)

sort.word.freq <- sort(word.freq, decreasing = T)
head(sort.word.freq, 20)

# 빈도 누적
cumsum.word.freq <- cumsum(sort.word.freq)
head(cumsum.word.freq, 20)

# 최대값이 1이되는 누적 비율 계산
prof.word.freq <- cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
head(prof.word.freq, 20)

# 단어빈도에 따른 간단한 분석
library(ggplot2)

myfig <- data.frame(1:length(word.freq), prof.word.freq) # 선그래프용 데이터 만들기
colnames(myfig) <- c('loc', 'prop')

myfig2 <- myfig[round(83.9*(0:10)),]
myfig2$lblloc <- paste(10*(1:19), "%th\nposition", sep = "") #점그래프용 데이터 생성

ggplot() +
  geom_line(data=myfig, aes(x=loc, y=prop), color='red') +
  geom_point(data=myfig2, aes(x=loc, y=prop), size=2, color='blue')+ 
  scale_x_continuous(breaks=myfig2$loc, labels = myfig2$lblloc) + 
  scale_y_continuous(breaks = 0.20*(0:5), labels = paste(20*(0:5), "%", sep=""))+ 
  labs(x= '\nOrder of word frequency', y= 'Cumulative proprtion\n') + 
  theme_classic()

# 워드 클라우드 그리기
install.packages("wordcloud2")
library(wordcloud2)

Wc_data <- data.frame(names(word.freq), word.freq)  
names(Wc_data) <- c('word', 'freq')  #단어를 첫번째 변수로, 빈도를 두번째 변수로


wordcloud2(Wc_data[Wc_data$word != "코로나"], size = 1, # 주요 키워드에 해당하는 코로나는 제외
           fontFamily = "Times New Roman", #원하는 글자 폰트 지정
           shuffle=F, #true로 지정하면 그릴 때마다 단어 배치가 바뀜
           rotateRatio = 0.3)   #0이면 회전 없음, 1이면 완전 회전


