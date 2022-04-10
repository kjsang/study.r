library(tidyverse)
# 오징어 게임


# 가정 --------------------------------------------------

# 1. 참가자가 강화유리를 구분할 확률은 50%
# 2. 각 단계별 선택은 독립
# 3. 뒤에 따라오는 사람이 앞사람 선택을 모두 기억을 한다.

# 18단계를 살아남을 확률을 찾기보다는 한 사람이 어떻게 선택하는지, 한 번의 선택에서 나타나는 확률을 찾아보면 좋다.


# 1.  점프 ----------------------------------------------

# 점프를 구현하는 방법은 아래와 같다.
jump <- function() {
  ifelse(
    runif(1) < 0.5,
    "survival", "death")
}
jump()


# 2.성공 패턴 구현 ----------------------------------------

# 한 사람의 성공과 실패 패턴을 구현하자.
recursive_jumping <- function(x) {
  if (jump() == "death") {
    return ("death")
  } else {
    return (c("survive",
              recursive_jumping(x)))
  }
}
recursive_jumping(1)
recursive_jumping(2)
recursive_jumping(3)
recursive_jumping(4)

# 사람이 특정 단계에서 죽었다면, 해당 단게까지는 모든 사람이 해당 단계까지 해답을 알게 된다.

# count
person_survival <- function(n, 
                            verbose = F) {
  count_pass = 0
  for(i in 1:n) {
    confirms = length(recursive_jumping(i))
    count_pass = count_pass + confirms
  }
  if(verbose == T) {
    print(paste(count_pass, "confirmed"))
  }
  ifelse(count_pass > 18, 1, 0)
}
# 9번째 사람이 살아남았나?
person_survival(9, verbose = T)

#  100000번 시도했을 때 9번째 사람이 살아남을 확률
mean(replicate(100000, person_survival(9))) # 0.40862 정도 나온다.
#  100000번 시도했을 때 5번째 사람이 살아남을 확률
mean(replicate(100000, person_survival(5))) # 0.01627 정도 나오네...

prob <- rep(0, 16)
for (i in 1:16) {
  prob[i] <- mean(replicate(100000, person_survival(i)))
  print(paste("survival brob. of ", i, "-th person:",
              prob[i]*100, "%"))
}

prob <- round(prob, digits = 3)
prob
barplot(prob,
        main = "오징어 게임 참가자별 생존 확률")

pbinom(0:15, 18, 0.5) %>% round(digits = 3)
p <- roun

x <- 0:18
y <- dgeom(0:18, 0.5)
plot(x, y)

