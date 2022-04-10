### 이전 데이터 날아감 ㅠ ###


library(tidyverse)
library(magrittr)

# 실업률 시계열 그래프
economics %>% 
  ggplot(aes(x = date, y = unemploy)) +
  geom_line()

# 혼자서 해보기1: 개인저축률 시계열 그래프
economics %>% 
  ggplot(aes(x = date, y = psavert)) +
  geom_line()

# 박스플롯
mpg %>% 
  ggplot(aes(x = drv, y = hwy)) +
  geom_boxplot()

# 혼자서 해보기1: 차종별 도시연비
mpg %>% 
  select(class, cty) %>% # 안 해도 되지만 하면 좋음! 데이터 범위 축소
  filter(class %in% c("compact", "subcompact", "suv")) %>% # 차종 세 가지를 선택
  ggplot(aes(class, cty)) + # 박스플랏 그리기 위해 x와 y 를 할당
  geom_boxplot()
