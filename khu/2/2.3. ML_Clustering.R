

# 상관분석 --------------------------------------------------------------------


# 1. Libraries and Data Loading
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, rstatix, # modeling packages
               ggpubr) #  visualization


iris %>% 
  select(Sepal.Length, Sepal.Width) %>% 
  cor_test()

read_csv("autoparts.csv") -> raw_autoparts
# 2. 데이터 살펴보기
raw_autoparts %>% skimr::skim()
raw_autoparts %>% glimpse()

# 3. 데이터 전처리
raw_autoparts %>% 
  filter(prod_no=="90784-76001") %>% 
  select(-prod_no) %>% 
  filter(c_thickness < 1000) %>% 
  filter(highpressure_time != 65534) -> autoparts

# 4. 상관관계분석
autoparts %>% 
  select(separation, s_separation) %>% 
  cor_test()

# 5. 시각화
my.palette <- get_palette(c("red", "white", "blue"),
                          200)
my.palette <- get_palette("PuOr", 200)
autoparts %>% 
  cor_mat() %>% 
  cor_reorder() %>% 
  pull_lower_triangle() %>% 
  cor_plot(method = "number",
           label = T,
           palette = my.palette)


# 군집분석 --------------------------------------------------------------------
# 1. 패키지 로딩
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, rstatix, # modeling packages
               ggpubr) #  visualization

# 2. 데이터 전처리
iris %>% 
  select(Petal.Length, Petal.Width, Species) %>% # 변수 선택
  rename(x1 = Petal.Length,
         x2 = Petal.Width,
         cluster = Species) %>% # 간편하게 이름 바꿔주기 
  mutate_at(c("x1", "x2"), ~(scale(.) %>% as.vector)) %>% # 데이터 표준화
  as_tibble() -> labelled_points # 티블로 바꾼 후 데이터 적재

# 2.1. 데이터 살펴보기 (시각화)
ggplot(labelled_points, aes(x1, x2, color = cluster)) + # 지지플랏 그려보자~
  geom_point(alpha = 0.3)

# 3.1. 군집분석
points <- 
  labelled_points %>% 
  select(-cluster) # 종을 빼주자 (목표변수니까 빼줘야함)

kclust <- kmeans(points, centers = 3) # 3개로 묶어볼까?
kclust
summary(kclust)
augment(kclust, points)
tidy(kclust)
glance(kclust)

kclusts <-
  tibble(k = 3) %>%
  mutate(
    kclust = map(k, ~ kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )
clusters <-
  kclusts %>%
  unnest(cols = c(tidied))

assignments <-
  kclusts %>%
  unnest(cols = c(augmented))

clusterings <-
  kclusts %>%
  unnest(cols = c(glanced))

p1 <-
  ggplot(assignments, aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap( ~ k)
p1

p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2


# 4. 팔꿈치 그리기~
kclusts <-
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~ kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )
clusters <-
  kclusts %>%
  unnest(cols = c(tidied))

assignments <-
  kclusts %>%
  unnest(cols = c(augmented))

clusterings <-
  kclusts %>%
  unnest(cols = c(glanced))

p1 <-
  ggplot(assignments, aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap( ~ k)
p1

p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

# 자동차 변수로 만들기 -------------------------------------------------------------

# 1. 패키지 로딩
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, rstatix, effectsize, factoextra,# modeling packages
               ggpubr) #  visualization

# 2. 데이터 전처리
read_csv("autoparts.csv") %>% 
  filter(prod_no=="90784-76001") %>% 
  select(-prod_no) %>% 
  filter(c_thickness < 1000) %>% 
  filter(highpressure_time != 65534) %>% 
  mutate(flag = ifelse(c_thickness > 32, 2,
                       ifelse(c_thickness < 20, 3, 1))) -> autopart

autopart %>% 
  select(-flag) %>% 
  effectsize::standardize() -> autoparts
autopart %>% 
  select(flag) -> a

autoparts %>% 
  bind_cols(a) -> labelled_points

# 3.1. 군집분석
points <- 
  labelled_points %>% 
  select(-flag) # 종을 빼주자 (목표변수니까 빼줘야함)

kclust <- kmeans(points, centers = 3) # 3개로 묶어볼까?
fviz_cluster(kclust, data = points, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )
kclust <- kmeans(points, centers = 4) # 4개로 묶어볼까?
fviz_cluster(kclust, data = points, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )

# 4. 팔꿈치 그리기~
kclusts <-
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~ kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )
clusters <-
  kclusts %>%
  unnest(cols = c(tidied))

assignments <-
  kclusts %>%
  unnest(cols = c(augmented))

clusterings <-
  kclusts %>%
  unnest(cols = c(glanced))

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

# 5. 정분류율 보기
kclust <- kmeans(points, centers = 3)
table(real = labelled_points$flag, pred = kclust$cluster)


# 유클리디안 거리 ----------------------------------------------------------------
tibble(x1 = c(8,12,15), x2 = c(10,13,18)) -> x
dist(x) %>% 
  hclust() %>% 
  plot()

tibble (x1 = c(1,3,7,14,2,28,27,25,10,11)) %>% 
  dist() %>% 
  hclust() %>% 
  plot()

read_csv("autoparts.csv") %>% 
  filter(prod_no=="45231-P3B750") %>% 
  select(-prod_no) %>% 
  filter(c_thickness < 1000) %>% 
  mutate(flag = ifelse(c_thickness > 32, 2,
                       ifelse(c_thickness < 20, 3, 1))) -> autopart
autopart %>% 
  mutate(flag = as.factor(flag)) %>% 
  group_by(flag) %>% 
  summarize(n = n())
# # A tibble: 3 x 2
#   flag      n
#   <fct> <int>
# 1 1        50
# 2 2        17
# 3 3         2

autopart %>% 
  select(-flag) %>% 
  effectsize::standardize() -> autoparts # 정규화
autopart %>% 
  select(flag) -> a
autoparts %>% 
  bind_cols(a) -> labelled_points
autoparts %>% 
  dist() %>% # 거리구하는 함수
  hclust() %>% # 거리비교
  plot() # 그림으로 봅시다 
autoparts %>% 
  dist() %>% # 거리구하는 함수
  hclust() %>%
  rect.hclust(k=3) # 네모를 씌워주자


# 다차원척도법 ------------------------------------------------------------------
# 1. 패키지 로딩
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, rstatix, effectsize, factoextra,# modeling packages
               ggpubr) #  visualization

# 2. 데이터 전처리
read_csv("autoparts.csv") %>% 
  filter(prod_no=="45231-P3B750") %>% 
  select(-prod_no) %>% 
  filter(c_thickness < 1000) %>% 
  mutate(flag = ifelse(c_thickness > 32, 2,
                       ifelse(c_thickness < 20, 3, 1))) -> autopart


autopart %>% 
  select(-flag & -c_thickness) %>% 
  dist() %>% 
  cmdscale() %>% 
  as_tibble() %>% 
  rename(Dim.1 = V1,
         Dim.2 = V2) %>% 
  ggscatter(x = "Dim.1", y = "Dim.2",
            size = 1,
            palette = "jco")

