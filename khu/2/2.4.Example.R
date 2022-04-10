
# 0.패키지로딩 -----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels, rstatix, effectsize, factoextra, outliers,# modeling packages
               ggpubr) #  visualization

# 1. 자료확인 -----------------------------------------------------------------
read_csv("occupancy_all.csv") -> mydata

# # A tibble: 10,808 x 7
#    date                Temperature Humidity Light   CO2 HumidityRatio Occupancy
#    <dttm>                    <dbl>    <dbl> <dbl> <dbl>         <dbl>     <dbl>
#  1 2015-02-02 14:19:00        23.7     26.3  585.  749.       0.00476         1
#  2 2015-02-02 14:19:59        23.7     26.3  578.  760.       0.00477         1
#  3 2015-02-02 14:21:00        23.7     26.2  573.  770.       0.00477         1
#  4 2015-02-02 14:22:00        23.7     26.1  494.  775.       0.00474         1
#  5 2015-02-02 14:23:00        23.8     26.2  489.  779        0.00477         1
#  6 2015-02-02 14:23:59        23.8     26.3  569.  790        0.00478         1
#  7 2015-02-02 14:25:00        23.7     26.3  536.  798        0.00478         1
#  8 2015-02-02 14:25:59        23.8     26.3  509   797        0.00478         1
#  9 2015-02-02 14:26:59        23.8     26.4  476   803.       0.00479         1
# 10 2015-02-02 14:28:00        23.7     26.4  510   809        0.00480         1
# # … with 10,798 more rows

mydata %>% glimpse()
mydata %>% get_summary_stats()

# # A tibble: 6 x 13
#   variable          n     min      max  median      q1      q3     iqr    mad    mean      sd    se    ci
#   <chr>         <dbl>   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl> <dbl> <dbl>
# 1 CO2           10808 413.    2028.    464     441     761     320     44.5   634.    313.    3.01  5.90 
# 2 Humidity      10808  16.7     39.1    25.7    21.4    28.3     6.93   5.16   25.6     4.95  0.048 0.093
# 3 HumidityRatio 10808   0.003    0.006   0.004   0.003   0.004   0.001  0.001   0.004   0.001 0     0    
# 4 Light         10808   0     1697.      0       0     414.    414.     0     138.    212.    2.04  4.00 
# 5 Occupancy     10808   0        1       0       0       0       0      0       0.25    0.433 0.004 0.008
# 6 Temperature   10808  19       24.4    20.7    20      21.5     1.5    1.08   20.8     1.08  0.01  0.02 


# 2.이상치확인 및 제거 ----------------------------------------------------------------
mydata %>% 
  select(-date) -> mydata.2
boxplot(mydata.2)$stats
mydata.2 %<>% 
  filter(Light <= 413 &
           CO2 <= 1240.75)

# 3.상관분석수행 --------------------------------------------------------------
my.palette <- get_palette(c("red", "white", "blue"),
                          200)
mydata.2 %>% cor_test()
mydata.2 %>% cor_mat()
mydata.2 %>% 
  cor_mat() %>% 
  cor_reorder() %>% 
  pull_lower_triangle() %>% 
  cor_plot(method = "number",
           label = T,
           palette = my.palette)

# 4.K-평균군집분석 --------------------------------------------------------------
mydata.2 %>%
  mutate(Occupancy = ifelse(Occupancy == 0, 2, 1)) -> mydata2
mydata2 %>% 
  select(-Occupancy) %>% 
  effectsize::standardize() -> points
mydata2 %>% 
  select(Occupancy) -> a
points %>% 
  bind_cols(a) -> labelled_points

# 3.1. 군집분석
kclust <- kmeans(points, centers =2) # 3개로 묶어볼까?
fviz_cluster(kclust, data = points, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )
kclust <- kmeans(points, centers = 2) # 4개로 묶어볼까?
fviz_cluster(kclust, data = points, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )
kclusts <-
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~ kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )
clusters <- kclusts %>% unnest(cols = c(tidied))
assignments <- kclusts %>% unnest(cols = c(augmented))
clusterings <- kclusts %>% unnest(cols = c(glanced))
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point() # 최적의 k 는 4

kclust <- kmeans(points, centers = 4) # 4개로 묶어볼까? 근데 원래는 2임 
fviz_cluster(kclust, data = points, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )

# 정오분류표
kclust <- kmeans(points, centers = 2) # 4개로 묶어볼까? 근데 원래는 2임
fviz_cluster(kclust, data = points, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )
caret::confusionMatrix(as.factor(mydata2$Occupancy), as.factor(kclust$cluster))
# Confusion Matrix and Statistics
# |
#           Reference
# Prediction    1    2
#          1   45   82
#          2 3143 4758
#                                          
#                Accuracy : 0.5983         
#                  95% CI : (0.5875, 0.609)
#     No Information Rate : 0.6029         
#     P-Value [Acc > NIR] : 0.8039         
#                                          
#                   Kappa : -0.0034        
#                                          
#  Mcnemar's Test P-Value : <2e-16         
#                                          
#             Sensitivity : 0.014115       
#             Specificity : 0.983058       
#          Pos Pred Value : 0.354331       
#          Neg Pred Value : 0.602202       
#              Prevalence : 0.397110       
#          Detection Rate : 0.005605       
#    Detection Prevalence : 0.015820       
#       Balanced Accuracy : 0.498587       
#                                          
#        'Positive' Class : 1   
