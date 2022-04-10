
# 1. Usage ----------------------------------------------------------------

library(tibble)
as_tibble(iris)
# # A tibble: 150 x 5
#    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#  1          5.1         3.5          1.4         0.2 setosa 
#  2          4.9         3            1.4         0.2 setosa 
#  3          4.7         3.2          1.3         0.2 setosa 
#  4          4.6         3.1          1.5         0.2 setosa 
#  5          5           3.6          1.4         0.2 setosa 
#  6          5.4         3.9          1.7         0.4 setosa 
#  7          4.6         3.4          1.4         0.3 setosa 
#  8          5           3.4          1.5         0.2 setosa 
#  9          4.4         2.9          1.4         0.2 setosa 
# 10          4.9         3.1          1.5         0.1 setosa 
# # … with 140 more rows

tibble(x = 1:5,
       y = 1,
       z = x ^ 2 + y)
# # A tibble: 5 x 3
#       x     y     z
#   <int> <dbl> <dbl>
# 1     1     1     2
# 2     2     1     5
# 3     3     1    10
# 4     4     1    17
# 5     5     1    26

tribble(
  ~x, ~y,  ~z,
  "a", 2,  3.6,
  "b", 1,  8.5
)
# # A tibble: 2 x 3
#   x         y     z
#   <chr> <dbl> <dbl>
# 1 a         2   3.6
# 2 b         1   8.5

tibble(x = 1:3,
       y = list(1:5, 1:10, 1:20)) -> tibbledata
# # A tibble: 3 x 2
#       x y         
#   <int> <list>    
# 1     1 <int [5]> 
# 2     2 <int [10]>
# 3     3 <int [20]>
# List-columns are often created by tidyr::nest(), but they can be useful to create by hand.
tibbledata %>% 
  tidyr::unnest(y)
# # A tibble: 35 x 2
#        x     y
#    <int> <int>
#  1     1     1
#  2     1     2
#  3     1     3
#  4     1     4
#  5     1     5
#  6     2     1
#  7     2     2
#  8     2     3
#  9     2     4
# 10     2     5
# # … with 25 more rows
