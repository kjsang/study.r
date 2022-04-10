# 0. packages and data loading

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               modeldata) # datasets

# 1. data loading
data(crickets, package = "modeldata")
names(crickets)
# [1] "species" "temp"    "rate"

# 1.1. plot
crickets %>% 
  ggplot(aes(x = temp, y = rate,
             # elements will be colored differently for each species:
             col = species)) +
  # Plot points for each data point and color by species
  geom_point() +
  # Show a simple linear model fit created separately for each species:
  geom_smooth(method = lm, se = F) +
  labs(x = "Temperature (C)",
       y = "Chirp Rate (per minute")

# 1.2. interaction effect
interaction_fit <-  lm(rate ~ (temp + species)^2, data = crickets) 
interaction_fit
# Call:
# lm(formula = rate ~ (temp + species)^2, data = crickets)
# 
# Coefficients:
#           (Intercept)                   temp       speciesO. niveus  
#               -11.041                  3.751                 -4.348  
# temp:speciesO. niveus  
#                -0.234  

# 2.1. using diagnostic plots
par(mfrow = c(1, 2))
plot(interaction_fit, which = 1)
plot(interaction_fit, which = 2)

# 2.2. ANOVA 
main_effect_fit <-  lm(rate ~ temp + species, data = crickets) 
anova(main_effect_fit, interaction_fit)
# Analysis of Variance Table
# 
# Model 1: rate ~ temp + species
# Model 2: rate ~ (temp + species)^2
#   Res.Df    RSS Df Sum of Sq     F Pr(>F)
# 1     28 89.350                          
# 2     27 85.074  1    4.2758 1.357 0.2542

summary(main_effect_fit)
# Call:
# lm(formula = rate ~ temp + species, data = crickets)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.0128 -1.1296 -0.3912  0.9650  3.7800 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -7.21091    2.55094  -2.827  0.00858 ** 
# temp               3.60275    0.09729  37.032  < 2e-16 ***
# speciesO. niveus -10.06529    0.73526 -13.689 6.27e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.786 on 28 degrees of freedom
# Multiple R-squared:  0.9896,	Adjusted R-squared:  0.9888 
# F-statistic:  1331 on 2 and 28 DF,  p-value: < 2.2e-16

corr_res <- 
  purrr::map(mtcars %>% 
                  select(-mpg), 
             cor.test,
             y = mtcars$mpg)

# The first of ten results in the vector: 
corr_res[[1]]
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  .x[[i]] and mtcars$mpg
#> t = -8.9, df = 30, p-value = 6e-10
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.9258 -0.7163
#> sample estimates:
#>     cor 
#> -0.8522

broom::tidy(corr_res[[1]])
#> # A tibble: 1 x 8
#>   estimate statistic  p.value parameter conf.low conf.high method        alternative
#>      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>         <chr>      
#> 1   -0.852     -8.92 6.11e-10        30   -0.926    -0.716 Pearson's pr… two.sided

# These results can be “stacked” and added to a ggplot():
corr_res %>% 
  # Convert each to a tidy format; `map_dfr()` stacks the data frames 
  map_dfr(tidy, .id = "predictor") %>% 
  ggplot(aes(x = fct_reorder(predictor, estimate))) + 
  geom_point(aes(y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  labs(x = NULL, y = "Correlation with mpg")

### 3.4 COMBINING BASE R MODELS AND THE TIDYVERSE
split_by_species <-
  crickets %>% 
  dplyr::group_nest(species)
# # A tibble: 2 x 2
#   species                        data
#   <fct>            <list<tibble[,2]>>
# 1 O. exclamationis           [14 × 2]
# 2 O. niveus                  [17 × 2]