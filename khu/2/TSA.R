
# 패키지 적재 ----------------------------------------------

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tsibble, # modeling packages
               tseries, forecast)


# 데이터 살펴보기 --------------------------------------------

# 원데이터 살펴보기
AirPassengers %>% 
  as_tsibble() %>% 
  ggplot(aes(x = index, y = value)) +
  geom_line(aes(col = index))

# 계절성 제거한 데이터 추세 살펴보기
AirPassengers %>% 
  decompose() -> AirPassengers_decompose
AirPassengers_decompose_adj <- AirPassengers - AirPassengers_decompose$seasonal
plot(AirPassengers_decompose_adj)


# 시계열 분해 ----------------------------------------------

AirPassengers %>% 
  ts(frequency = 12) %>% 
  decompose() %>% 
  plot()

AirPassengers %>% 
  stl(s.window = "periodic") %>% 
  plot()

AirPassengers %>% 
  log() %>% # 크기 변화 상쇄용
  diff() %>% # 시간간격을 차분하여 주기에 따른 변화 상쇄
  adf.test(alternative = "stationary", k=0)

# 	Augmented Dickey-Fuller Test
# 
# data:  .
# Dickey-Fuller = -9.6003, Lag order = 0, p-value = 0.01
# alternative hypothesis: stationary

AirPassengers %>% 
  log() %>% # 크기 변화 상쇄용
  diff() %>% # 시간간갸ㅕㄱ 차분하여 주기에 따른 변화 상쇄
  auto.arima()
# Series: . 
# ARIMA(0,0,1)(0,1,1)[12] 
# 
# Coefficients:
#           ma1     sma1
#       -0.4018  -0.5569
# s.e.   0.0896   0.0731
# 
# sigma^2 estimated as 0.001369:  log likelihood=244.7
# AIC=-483.39   AICc=-483.2   BIC=-474.77

AirPassengers %>% 
  log() %>% # 크기 변화 상쇄용
  diff() %>% # 시간간갸ㅕㄱ 차분하여 주기에 따른 변화 상쇄
  auto.arima() %>% # 자동으로 아리마분석
  tsdiag() # 그림그려주기


# 예측 --------------------------------------------------

AirPassengers %>% 
  arima(order = c(2,1,1), list(order = c(0,1,0), period = 12)) %>% 
  predict(n.ahead = 24) -> forecast_Air
forecast_Air

AirPassengers %>% 
  ts.plot(forecast_Air$pred, # 
          forecast_Air$pred + 2*forecast_Air$se, 
          forecast_Air$pred - 2*forecast_Air$se,
          col = c(1,2,4,4),
          lty = c(1,1,2,2),
          xlab="year", ylab="counts")

?ts.plot

legend("topleft",
       c("Actual", "Forecast", "Error Bounds (95% Confidence)"),
       col = c(1,2,4), lty = c(1,1,2))


# Nile dataset ----------------------------------------

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               tsibble, # modeling packages
               forecast)

Nile %>% 
  auto.arima
Series: . 
# ARIMA(1,1,1) 
# 
# Coefficients:
#          ar1      ma1
#       0.2544  -0.8741
# s.e.  0.1194   0.0605
# 
# sigma^2 estimated as 20177:  log likelihood=-630.63
# AIC=1267.25   AICc=1267.51   BIC=1275.04

Nile %>% 
  auto.arima %>% 
  forecast() %>% 
  plot()
