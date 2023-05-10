# 시계열관련 라이브러리
library(TTR)
library(forecast)

# 1987년 1월부터 1993년 12월까지 리조트 기념품매장 매출액
data <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
data

fancy <- ts(data, frequency = 12, start = c(1987, 1))
fancy

win.graph(); plot.ts(fancy)
# 분산이 증가하는 경향이 있음 => log 변환으로 분산 조정

fancy_log <- log(fancy)
win.graph(); plot.ts(fancy_log)

# 1차 차분
fancy_diff <- diff(fancy_log, differences = 1)
win.graph(); plot.ts(fancy_diff)
# 평균은 어느정도 일정하지만 특정 시기에 분산이 크다

win.graph(); acf(fancy_diff, lag.max = 100)

win.graph(); pacf(fancy_diff, lag.max = 100)
# 절단점이 명확하지 않음 => auto.arima 함수 사용

auto.arima(fancy)

fancy_arima <- arima(fancy, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
fancy_fcast <- forecast(fancy_arima)
win.graph(); plot(fancy_fcast)
