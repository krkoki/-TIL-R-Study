#시계열관련 라이브러리
library(TTR)
library(forecast)

# 1946년 1월부터 1959년 12월까지의 뉴욕의 월별 출생자수
data <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
data

# start 1946년 1월부터 시작
birth <- ts(data, frequency = 12, start = c(1946, 1))
birth

win.graph(); plot.ts(birth, main = "뉴욕 월별 출생자 수")

# 데이터 분해 - trend, seasonal, random 데이터 추세 확인
# trend - 증가추세, seasonal 계절성 요인이 있음
birth_comp <- decompose(birth)
win.graph(); plot(birth_comp)

birth_comp$trend
birth_comp$seasonal

# 시계열 데이터에서 계절성 요인을 제거한 데이터
birth_adjusted <- birth - birth_comp$seasonal
win.graph(); plot.ts(birth_adjusted, main = "birth - seasonal factor")

# 차분을 통해 정상성 확인
birth_diff1 <- diff(birth_adjusted, differences = 1)
win.graph(); plot.ts(birth_diff1, main = "1차 차분")
# 1차 차분 후에도 분산의 변동성이 크다

win.graph(); acf(birth_diff1, lag.max = 20)
win.graph(); pacf(birth_diff1, lag.max = 20)
# 절단값이 명확하지 않아 ARIMA 모형 확정이 어려운 상태

# auto.arima 함수 사용
auto.arima(birth) # ARIMA(2,1,2)(1,1,1)[12]

birth_arima <- arima(birth, order = c(2,1,2), seasonal =
list(order = c(1,1,1), period = 12))
birth_arima

birth_fcast <- forecast(birth_arima)
birth_fcast
win.graph(); plot(birth_fcast, main = "Forecasts 1960 & 1961")