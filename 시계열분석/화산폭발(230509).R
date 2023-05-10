# 시계열관련 라이브러리
library(TTR)
library(forecast)

# 1500년부터 1969년까지 화산폭발 먼지량
data <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip = 1)
dust <- ts(data, start = c(1500))
dust

win.graph(); plot.ts(dust)
# 평균과 분산이 어느정도 일정함 => 차분하지 않음

win.graph(); acf(dust, lag.max = 20)
# lag = 4 => MA(3)

win.graph(); pacf(dust, lag.max = 20)
# lag 3부터 신뢰구간 안에 존재함. lag 절단값은 3 => AR(2)

auto.arima(dust)
# ARIMA(1,0,2) p,d,q

# d = 0 인 경우 AR(2) / MA(3) / ARIMA(1,0,2) 중 선택해서 적용 가능
# 파라미터가 가장 적은 모형을 선택하는 것을 추천함 => AR(2)적용 => c(2,0,0)
# MA(3)을 선택한다면 => c(0,0,3)

dust_arima <- arima(dust, order = c(2, 0, 0))
dust_fcast <- forecast(dust_arima, h = 30)
win.graph(); plot(dust_fcast)


dust_arima <- arima(dust, order = c(0, 0, 3))
dust_fcast <- forecast(dust_arima, h = 30)
win.graph(); plot(dust_fcast)

dust_arima <- arima(dust, order = c(1, 0, 2))
dust_fcast <- forecast(dust_arima, h = 30)
win.graph(); plot(dust_fcast)