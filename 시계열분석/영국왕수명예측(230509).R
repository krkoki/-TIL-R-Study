# 시계열관련 라이브러리
# install.packages("TTR")
library(TTR)
# install.packages("forecast")
library(forecast)

# 안정적인 시계열 : 시간의 추이와 관계없이 평균, 분산이 일정한 데이터
# 안정적인 시계일이 아닌 데이터 : 로그를 이용하거나 차분을 통해 안정적인 시계열로 변환한 후 분석 진행

########################################
# 영국왕들의 수명 데이터
kings <- read.csv("d:/workspace_kitae/R2/data/time/kings.dat", header=F)
kings

########################################
# 숫자형 자료를 시계열자료로 변환, time series
# 시각화하여 안정적인 시계열인지 확인하는 과정

kings_ts <- ts(kings)
win.graph(); plot.ts(kings_ts)

########################################
# 안정적인 시계열이 아닌 경우 변환(이동평균, 차분)
# arima 모형 생성 - ACF,PACF 차트 또는 auto.arima 함수를 사용 하여 최적화된 파라미터 도출

########################################
# 이동평균
# 1050 + 1000의 평균, 1050 + 1010의 평균, 1010 + 1020의 평균, 1020 + 1040의 평균
# 어느 시점의 충격이 이틀간 영향을 미친다면 2차이동평균 모형 MA(2) 모델을 추정하게 됨
# MA(1) 1차 이동평균모형, MA(2) 2차 이동평균모형
a <- c(1000,1050,1010,1020,1040)
b <- SMA(a, n=2) # n 갯수
b
win.graph(); plot.ts(b)

########################################
# 시계열 데이터를 이동평균한 값을 생성
# 차분(difference) : 현재 시점 자료에서 이전 시점의 자료를 뺀값
# 이동평균(그래프를 부드럽게 표현하기 위한 방법)
kings_sma3 <- SMA(kings_ts, n = 3) # 3차 이동평균(3년간의 평균)
kings_sma8 <- SMA(kings_ts, n = 8)
kings_sma12 <- SMA(kings_ts, n = 12)

win.graph()
# graphical PARameters
par(mfrow = c(2, 2)) # 2행 2열 형식(그래프를 합치는 옵션)
plot.ts(kings_ts)
plot.ts(kings_sma3)
plot.ts(kings_sma8)
plot.ts(kings_sma12)

########################################
# 1025 1030 1015 1030
# 1030-1025=5
# 1015-1030=-15
# diff(데이터,differences=n) n 몇번 차분
# 정상성을 만족시키기 위해 차분을 사용함
# 1차 차분
c <- diff(b, differences = 1)
c
win.graph(); plot.ts(c)

########################################
# 정상성(stationary) : 모든 시점에 평균이 일정한 특성
# 시간에 따라 확률적인 성분이 변하지 않는다는 가정, 정상성이 없으면 차분, 이동평균 등을 사용하여 변환
# 차분(difference) : 현시점 자료에서 전시점 자료를 빼는 것
# 평균이 일정하지 않은 시계열은 차분(difference)을 통해 정상화
# 1차 차분을 통해 데이터를 정상화하는 과정
kings_diff1 <- diff(kings_ts, differences = 1)
kings_diff2 <- diff(kings_ts, differences = 2)
kings_diff3 <- diff(kings_ts, differences = 3)

win.graph()
par(mfrow = c(2,2)) #2행 2열 형식
plot.ts(kings_ts)
plot.ts(kings_diff1)
plot.ts(kings_diff2)
plot.ts(kings_diff3)
# 1차 차분으로도 어느 정도 정상화 패턴을 보이고 있음

########################################
# 1차 차분한 데이터로 ARIMA 모형 확인
# acf와 pacf를 통해 적합한 arima 모형 결정
# acf(자기상관함수), pacf(부분자기상관함수) , lag 시차
win.graph(); acf(kings_diff1, lag.max = 20)
# lag 0 시점은 읽지 않음
# lag 2부터 점선 안에 존재함(모두 신뢰구간 안에 있음) lag 절단값은 2 => MA(1)

########################################
win.graph(); pacf(kings_diff1, lag.max = 20)
# lag 4부터 점선 안에 존재함. lag 절단값은 4 => AR(3) 앞의 3개 데이터가 현재 데이터에 영향을 줌

########################################
library(tseries)
# 로그 후 차분한 자료를 adf.test 함수로 안정적인 시계열인지 확인하는 과정
# k 테스트 통계를 계산하기위한 지연 순서
adf.test(diff(log(kings_ts)), alternative="stationary", k=0)
# p-value가 0.05보다 작으므로 95% 신뢰수준 하에서 유의함(안정적인 시계열 자료임)

########################################
# 가장 적절한 arima 모델을 추천해주는 함수
# arima: AR모형과 MA모형을 결합한 함수
# arima(p,d,q)
# ar(p) 모형의 p,
# 자기회귀모형(Autoregressive model, AR) : 이전의 값이 이후의 값에 영향을 미치는 모형
# p시점 이전의 자료가 현재 자료에 영향을 주는 모형
# 자기상관함수(Autocorrelation Function, ACF): k기간 떨어진 값들의 상관계수
# 부분자기상관함수(Partial ACF): 서로 다른 두 시점의 중간에 있는 값들의 영향을 제외시킨 상관계수
# ar(1) : 직전 데이터가 다음 데이터에 영향을 줄 경우
# d : 차분차수
# ma(q) 모형의 q
# 이동평균(Moving Average model, MA) : 시간이 지날수록 평균값이 지속적으로 증가하거나 감소하는 경향
# ma(1) 직전 데이터가 다음 데이터에 영향을 주는 경우

########################################
# 자기회귀통합이동평균 모형(Autoregressive integrated movingaverage model, ARIMA)
# ARIMA(p,d,q) p AR모형의 차수, d 차분의 차수, q MA 모형의 차수
auto.arima(ts(kings))
# 가장 적절한 모형은 arima(0,1,1)

########################################
# arima(1,2,1)을 수행할 경우 arima(data, order=c(1,2,1))로 모형을 생성함
kings_arima <- arima(kings_ts, order = c(0,1,1)) # 차분을 통해 확인한 값 적용
# h 예측하고 싶은 수치(42명의 왕의 데이터가 있고 47번째 왕의수명을 예측하고 싶음)
# 67.75063으로 예측

########################################
# 미래예측함수
kings_fcast <- forecast(kings_arima, h = 5)
kings_fcast
win.graph(); plot(kings_fcast)

########################################
# acf(), pacf() 함수로 얻은 파라미터를 입력한 모형
kings_arima2 <- arima(kings_ts, order = c(3,1,1))
kings_arima2

########################################
kings_fcast <- forecast(kings_arima2, h = 5)
kings_fcast
win.graph(); plot(kings_fcast)
