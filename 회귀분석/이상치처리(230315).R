df<-read.csv("d:/workspace/Python2/data/rides/rides.csv")
head(df)
#install.packages('car')
library(car)

# 회귀분석 모형
model <- lm(overall~num.child + distance + rides + games + 
              wait + clean, data=df)
summary(model)
# 설명력 68.27%

# 1. 아웃라이어
# 잔차가 2배 이상 크거나 2배 이하로 작은 경우
outlierTest(model)
# 이상치 데이터 발견 - 184번 샘플(Bonferonni p value가 0.05보다 작은 값)
# rstudent - Studentized Residual - 잔차를 잔차의 표준편차로 나눈 값
# unadjusted p-value : 다중 비교 문제가 있는 p-value
# 본페로니 p - 여러 개의 가설 검정을 수행할 때 다중 비교 문제로 인해 귀무가설을 기각하게 될 확률이 높아지는 문제를 교정한 p-value

# 184번 샘플을 제거한 모형
model2 <- lm(overall~num.child + distance + rides + games + 
               wait + clean, data=df[-184,])
summary(model2)
# 설명력이 68.27% -> 68.76% 로 개선됨

#2. 영향 관측치(influential observation) : 모형의 인수들에 불균형한 영향을 미치는 관측치
# 영향 관측치를 제거하면 더 좋은 모형이 될 수 있음
# Cook's distance(레버리지와 잔차의 크기를 종합하여 영향력을 판단하는 지표)를 이용하여
# 영향 관측치를 찾을 수 있음
# 레버리지(leverage) : 실제값이 예측값에 미치는 영향을 나타낸 값
# x축: Hat-Values(큰 값은 지렛점)
# y축: Studentized Residuals(표준화 잔차) : 잔차를 표준오차로 나눈 값
win.graph(); influencePlot(model)
#184,103,227,367,373
# 2보다 큰 값, -2보다 작은 값들은 2배 이상 떨어져있는 이상치)
#레버리지와 잔차의 크기가 모두 큰 데이터들은 큰 원으로 표현(영향력이 큰 데이터들)
#184,103,227,367,373

model3=lm(overall~num.child + distance + rides + games +
            wait + clean, data=df[c(-184,-103,-367,-373),])
model3
summary(model3)
# 설명력 69.12%