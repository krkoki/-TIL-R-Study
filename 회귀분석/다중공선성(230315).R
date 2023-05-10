# 다중공선성(Multicollinearity) : 독립변수끼리 강한 상관관계를 가지는 현상
# 다중공선성을 파악하기 위한 수치적 지표
# VIF(Variance Inflation Factor, 분산팽창인자)
# VIFi= 1 / ( 1 - R^2i)

library(car)

# 미국 미니애폴리스 지역의 총인구, 백인비율, 흑인비율, 외국태생, 가계소득, 빈곤, 대학졸업비율을 추정한 데이터셋
df <- MplsDemo
head(df)

win.graph(); plot(df[, -1])
cor(df[, 2:7]) # 독립변수들의 상관계수

#install.packages('corrplot')
library(corrplot)
win.graph(); corrplot(cor(df[, 2:7]), method='number')
# white 변수의 경우 다른 변수들과 상관관계가 높음 (다중공선성이 의심됨)

model1 <- lm(collegeGrad ~.-neighborhood, data=df)
summary(model1)
# 설명력은 81.86%로 좋은 모형이지만
# black(흑인비율), foreignBorn(외국태생) 변수의 회귀계수가 양수로 출력됨
# 실제 현상을 잘 설명하지 못하는 모형

# white 변수를 제거한 모형
model2 <- lm(collegeGrad ~.-neighborhood-white, data=df)
summary(model2)
#설명력은 다소 떨어졌지만 회귀계수가 실제 현상을 잘 설명하는 것으로 보임
#black(흑인비율)이 음수로 바뀌었음, foreignBorn(외국태생) 변수는 양수이지만 유의하지 않음

# 다중공선성에 대해 확인이 필요한 경우
# p-value가 유의하지 않은 경우
# 회귀계수의 부호가 예상과 다른 경우
# 데이터를 추가, 제거시 회귀계수가 많이 변하는 경우

model <- lm(population ~ .-collegeGrad-neighborhood, data=df)

print(paste("population의 VIF : ", 
  (1-summary(model)$r.squared)^{-1}))# ^{-1} -1승 중괄호를 안써도 됨

# 다중공선성이 매우 높은 변수
model <- lm(white ~.-collegeGrad-neighborhood, data=df)
print(paste("white의 VIF : ", (1-summary(model)$r.squared)^{-1}))

model<-lm(black~.-collegeGrad-neighborhood,data=df)
print(paste("black의 VIF : ",(1-summary(model)$r.squared)^{-1}))

model<-lm(foreignBorn~.-collegeGrad-neighborhood,data=df)
print(paste("foreinBorn의 VIF : ",(1-summary(model)$r.squared)^{-1}))

model<-lm(hhIncome~.-collegeGrad-neighborhood,data=df)
print(paste("hhIncome의 VIF : ",(1-summary(model)$r.squared)^{-1}))

model<-lm(poverty~.-collegeGrad-neighborhood,data=df)
print(paste("poverty의 VIF : ",(1-summary(model)$r.squared)^{-1}))

#다중공선성을 계산해주는 함수
vif(model1)
# 다중공선성이 높은 white 변수 제거
model2<-lm(collegeGrad~.-neighborhood-white,data=df)
summary(model2)
vif(model2)
# vif 수치가 많이 낮아졌고 특히 black의 수치도 많이 낮아졌음