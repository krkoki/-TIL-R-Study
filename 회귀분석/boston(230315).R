library(MASS)
head(Boston)
tail(Boston)
dim(Boston)
summary(Boston)

win.graph(); pairs(Boston) # 산점도 행렬

plot(medv ~ crim, data=Boston, main="범죄율과 주택가격과의 관계",  xlab="범죄율", ylab="주택가격")

# 범죄율과의 상관계수 행렬
(corrmatrix <- cor(Boston)[1,]) # 첫번째 변수
# 범죄율이 높으면 주택가격이 떨어진다.

#강한 양의 상관관계, 강한 음의 상관관계
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5]

# 세율과의 상관계수 행렬
(corrmatrix <- cor(Boston)[10,])

# 강한 양의 상관관계, 강한 음의 상관관계
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5]

# CHAS : 찰스강의 경계에 위치한 경우는 1, 아니면 0
table(Boston$chas)

# 최고가로 팔린 주택들
(seltown <- Boston[Boston$medv == max(Boston$medv),])

# 최저가로 팔린 주택들
(seltown <- Boston[Boston$medv == min(Boston$medv),])

# 다중회귀분석 모델 생성
(model <- lm(medv ~., data=Boston))

# 분석결과 요약
summary(model)
# p-value가 0.05보다 작으므로 통계적으로 유의함
# 모델의 설명력(예측의 정확성) 73.3%

# 전진선택법과 후진제거법
# 후진제거법:기여도가 낮은 항목을 제거함으로써 의미있는 회귀식을 구성하는 과정
reduced <- step(model, direction="backward")
# 최종적으로 선택된 변수들 확인

#install.packages('olsrr')
library(olsrr)
#model2<-ols_step_forward_p(model)
model2<-ols_step_forward_p(model,details=TRUE)
model2


model3<-ols_step_backward_p(model,details=TRUE)
model3

# 최종 결과 확인
summary(reduced)
# p-value가 0.05보다 작으므로 이 회귀모델은 통계적으로 유의함.
# 모델의 설명력(신뢰도,예측정확성) : 73.4%