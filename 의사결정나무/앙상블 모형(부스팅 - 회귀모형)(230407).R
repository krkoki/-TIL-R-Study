# install.packages('gbm')
library(MASS)
library(gbm)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)

# 부스트 모형, 트리개수 100개
boost.boston <- gbm(medv ~., data=Boston[train,], n.trees=100)

# 변수의 영향력 그래프
win.graph(); summary(boost.boston)

# 가장 영향력이 큰 변수 2개
# rm : 방의 수, 양의 상관관계
win.graph(); plot(boost.boston,i="rm")
# lstat : 인구 중 하위 계층 비율, 음의 상관관계
win.graph(); plot(boost.boston,i="lstat")

boston.test <- Boston[-train, "medv"]

# 검증용 데이터셋을 입력하여 얻은 예측값
pred <- predict(boost.boston, newdata=Boston[-train, ], n.trees=100)
mean((pred-boston.test)^2)
# 오차 약 22000 달라