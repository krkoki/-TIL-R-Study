df <- read.csv('data/diabetes/data.csv')
head(df)
##########################################
library(dplyr)
# 필드 제거
df <- df %>% select(-target2)
df
dim(df)

X <- df[, -11]
y <- df[, 11]
##########################################
# 최적의 sigma(gamma)와 Cost
svm.Grid <- expand.grid(.sigma = c(0.0001, 0.001, 0.01, 0.1),
                        .C = c(0.001, 0.01, 0.1, 1, 10, 100))
model <- train(X, y,
                  method = "svmRadial",
                  preProcess = c("center", "scale"),
                  tuneGrid = svm.Grid,
                  tuneLength = 3,
                  trControl = trainControl(method = "cv"))
model
##########################################
# sigma 0.01, C 1
# RMSE(root mean square error, 평균 제곱근 오차)
# MAE(mean absolute error, 평균 절대 오차)
# MSE(mean squared error, 평균 제곱 오차)
##########################################
pred <- predict(model, X)
pred

d <- y-pred
d

mse <- mean(d^2)
mse

mae<-mean(abs(d))
mae

rmse<-sqrt(mse)
rmse

rsquared<- 1-(sum(d^2)/sum((y-mean(y))^2))
rsquared

MAE(pred, y) #기본함수

##########################################
library(caret)
RMSE(pred, y) #caret 패키지의 함수
R2(pred, y)