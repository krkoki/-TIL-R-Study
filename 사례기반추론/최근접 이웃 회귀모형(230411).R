df <- read.csv("../Python2/data/ozone/ozone2.csv")
head(df)
#############################################################
library(dplyr)
# 필드 제거
df <- df %>% select(-Month, -Day, -Result)
library(caret)
# 랜덤 시드 고정
set.seed(123)
# 학습용, 검증용 구분
idx_train <- createDataPartition(y=df$Ozone, p=0.8, list=F)
#학습용
train <- df[idx_train, ]
X_train <- train[, -4]
y_train <- train[, 4]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -4]
y_test <- test[, 4]
head(X_train)
head(y_train)
#############################################################
# install.packages("FNN")
library(FNN)
acc <- NULL
for(i in c(1:10)){
  set.seed(123) # 재현성을 위하여 랜덤 시드값 고정
  model <- knn.reg(X_train, X_test, y_train, k=i)
  #                 학습X,  검증X,  학습y
  m <- mean((model$pred - y_test)^2)
  acc <- c(acc, m)
}
# 차트를 그리기 위해 데이터프레임으로 변환
df <- data.frame(k=c(1:10), mse=acc)
df
# k에 따른 분류 정확도 그래프 그리기
win.graph(); plot(mse ~ k, data=df, type="o", pch=20, main="최적의 k값", col="red")
#############################################################
# 그래프에 k 라벨링 하기
with(df, text(mse ~ k, labels = c(1:10), pos = 1, cex = 0.7))
n <- min(df[df$mse %in% min(acc), "k"])
df[n, ]
#############################################################
model <-knn.reg(X_train, X_test, y_train, k=6)
model$pred # 예측값
#############################################################
# Mean Squared Error (평균제곱오차)
mean((model$pred - y_test)^2)