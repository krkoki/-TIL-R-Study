df <- read.csv('./data/ozone/ozone2.csv')
head(df)

library(dplyr)
# 필드 제거
df <- df %>% select(-Month, -Day, -Result)
head(df)

library(caret)
# 랜덤 시드 고정
set.seed(123)
idx_train <- createDataPartition(y=df$Ozone, p=0.8, list=F)
# 학습용
train <- df[idx_train, ]
X_train <- train[, -1]
X_train
y_train <- train[, 1]
y_train
# 검증용
test <- df[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]

library(neuralnet)
set.seed(123)
model <- neuralnet(Ozone ~ ., data=train, hidden=10, threshold=0.05, stepmax=1e7)
win.graph(); plot(model)
pred <- predict(model, X_test)
pred
mean((y_test - pred)^2) # 평균제곱오차
