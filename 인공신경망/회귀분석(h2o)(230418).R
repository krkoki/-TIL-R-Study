df <- read.csv('./data/ozone/ozone2.csv')
head(df)

library(dplyr)
# 필드 제거
df<-df %>% select(-Result)

library(caret)
#랜덤 시드 고정
set.seed(123)
#학습용:검증용 8:2로 구분
#list=FALSE, 인덱스값들의 리스트를 반환하지 않음
idx_train <- createDataPartition(y=df$Ozone, p=0.8, list=F)
#학습용
train <- df[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]
head(X_train)
head(y_train)

library(h2o)
h2o.init()
set.seed(123)
tr_data <- as.h2o(train)
te_data <- as.h2o(test)
target <- "Ozone"

# 독립변수들의 이름
features <- names(train)[2:4]
features
model <- h2o.deeplearning(x = features, y = target,
                          training_frame = tr_data, ignore_const_cols = F,
                          hidden = c(8, 7, 5, 5))
summary(model)

# 예측값
pred <- h2o.predict(model, te_data)
pred

# MSE 계산
perf <- h2o.performance(model, newdata = te_data)
perf
# H2ORegressionMetrics : deeplearning
# MSE:  761.0901
# RMSE:  27.58786
# MAE:  19.83632
# RMSLE:  0.596276

h2o.mse(perf)