df <- read.csv("./data/iris/iris.csv")
head(df)

library(dplyr)
df <- df %>% select(-Name)
tbl <- table(df$Species)
tbl

library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df$Species, p=0.8, list=F)
# 학습용
train <- df[idx_train, ]
X_train <- train[, -5]
y_train <- train[, 5]
# 검증용
test <- df[-idx_train, ]
X_test <- test[, -5]
y_test <- test[, 5]

library(neuralnet)
# threshold : 에러의 감소분이 threshold 값보다 작으면 stop
set.seed(123)
model <- neuralnet(as.factor(Species) ~ ., data=train, hidden=10,
                    threshold=0.01, linear.output=F)
# model$result.matrix # 가중치 정보
win.graph(); plot(model)
pred <- predict(model, X_test, type='prod')
pred

# apply(x, direction, function) direction: 1 행방향, 2 열방향
result <- apply(pred, 1, function(x) which.max(x)-1)
result
table(y_test, result)
mean(y_test == result)
