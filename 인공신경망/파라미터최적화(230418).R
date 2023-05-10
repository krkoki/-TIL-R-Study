library(nnet)
df <- read.csv('./data/ozone/ozone2.csv')
head(df)

library(dplyr)
df <- df %>% select(-Ozone)

library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df$Result, p=0.8, list=F)
# 학습용
train <- df[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
# 검증용
test <- df[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]

# 은닉층의 노드 개수를 튜닝하는 함수
test.rate <- function(h.size){
#size 은닉층의 노드개수
model <- nnet(as.factor(Result) ~ ., data = train, size = h.size)
pred <- predict(model,X_test,type='class')
rate <- mean(y_test == pred)
c(h.size, rate)
}

# sapply(simple apply) 함수를 활용해 1~n개의 은닉노드 개수에 따른 함수 적용 결과를 출력, sapply(입력값, FUN=함수이름)
sapply(1:5, FUN = test.rate)

# t() 행렬전치(행과 열을 바꿈)
out <- t(sapply(10:50, FUN = test.rate))
out
out[which.max(out[, 2]), ] # 오차가 최소인 은닉노드의 개수와 오차 확인
win.graph(); plot(out, type="b", xlab="hidden nodes", ylab="accuracy")

library(e1071)
set.seed(123)
tmodel <- tune.nnet(as.factor(Result) ~ ., data = train, size= 10:50)
summary(tmodel)
bestmodel <- tmodel$best.model
summary(bestmodel)
pred <- predict(bestmodel, X_train)
result <- ifelse(pred>0.5, 1, 0)
table(y_train, result)
mean(y_train == result)

# expand.grid 가능한 모든 파라미터의 조합을 만드는 함수
# decay 가중치를 조절하기 위한 옵션
my.grid <- expand.grid(.decay = c(0.3), .size = 10:50)

# train() 함수를 통해 최적 모형 학습
fit <- train(as.factor(Result) ~ ., data=train, method="nnet",
              tuneGrid=my.grid, trace=F)
fit

pred <- predict(fit, newdata=test)
pred

table(y_test, pred)
mean(y_test == pred)