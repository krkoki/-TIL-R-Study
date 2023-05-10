df <- read.csv("d:/workspace/Python2/data/rides/rides2.csv")
head(df)

# 0, 1 카운트
tbl <- table(df$overall)
tbl

# 카운트 플롯, beside 옆으로 나란히
win.graph();barplot(tbl, beside=T, legend=T, col=rainbow(2))

library(ROSE)
df_samp <- ovun.sample(overall ~ ., data=df, seed=1, 
  method='under', N=245*2)$data
tbl <- table(df_samp$overall)
tbl

library(caret)
set.seed(123)
idx_train <- createDataPartition(y=df_samp$overall, p=0.8, list=F)
train<- df_samp[idx_train, ]
X_train<- train[, -8] # 8번 필드 제외
y_train<- train[, 8] # 8번 필드만 선택
test<- df_samp[-idx_train, ]
X_test<- test[, -8]
y_test<- test[, 8]

library(reshape)
meltData <- melt(X_train)
win.graph();boxplot(data=meltData, value~variable)

X_train_scaled <- as.data.frame(scale(X_train))
meltData <- melt(X_train_scaled)
win.graph();boxplot(data=meltData, value~variable)

X_test_scaled <- as.data.frame(scale(X_test))
meltData <- melt(X_test_scaled)
win.graph();boxplot(data=meltData, value~variable)

train_scaled <- cbind(X_train_scaled, overall=y_train)
test_scaled <- cbind(X_test_scaled, overall=y_test)

# 로지스틱 회귀분석 모형 binomial 2진 분류
model <- glm(overall ~ ., data=train_scaled, family=binomial)
summary(model)
(coef1 <- coef(model))

reduced <- step(model, direction='backward')
summary(reduced)
(coef1 <- coef(reduced))

# 결과값이 0.0 ~ 1.0 사이로 출력
pred <- predict(model, newdata=X_test_scaled, type='response')
pred
result <- ifelse(pred > 0.5, 1, 0)
result
y_test
result
y_test == result
mean(y_test == result)
table(y_test, result)

library(ROCR)
pr <- prediction(pred, y_test)
pr
prf <- performance(pr, measure='tpr', x.measure = 'fpr')
win.graph(); plot(prf, main='ROC Curve')

# ROC Curve의 면적
auc <- performance(pr, measure = 'auc')
auc@y.values
auc <- auc@y.values[[1]]
auc

coef(reduced) # 회귀계수
# 자녀수가 많으면 만족도가 높다.
# 놀이기구가 좋으면 만족도가 높다.
# 청결도가 높으면 만족도가 높다.

exp(coef(reduced)) # 회귀계수를 지수함수로 변환