# install.packages("neuralnet")
library(neuralnet)
set.seed(100)
X <- as.matrix(sample(seq(-2, 2, length=50), 50, replace=FALSE), ncol=1)
# seq(start, end, length 자료수)
# sample(리스트, 샘플수, replace=F(비복원추출 = 중복값 No))
y <- X^2
win.graph(); plot(y ~ X)
df <- as.data.frame(cbind(X, y))
colnames(df) <- c("X", "y")
df

# 신경망 모형
nn <- neuralnet(y ~ X, data=df, hidden=c(10, 10))
win.graph(); plot(nn)
test <- as.matrix(sample(seq(-2, 2, length=10), 10, replace=F),ncol=1)
pred <- predict(nn, test)
test^2 # 실제값
pred # 예측값
mean((pred - test^2)^2)
result <- cbind(test, test^2, pred)
colnames(result) <- c("test", "test^2", "pred")
result