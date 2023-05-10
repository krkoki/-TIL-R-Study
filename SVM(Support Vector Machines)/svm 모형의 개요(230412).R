set.seed(123)

# rnorm(n) 정규분포 난수 n개를 생성하는 함수
x <- matrix(rnorm(40), 20, 2) # 20행 2열의 행렬

# rep 반복함수
y <- rep(c(-1, 1), c(10, 10))
x[y==1, ] <- x[y==1, ]+1
##########################################
library(e1071)
dat <- data.frame(x, y=as.factor(y))
# kernel = 'linear' 선형판별함수, scale=F 스케일링을 하지 않음
svmfit <- svm(y ~ ., data=dat, kernel='linear', cost=10, scale=F)
svmfit

win.graph(); plot(svmfit, dat) # 분류 그래프
##########################################
make.grid <- function(x, n = 75) {
  grange <- apply(x, 2, range) # x값의 세로방향 범위
  x1 <- seq(from = grange[1, 1], to = grange[2, 1], length=n)
  x2 <- seq(from = grange[1, 2], to = grange[2, 2], length=n)
  enpand.grid(X1 = x1, X2 = x2) # x1, x2의 모든 조합
}

xgrid <- make.grid(x)
xgrid[1:10, ]
##########################################
ygrid <= predict(svmfit, xgrid)
ygrid
win.graph()
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col = y + 3, pch = 19) # 샘플
points(x[svmfit$index, ], pch = 5, cex = 2) # 서포트 벡터