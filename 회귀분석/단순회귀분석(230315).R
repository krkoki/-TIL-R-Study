# 20명의 신장과 체중 데이터
height <- c(179,166,175,172,173,167,169,172,172,179,161,174,166,176,182,175,177,167,176,177)
weight <- c(113,84,99,103,102,83,85,113,84,99,51,90,77,112, 150,128,133,85,112,85)
plot(height,weight)

# 상관계수 계산
cor(height, weight)

# 기울기와 절편
slope <- cor(height, weight) * (sd(weight) / sd(height))
intercept <- mean(weight) - (slope * mean(height))
slope
intercept

# 단순회귀분석 모델 생성
# 체중 = 기울기 x 신장 + 절편
df <- data.frame(height, weight)
df

model <- lm(weight ~ height, data=df)
# 절편(intercept)
# 기울기
model

# 키가 180인 사람의 체중 예측
model$coefficients[[2]]*180 + model$coefficients[[1]]
summary(model)

plot(height, weight)
abline(model, col='red')

weight

pred <- model$coefficients[[2]] * height + model$coefficients[[1]]
pred

sum(weight-pred) # 오차와 합계는 0

err <- (weight-pred)^2

sum(err) # 오차와 제곱합

sum(err/length(weight)) # 평균제곱오차
# 비용함수(cost function) : 평균제곱오차를 구하는 함수

# 최적의 가중치(기울기)를 구하기 위한 계산(경사하강법, Gradient Descent)
# 여기서는 전체의 값이 아닌 1개의 값만 계산
x <- height[1]
y <- weight[1]
w <- seq(-1, 2.3, by=0.0001) # 가중치, by 간격
#w <- seq(-1, 2.3, by=0.1) # 가중치, by 간격
pred <- x*w # 예측값
err <- (y-pred)^2 # 제곱오차
win.graph(); plot(err)
# 기울기가 증가하면 오차가 증가하고 기울기가 감소하면 오차가 감소한다.
# 기울기가 0에 가까운 값이 최적의 기울기가 된다.
min(err) # 최소오차
i <- which.min(err) # which.min = 파이썬의 np.argmin 과 같다.
paste('최적의 기울기', w[i])

#최적의 편향(절편)을 구하기 위한 계산
x<-height[1]
y<-weight[1]
w<-0.6313 #가중치
b<-seq(-3.2,3.2,by=0.0001) #편향
#b<-seq(-1,3.2,by=0.1) #편향
pred<-x*w + b #예측값
err<-(y-pred)^2 #제곱오차
plot(err)
#기울기가 증가하면 오차가 증가하고 기울기가 감소하면 오차가 감소한다
#기울기가 0에 가까운 값이 최적의 기울기가 된다.
min(err) #최소오차
i<-which.min(err)
i
paste('최적의 편향=',b[i])

#위의 계산을 통해 얻은 최적의 w,b를 적용한 회귀식
x<-height[1]
y<-weight[1]
w<- 0.6313
b<- -0.00269999999999992
pred<-x*w + b
y
pred

regression <- read.csv("d:/workspace/R2/data/regression/regression(utf8).csv")
head(regression)
tail(regression)

summary(regression)
hist(regression$height)
hist(regression$weight)


# 상관계수를 구함
cor(regression$height, regression$weight)

# 키와 몸무게의 관계
# 독립변수 : 신장
# 종속변수 : 체중
# 귀무가설 : 신장은 체중에 영향을 주지 않을 것이다.
# 대립가설 : 신장은 체중에 영향을 줄 것이다.

# lm( y ~ x ) x 독립변수, y 종속변수 (x가 한단위 증가할 때 y에게 미치는 영향)
r <- lm(regression$weight ~ regression$height)
plot(regression$weight ~ regression$height, 
  main="평균키와 몸무게", xlab="Height", ylab="Weight")
abline(r,col='red') # 회귀선

# 키가 180인 사람의 체중 예측
r$coefficients[[2]] * 180 + r$coefficients[[1]]
summary(r)

# Call:
# lm(formula = regression$weight ~ regression$height)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -9.2327 -5.9811  0.0915  5.3125 10.4346 
# 
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -40.86594    4.22768  -9.666 1.42e-10 ***
# regression$height   0.61474    0.02998  20.508  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.355 on 29 degrees of freedom
# Multiple R-squared:  0.9355,	Adjusted R-squared:  0.9333 
# F-statistic: 420.6 on 1 and 29 DF,  p-value: < 2.2e-16

# Call : 회귀분석에 사용된 모델 식
# Residuals: 잔차, 회귀선의 값과 실제 관측 값의 차이를 각 분위수로 표시
# Coefficients: 절편, 독립변수 등에 대한 회귀계수
# Residual standard error: 잔차의 표준오차와 자유도
# Multiple R-squared: 결정계수, 즉 추정된 회귀선이 실제 관측값을 얼마나 잘 설명하는가를 의미하는 값. 0에서 1사이의 값을 가지며 1은 실제관측 값들이 회귀선 상에 위치함을 의미함
# Adjusted R-squared: 수정결정계수, 변수가 많아지면 R제곱이 무조건 높아지는 단점을 보완한 것, R제곱과 큰 차이가 나지 않을수록 좋은 모형
# F-statistic: F통계량은 해당 모형이 의미가 있는지 아닌지를 알려줌. 계수 중 하나라도 0이 아닌 것이 있다면 그 모형은 유의미하다고 판단함.