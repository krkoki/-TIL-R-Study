# 교차검증을 지원하는 svm 모형
# install.packages('kernlab')
library(kernlab)

# 스팸데이터
data(spam)
head(spam)
tail(spam)
##########################################
#학습용:검증용 5:5로 구분
index <- sample(1:dim(spam)[1])
       # sample(벡터, 개수)
spamtrain <- spam[index[1:2300], ]
spamtest <- spam[index[2301:4600], ]
nrow(spamtrain)
nrow(spamtest)
##########################################
# ksvm 모형
model <- ksvm(type~.,data=spamtrain,cross=3) #교차검증 3회
model
##########################################
# 분류
mailtype <- predict(model,spamtest[,-58])
mailtype
#성능평가
table(mailtype,spamtest[,58])
mean(mailtype == spamtest[,58])
##########################################
#iris
df<-read.csv("data/iris/iris.csv")
head(df)
##########################################
library(dplyr)
# 필드 제거
df<-df %>% select(-Name)
head(df)
tbl<-table(df$Species)
tbl
##########################################
library(caret)
#랜덤 시드 고정
set.seed(123)
#학습용:검증용 8:2로 구분
#list=FALSE, 인덱스값들의 리스트를 반환하지 않음
idx_train <- createDataPartition(y=df$Species, p=0.8, list=FALSE)
# 학습용
train <- df[idx_train, ]
X_train <- train[, -5]
y_train <- train[, 5]
# 검증용
test <- df[-idx_train, ]
X_test <- test[, -5]
y_test <- test[, 5]
##########################################
irismodel<- ksvm(as.factor(y_train)~.,data=X_train,cross=3)
irismodel
##########################################
# 학습
fitted(irismodel)
pred<-predict(irismodel, X_test)
pred
table(y_test , pred)
mean(y_test == pred)
##########################################
# svm 그래프
x <- rbind(matrix(rnorm(120),120,2),matrix(rnorm(120,mean=3),120,2))
y <- matrix(c(rep(1,60),rep(-1,60)))
svp <- ksvm(x,y,type="C-svc",cross=3) #분류 svm
win.graph(); plot(svp,data=x)
##########################################
# svm회귀모형
x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)
regm <- ksvm(x,y,cross=3)
win.graph(); plot(x,y,type="l")
lines(x,predict(regm,x),col="red")