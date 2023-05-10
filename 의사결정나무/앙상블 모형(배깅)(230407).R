#install.packages('adabag')
library(adabag)
train <- sample(1:150, 100)

iris.bagging <- bagging(Species ~ ., data=iris[train, ],
  mfinal=5, control=rpart.control(maxdepth=5, minsplit=5))
iris.bagging$trees # 개별 모형의 결과

# 첫번째 모형의 트리 그래프
library(rpart.plot)
win.graph(); rpart.plot(iris.bagging$trees[[1]])

# 검증용 데이터셋을 입력하여 분류
predict(iris.bagging, iris[-train,])$class

# 오분류표(confusion matrix) 작성
tbl <- table(iris$Species[-train], predict(iris.bagging, iris[-train,])$class)

# 정분류율, 오분류율 계산
rate1 <- sum(tbl[row(tbl) == col(tbl)])/sum(tbl) #정분류율
rate2 <- 1 - rate1 # 오분류율
rate1
rate2































