library(party)
# bootstrap 데이터 생성
data_boot1 <- iris[sample(1:nrow(iris), replace = T), ] # replace=T 복원추출(중복가능)
data_boot2 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot3 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot4 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot5 <- iris[sample(1:nrow(iris), replace = T), ]

# modeling
tree1 <- ctree(Species ~ ., data_boot1)
tree2 <- ctree(Species ~ ., data_boot2)
tree3 <- ctree(Species ~ ., data_boot3)
tree4 <- ctree(Species ~ ., data_boot4)
tree5 <- ctree(Species ~ ., data_boot5)

win.graph(); plot(tree1)

pred1 <- predict(tree1, iris)
pred2 <- predict(tree2, iris)
pred3 <- predict(tree3, iris)
pred4 <- predict(tree4, iris)
pred5 <- predict(tree5, iris)

# 각각의 예측 결과를 취합
test <- data.frame(Species = iris$Species, pred1, pred2, pred3, pred4, pred5)
head(test)

# 5개 모형의 결과를 취합하여 최종 결과를 voting
myfunc <- function(x) {
  result <- NULL
  for (i in 1:nrow(x)) {
    xtab <- table(t(x[i, ]))
    print(xtab)
    # versicolr 1 / virginica 4 라면 다수결로 virginica로 채택
    rvalue <- names(sort(xtab, decreasing = T)[1])
    result <- c(result, rvalue)
  }
  return(result)
}

test$result <- myfunc(test[ , 2:6])
test$result<-as.factor(test$result)
confusionMatrix(test$result, test$Species)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    