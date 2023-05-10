# 데이터셋 구조
names(iris) # 변수명(컬럼)
attributes(iris) # 변수명, 자료형, 행의 인덱스
str(iris) # 데이터의 구조 보기
#############################################################
# iris 데이터셋을 8:2 비율로 구분
set.seed(123)
dim(iris)
#############################################################
idx <- sample(1:nrow(iris), nrow(iris) * 0.8)
tr <- iris[idx, ]
te <- iris[-idx, ]
dim(tr)
head(tr)
head(te)
#############################################################
# K겹 교차검정 데이터셋 생성
name <- c('김철수', '이미영', '홍상수', '이찬수', '송희연', '최승희', '박만수', '민지영', '정성호', '한상수')
score <- c(90, 85, 70, 85, 60, 66, 77, 88, 99, 100)
df <- data.frame(Name=name, Score=score)
df
#############################################################
# install.packages('cvTools')
set.seed(123)
library(cvTools)
# n 데이터샘플수, K 교차검증횟수
# type : random, consecutive, interleaved
# subset - 인덱스값, which 몇번째 분할인지를 나타내는 인덱스
# 전체 10세트, K=5 5세트x2 = 10세트
cross <- cvFolds(n=10, K=5, type='random')
cross
# k가 1일 때, 3, 9를 검증용으로 나머지를 학습용으로
# k가 2일 때, 10, 1을 검증용으로 나머지를 학습용으로
#############################################################
# which를 이용하여 subsets 데이터 참조
cross$subsets[cross$which == 1, 1] # k=1인 경우
cross$subsets[cross$which == 2, 1] # k=2인 경우
cross$subsets[cross$which == 3, 1] # k=3인 경우
cross$subsets[cross$which == 4, 1] # k=4인 경우
cross$subsets[cross$which == 5, 1] # k=5인 경우
str(cross)
#############################################################
set.seed(123)
# 연속된 데이터를 순서대로 검증용 데이터로 사용하는 방식
cross <- cvFolds(n=10, K=5, type="consecutive")
cross
# k가 1일 때, 1, 2를 검증용으로 나머지를 학습용으로
# k가 2일 때, 3, 4를 검증용으로 나머지를 학습용으로
#############################################################
set.seed(123)
# 연속된 데이터를 차례로 서로 다른 k의 검증 데이터로 할당하는 방식
cross <- cvFolds(n=10, K=5, type='interleaved')
cross
#############################################################
for(k in 1:5){
  idx <- cross$subsets[cross$which==k, 1]
  cat('\nk=', k, '학습용\n')
  print(df[-idx, ])
  cat('\nk=', k, '검증용\n')
  print(df[idx, ])
}
#############################################################
# install.packages('e1071')
set.seed(123)
library(caret)
library(e1071)
# method='cv' 교차검증, number=5 5회
trControl <- trainControl(method = 'cv', number = 5)
# tuneGrid k값을 1~10까지 테스트
fit <- train(Species ~. ,
  method = 'knn',
  tuneGrid = expand.grid(k = 1:10),
  trControl = trControl,
  metric = "Accuracy",
  data = iris)
fit
#############################################################
set.seed(123)
library("class")
# 구간 설정
group1 <- cut(seq(1, 50), breaks=5, labels=F)
group2 <- cut(seq(51, 100), breaks=5, labels=F)
group3 <- cut(seq(101, 150), breaks=5, labels=F)
fold <- c(group1, group2, group3)
acc <- c()
# 실험 5회
for(i in 1:5){
  ds.tr <- iris[fold != i, 1:4] # 학습용
  ds.te <- iris[fold == i, 1:4] # 검증용
  cl.tr <- factor(iris[fold != i, 5])
  cl.te <- factor(iris[fold == i, 5])
  # knn 모형 knn(학습용, 검증용, 라벨, 이웃의수)
  pred <- knn(ds.tr, ds.te, cl.tr, k=7)
  # 정확도
  acc[i] <- round(mean(pred==cl.te) * 100, 1)
}
acc
mean(acc)