df<-read.csv("../Python2/data/ozone/ozone2.csv")
head(df)
#############################################################
library(dplyr)
# 필드 제거
df<-df %>% select(-Ozone)
head(df)
dim(df)
summary(df)
#############################################################
# 상관계수 행렬
(corrmatrix <- cor(df))
#############################################################
library(corrplot)
corrplot(cor(df), method="circle")
#############################################################
# 불균형 데이터셋
tbl<-table(df$Result)
tbl
barplot(tbl, beside = TRUE, legend = TRUE, col = rainbow(2))
#############################################################
# under sampling
# install.packages("ROSE")
library(ROSE)
# method: under,over,both N: 샘플링 후의 샘플 개수(적은 쪽 x2) 또는 p=0.5 50:50으로 선택
df_samp <- ovun.sample(Result ~ . ,data = df, seed=1, method =
"under", N=72*2)$data
tbl<-table(df_samp$Result)
tbl

library(caret)
# 랜덤 시드 고정
set.seed(123)
# 학습용:검증용 8:2로 구분
# list=FALSE, 인덱스값들의 리스트를 반환하지 않음
idx_train <- createDataPartition(y=df_samp$Result, p=0.8,
list=FALSE)
# 학습용
train <- df_samp[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
# 검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]
head(X_train)
head(y_train)
#############################################################
# install.packages("party")
library(party)
# 기계학습 모델 생성
model <- ctree(Result ~ ., data = train)
model
plot(model)
#############################################################
plot(model, type='simple')
# predict(트리모델, 학습용데이터셋)
pred <- predict(model, X_train)
# 0.5보다 크면 1, 아니면 0으로 설정
result <- ifelse(pred>0.5,1,0)
head(pred)
head(y_train)
# 분류 모델 평가를 위해 오분류표(confusion matrix) 출력
table(y_train, result)
mean(result == y_train)
#############################################################
# predict(트리모델, 검증용데이터셋)
pred <- predict(model, X_test)
#0.5보다 크면 1, 아니면 0으로 설정
result <- ifelse(pred>0.5,1,0)
head(pred)
head(y_test)
# 분류 모델 평가를 위해 오분류표(confusion matrix) 출력
table(y_test, result)
mean(result == y_test)
#############################################################
# 랜덤 포레스트 모형
# install.packages('randomForest')
library(randomForest)
set.seed(1)
# mtry 변수의 개수, ntree 트리의 개수, importance=T 변수의 중요도로 모델 생성
model <- randomForest(Result ~ . , data=train,
mtry = 3, ntree = 5, importance = T)
model
#############################################################
# predict(학습용데이터셋)
pred <- predict(model, X_train)
# 0.5보다 크면 1, 아니면 0으로 설정
result <- ifelse(pred>0.5,1,0)
head(pred)
head(y_train)
# 분류 모델 평가를 위해 오분류표(confusion matrix) 출력
table(y_train, result)
mean(result == y_train)
#############################################################
# predict(검증용데이터셋)
pred <- predict(model, X_test)
# 0.5보다 크면 1, 아니면 0으로 설정
result <- ifelse(pred>0.5,1,0)
head(pred)
head(y_test)
# 분류 모델 평가를 위해 오분류표(confusion matrix) 출력
table(y_test, result)
mean(result == y_test)
#############################################################
importance(model) # 특성의 중요도
# %IncMSE 정확도, IncNodePurity 중요도
varImpPlot(model)