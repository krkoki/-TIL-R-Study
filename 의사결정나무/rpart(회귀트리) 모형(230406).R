df <- read.csv("../Python2/data/ozone/ozone2.csv")
head(df)

library(dplyr)
# 필드 제거
df<-df %>% select(-Result, -Month, -Day)

library(caret)
#랜덤 시드 고정
set.seed(123)
#학습용:검증용 8:2로 구분
idx_train <- createDataPartition(y=df$Ozone, p=0.8, list=FALSE)
#학습용
train <- df[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
#검증용
test <- df[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]
head(X_train)
head(y_train)

#install.packages("reshape")
library(reshape)
meltData <- melt(X_train)
win.graph(); boxplot(data=meltData, value~variable)

# 정규화된 데이터를 data.frame형태로 변경
X_train_scaled <- as.data.frame(scale(X_train))
X_test_scaled <- as.data.frame(scale(X_test))

# 데이터프레임 연결(가로방향)
train_scaled <- cbind(X_train_scaled, Ozone=y_train)
test_scaled <- cbind(X_test_scaled, Ozone=y_test)
head(train_scaled)
head(test_scaled)

meltData <- melt(X_train_scaled)
win.graph(); boxplot(data=meltData, value~variable)

library(rpart)
# 최대 깊이를 제한
a <- rpart.control(maxdepth = 20)
model <- rpart(Ozone ~ ., method='anova', data=train, control=a)

# 트리 그래프
win.graph()
plot(model, uniform=T, main="Tree")
text(model, use.n=T, all=T, cex=.8)

# 가지치기를 위한 최적의 노드 개수 확인
# CP(Complexity Parameter, 복잡성 매개변수) : 에러율이 가장 낮을 때의 노드 개수
printcp(model)
win.graph()
plotcp(model) # 교차검증 결과
summary(model) # 세부적인 노드 정보

# 트리를 pdf로 저장하기 위한 postscript 생성
# 생성된 ps 파일을 더블클릭한 후 Acrobat Distiller 프로그램을 이용하여 pdf 파일을 생성함
post(model, file = "d:/workspace/R2/data/ozone/ozone_ptree.ps", title = "Tree Model")

# 가지치기
# 에러율이 가장 낮을 때의 CP값으로 가치치기 진행
pfit <- prune(model, cp=model$cptable[which.min(model$cptable[, "xerror"]), "CP"])
win.graph()
plot(pfit, uniform=T, main="Pruned Tree")
text(pfit, use.n=T, all=T, cex=.8)
post(pfit, file = "d:/workspace/R2/data/ozone/ozone_ptree.ps", title ="Pruned Tree")

# 트리를 시각적으로 꾸며주는 패키지
library(rattle)
library(rpart.plot)
win.graph(); rpart.plot(pfit) #기본형 트리
win.graph(); fancyRpartPlot(pfit) #파스텔톤의 트리

# predict(트리모델, 학습용데이터셋), 회귀트리모형은 새로운 데이터는 잘 못맞추는 단점이 있음
pred <- predict(pfit,newdata=X_test_scaled)
pred
#평균제곱근오차(Root Mean Square Error) : 실제값과 예측값의 차이를 나타내는 지표
RMSE(pred = pred, obs = y_test)