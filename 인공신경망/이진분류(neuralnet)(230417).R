df <- read.csv('./data/diabetes/data.csv')
head(df)
library(dplyr)

# 필드 제거
df <- df %>% select(-target)
df

# 불균형 데이터셋
tbl <- table(df$target2)
tbl
win.graph(); barplot(tbl, beside=T, legend=T, col=rainbow(2))

# 언더샘플링
library(ROSE)
# method: under,over,both N: 샘플링 후의 샘플 개수(적은 쪽 x 2) 또는 p=0.5 50:50으로 선택
df_samp <- ovun.sample(target2 ~ . ,data = df, seed=1, method= "under", N=195*2)$data
tbl <- table(df_samp$target2)
tbl

library(caret)
# 랜덤 시드 고정
set.seed(123)
# 학습용:검증용 8:2로 구분
# list=FALSE, 인덱스값들의 리스트를 반환하지 않음
idx_train <- createDataPartition(y=df_samp$target2, p=0.8, list=FALSE)
# 학습용
train <- df_samp[idx_train, ]
X_train <- train[, -11]
y_train <- train[, 11]
# 검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -11]
y_test <- test[, 11]

library(neuralnet)
# threshold : 에러의 감소분이 threshold 값보다 작으면 stop(학습중지기준)
# linear.output 회귀분석인 경우 T, 분류인 경우 F
set.seed(123)
# model <- neuralnet(target2 ~ ., data=train, hidden=10, threshold=0.01, linear.output = F)
# install.packages('NeuralNetTools')
# library(NeuralNetTools) # 특성중요도를 그래프로 확인할 수 있다.
# win.graph(); garson(model)

model <- neuralnet(as.factor(target2) ~ ., data=train, hidden=c(10, 10), linear.output = F)

# model$result.matrix #가중치 정보
win.graph();plot(model)

# 변수의 중요도 그래프는 출력층의 노드가 1개일 때만 출력 가능함
# 세부적인 계산 결과
# pred<-compute(model, X_test)
# pred
# 출력결과(확률)
pred<-predict(model, X_test, type='prob')
pred

# 각 샘플에 대해 0,1로 출력
# 0 열방향(세로), 1 행방향(가로)
result <- apply(pred, 1, function(x) which.max(x)-1)
result
table(y_test , result)
mean(y_test == result)

library(Epi)
win.graph();ROC(test=result, stat=y_test, plot="ROC", AUC=T, main="신경망")