# 성별(Sex), 나이(Age), 객실등급(Pclass), 요금(Fare)이 생존에 어느 정도의 영향을 미쳤는가?
df<-read.csv("d:/workspace/Python2/data/titanic/train3.csv")
head(df)
tail(df)

# 상관계수 행렬
(corrmatrix <- cor(df))

library(corrplot)
win.graph();corrplot(cor(df), method='circle')

# 불균형 데이터셋
tbl <- table(df$Survived)
tbl
win.graph();barplot(tbl, beside=TRUE, legend=TRUE, col=rainbow(2))

# 언더샘플링
library(ROSE)
# method: under,over,both N: 샘플링 후의 샘플 갯수(적은 쪽 x2) 또는 p=0.5 50:50으로 선택
df_samp <- ovun.sample(Survived ~ . ,data = df,
  seed=1, method= "under", N=342*2)$data
tbl<-table(df_samp$Survived)
tbl

library(caret)
set.seed(3)
idx_train <- createDataPartition(y=df_samp$Survived, 
  p=0.8, list=FALSE)
#학습용
train <- df_samp[idx_train, ]
X_train <- train[, -1]
y_train <- train[, 1]
#검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]
head(X_train)
head(y_train)

# 로지스틱 회귀모델 생성
model <- glm(Survived ~ ., data=train)

# 모델 정보 요약
summary(model)

# 회귀 계수 확인
(coef1 <- coef(model))

# 예측값을 0~1 사이로 설정
pred <- predict(model, newdata=X_test, type='response')
# 0.5보다 크면 1, 아니면 0으로 설정
result <- ifelse(pred>0.5,1,0)

# 예측정확도
mean(y_test == result)

# 오분류표 출력
table(y_test, result)

#OR(odds ratio, 오즈비=승산비=교차비)
#독립변수의 회귀계수를 exp(회귀계수)로 변환한 값
# P/(1-P) y=0이 일어날 확률 대비 y=1 이 일어날 확률의 비
# 1보다 크면 성공 확률 > 실패 확률
# 1보다 작으면 성공 확률 < 실패 확률
# 1이면 p=0.5
# 3이면 성공확률이 실패확률보다 3배 높다
exp(coef1) #지수함수
# Pclass1 : 1.36 1등석 탑승자는 아닌 사람보다 1.36배 생존 확률이 높음