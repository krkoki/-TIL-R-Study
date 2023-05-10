df <- read.csv("../Python2/data/ozone/ozone2.csv")
head(df)
tail(df)

library(dplyr)
# 필드 제거
df<-df %>% select(-Ozone, -Month, -Day) # + 변수추가, - 변수제외

# 상관계수 행렬
(corrmatrix <- cor(df))

# 강한 양의 상관관계, 강한 음의 상관관계
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5]

library(corrplot)
win.graph(); corrplot(cor(df), method="circle")

# 불균형 데이터셋
tbl <- table(df$Result)
tbl
win.graph() ;barplot(tbl, beside = TRUE, legend = TRUE, col = rainbow(2))

# under sampling
# install.packages("ROSE")
library(ROSE)
# method : under, over, both
# N : 샘플링 후의 샘플 갯수(적은 쪽 x2) 또는 p=0.5 50:50 선택
df_samp <- ovun.sample(Result ~ . ,data = df, 
  seed = 1, method = "under", N = 144)$data
tbl <- table(df_samp$Result)
tbl


library(caret)
set.seed(123) # 랜덤 시드 고정
# 학습용 8 : 검증용 2로 구분
idx_train <- createDataPartition(y = df_samp$Result, 
  p = 0.8, list = FALSE) # list = FALSE, 인덱스값들의 리스트를 반환하지 않음

# 학습용
train <- df_samp[idx_train, ]
X_train <- train[, -1] # [행범위, 열범위] [, -1] 모든행, 1번 제외 나머지 다
y_train <- train[, 1] # [모든행, 1번열만] R은 인덱스가 1번부터 시작한다.

# 검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -1]
y_test <- test[, 1]

# 로지스틱 회귀모델 생성
model <- glm(Result ~., data=train)

# 모델정보 요약
summary(model)

# 회귀계수 확인
(coef1 <- coef(model))

# 일조량, 온도 : 양의 상관관계
# 풍량 : 음의 상관관계

# 예측값을 0~1 사이로 설정
pred <- predict(model, newdata = X_test)
pred

# 0.5 보다 크면 1, 작으면 0으로 설정
(result <- round(pred))
(result <- ifelse(pred > 0.5, 1, 0))

# 예측정확도
mean(y_test == result)

# 오분류표 출력
table(y_test, result)

# install.packages("ROCR")
library(ROCR)
pr <- prediction(pred, y_test)
pr@predictions # 출력값
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
win.graph(); plot(prf, main="ROC  Curve")

# AUC (The Area Under an ROC Curve)
auc <- performance(pr, measure = "auc")
auc@y.values
auc@y.values[[1]]
