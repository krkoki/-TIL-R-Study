df<-read.csv("d:/workspace/Python2/data/wine/wine_new.csv")
head(df)
tail(df)

library(dplyr)
# 필드 제거
df <- df %>% select(-quality)
head(df)
dim(df)
summary(df)

# 상관계수 행렬
(corrmatrix <- cor(df))

library(corrplot)
win.graph();corrplot(cor(df), method='circle')

# 불균형 데이터셋
tbl <- table(df$class)
tbl
win.graph();barplot(tbl, beside=TRUE, legend=TRUE, col=rainbow(2))

# 언더 샘플링
library(ROSE)
df_samp <- ovun.sample(class ~ ., data=df, seed=1, method="under", N=744*2)$data
tbl <- table(df_samp$class)
tbl
win.graph();barplot(tbl, beside=TRUE, legend=TRUE, col=rainbow(2))

library(caret)
# 랜덤시드 고정
set.seed(3)
idx_train <- createDataPartition(y=df_samp$class, p=0.8, list=FALSE)

# 학습용
train <- df_samp[idx_train, ]
X_train <- train[, -12]
y_train <- train[, 12]
# 검증용
test <- df_samp[-idx_train, ]
X_test <- test[, -12]
y_test <- test[, 12]

# 로지스틱 회귀모델 생성
model <- glm(class ~ ., data=train, family=binomial)

# 모델정보 요약
summary(model)

# 회귀계수 확인
(coef1 <- coef(model))

# 예측값을 0~1 사이로 설정
pred <- predict(model, newdata=X_test, type='response')

# 0.5 보다 크면 1, 아니면 0으로 설정
result <- ifelse(pred>0.5, 1, 0)

# 예측정확도
mean(y_test == result)

# 오분류표 출력
table(y_test, result)

# 후진제거법
reduced <- step(model, direction='backward')
# Deviance:이탈도
# AIC: Akaike 정보지수(Akaike information criterion)
# 두 개의 수치가 모두 작을수록 좋은 모형
# 첫번째 단계에서 pH 변수가 제거되었고
# Start: AIC=1196.94
#최종모형의 Step: AIC=1195.14로 감소됨
#최종 결과 확인
summary(reduced)

# 회귀계수 확인
coef(reduced)

# 예측값을 0~1 사이로 설정
pred <- predict(reduced, newdata=X_test, type='response')

# 0.5보다 크면 1, 아니면 0으로 설정
result <- ifelse(pred>0.5, 1, 0)

# 예측정확도
mean(y_test == result)

# 오분류표 출력
table(y_test, result)