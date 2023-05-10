df<-read.csv("d:/workspace/Python2/data/house_regression/data.csv")
head(df)
tail(df)

library(dplyr)
# Suburb, Address, Type, Method, SellerG, Date, CouncilArea, Regionname필드 제거
df <- df %>% select(-Suburb, -Address, -Type, -Method, -SellerG, -Date, -CouncilArea, -Regionname)
dim(df)

# 결측값이 있는 행을 제거
df <- na.omit(df)
tail(df)
dim(df)
summary(df)

# 상관계수 행렬
(corrmatrix <- cor(df))

# 강한 양의 상관관계, 강한 음의 상관관계
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5]

library(corrplot)
corrplot(cor(df), method='circle')

# 다중회귀분석 모델 생성
model <- lm(Price ~ ., data=df)
model

# 분석결과 요약
summary(model)
# p-value가 0.05보다 작으므로 통계적으로 유의함
# 모델의 설명력(예측의 정확성) 0.4965

# 전진선택법과 후진제거법
# 후진제거법:기여도가 낮은 항목을 제거함으로써 의미있는 회귀식을 구성하는 과정
reduced<-step(model, direction="backward")
# 최종적으로 선택된 변수들 확인

# 최종 결과 확인
summary(reduced)
# p-value가 0.05보다 작으므로 이 회귀모델은 통계적으로 유의함.
# 모델의 설명력(신뢰도,예측정확성) : 73.4%