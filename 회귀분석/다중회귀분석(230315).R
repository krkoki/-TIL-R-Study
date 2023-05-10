data() # R에 기본적으로 포함되는 데이터셋 목록
help(attitude) # 데이터셋의 대한 도움말
head(attitude)
tail(attitude)

# 다중회귀분석 모델 생성
model <- lm(rating ~ ., data=attitude)
model

# 분석결과 요약
summary(model)
# complaints, learning이 기여도가 높은 변수
# p-value가 0.05보다 작으므로 통계적으로 유의함
# 모델의 설명력(예측의 정확성) 66%

# 기여도가 낮은 항목을 제거함으로써 의미있는 회귀식을 구성하는 과정
reduced <- step(model, direction="backward") # step 항수에서의 "backward" 후진제거법 (p-value가 높은순으로 제외)

# 최종 결과 확인
summary(reduced)
# p-value가 0.05보다 작으므로 이 회귀모델은 통계적으로 유의함.
# 모델의 설명력(신뢰도,예측정확성) : 68%