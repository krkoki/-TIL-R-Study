df <- read.csv("d:/workspace/Python2/data/rides/rides.csv")
head(df)

# 범주형 변수는 팩터 자료형으로 변환 후 스케일링 수행
df$weekend <- as.factor(df$weekend)
df$weekend

#install.packages("reshape")
library(reshape)
# melt() 필드 1개를 variable, value 로 여러 행으로 만드는 함수 (차원 변경)
meltData <- melt(df[2:7])
win.graph(); boxplot(data=meltData, value~variable)

# 평균 0, 표준편차 1로 만드는 작업
# 스케일링: 표준편차를 1로 만드는 작업
# 센터링: 평균을 0으로 만드는 작업
# 정규화된 데이터를 data.frame형태로 변경
df_scaled <- as.data.frame(scale(df[2:7])) # 스케일링과 센터링
head(df_scaled)

meltData <- melt(df_scaled)
win.graph(); boxplot(data=meltData, value~variable)

#caret 패키지(Classification And Regression Training):분류, 회귀 문제를 풀기 위한 다양한 도구 제공
#install.packages('caret')
library(caret)

df <- read.csv("d:/workspace/Python2/data/rides/rides.csv")

meltData <- melt(df[2:7])
win.graph(); boxplot(data=meltData, value~variable)

# 평균 0, 표준편차 1로 스케일링
prep <- preProcess(df[2:7], c("center", "scale"))
df_scaled2 <- predict(prep, df[2:7])
head(df_scaled2)

#range: 0~1 정규화
prep <- preProcess(df[2:7], c("range"))
df_scaled3 <- predict(prep, df[2:7])
head(df_scaled3)

meltData <- melt(df_scaled3)
win.graph(); boxplot(data=meltData, value~variable)