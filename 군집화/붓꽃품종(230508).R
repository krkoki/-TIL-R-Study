df <- read.csv('./data/iris/iris.csv')
head(df)
tail(df)

library(dplyr)
# 필드 제거
df <- df %>% select(-Name)
# Species 변수가 int로 되어 있는데 factor 타입으로 변경
str(df)
df$Species <- as.factor(df$Species)
str(df)
head(df)
dim(df)
summary(df)

# 상관계수 행렬
(corrmatrix <- cor(df[1:4]))
library(corrplot)  
win.graph(); corrplot(cor(df[1:4]), method="circle")
library(reshape)
meltData <- melt(df[, -5])
win.graph(); boxplot(data=meltData, value~variable)

# 데이터셋에서 특성별 최소값 계산
min_on_train = min(df[, -5])

# 데이터셋에서 특성별 (최대값 - 최소값) 범위 계산
range_on_train = max(df[, -5] - min_on_train)

# 데이터셋에서 최소값을 빼고 범위로 나누면 스탠다드스케일러
# 각 특성에 대해 최소값은 0, 최대값을 1로 조정됨
X = (df[, -5] - min_on_train) / range_on_train
df_scaled = cbind(X, Species = df[, 5])
head(df_scaled)
meltData <- melt(df_scaled[, -5])
win.graph(); boxplot(data=meltData, value~variable)

# 1. 최적의 군집수(k)를 설정하는 방법(엘보우 포인트)
result <- NULL
for (k in 1:10){
  result[[k]] <- kmeans(df_scaled[, -5], k)
}

# tot.withinss : The total within-cluster sum of square
# 클러스터 내 총 제곱합(작을수록 군집화가 잘 된것)
wss <- numeric(10)
for (k in 1:10){
  wss[k] <- result[[k]]$tot.withinss
}
win.graph(); plot(wss, type="l")

# 엘보우 포인트 : 2 또는 3
# 2. 최적의 군집수(k)를 설정하는 방법(실루엣 포인트)
library("cluster")
avgsil<-numeric(10)
for (k in 2:10){
  si<-summary(silhouette(result[[k]]$cluster,dist(df_scaled[,-5])))
  avgsil[k]<-si$avg.width
}
max(avgsil)
win.graph(); plot(avgsil,type="l")

# 실루엣 포인트가 최대가 되는 값 : k=2
# factoextra 패키지를 이용하여 적절한 클러스터 개수를 결정하고 시각화
# install.packages("factoextra")
library(factoextra)
fviz_nbclust(df_scaled[,-5], FUN=kmeans, method = "wss")
fviz_nbclust(df_scaled[,-5], FUN=kmeans, method = "silhouette")

# 산점도 행렬(클러스터가 3개일 경우)
win.graph(); plot(df_scaled[,-5], pch=result[[3]]$cluster,
col=result[[3]]$cluster)

set.seed(123)
# centers=군집수, df[,-5] 답은 제외하고 입력
model <- kmeans(df[,-5], centers = 3)
model
model$centers #중심좌표

# 군집 분석 결과를 데이터프레임에 추가
# integer를 factor로 변환
df$cluster <- as.factor(model$cluster - 1)
# 실제값과 클러스터링 결과를 확인해 보면 군집화는 대체로 잘 되었으나
# 0,1,2가 0,2,1로 처리됨
# 답을 미리 알려주지 않는 비지도학습이기에 label이 중요하지 않음
# 0,1,2를 2,1,0으로 변경하는 함수
df
convert <- function(i){
  if( i == 0){
    return("0")
  }else if (i==1){
    return("2")
  }else if (i==2){
    return("1")
  }
}

# 각 샘플에 convert() 함수를 적용
result<-sapply(df$cluster, convert)
result

#오분류표
table(df$Species, result)

#예측정확도
mean(result == df$Species)
