v1 <- c(1, 3, 6, 10, 18)

# 거리행렬
d1 <- dist(v1)
d1

# 거리행렬 모델(average 평균기준, complete 최장거리기준, single 최단거리기준, median 중앙값기준)
m1 <- hclust(d1, method='average')
m1

# 2개의 클러스터로 구분
win.graph(); plot(m1); rect.hclust(m1, k=2)

# 3개의 클러스터로 구분
win.graph(); plot(m1); rect.hclust(m1, k=3)
###################################################
df<-read.csv('./data/iris/iris.csv')
head(df)
library(dplyr)

# 필드 제거
df <- df %>% select(-Name)
df

# 그래프 출력을 위해 40개만 선택
idx <- sample(1:nrow(df), 40)
iris_samp <- df[idx, ]

# 스케일링
iris.scaled <- scale(iris_samp[, -5])

# head(dist_iris)
iris.hclust <- hclust(dist(iris.scaled))
summary(iris.hclust)

# hang 라벨을 아래쪽으로 이동시킴
win.graph(); plot(iris.hclust, hang=-1, labels=iris$Species[idx])
rect.hclust(iris.hclust, k=3)

# 클러스터링 결과 확인
# 3개의 군집으로 클러스터링한 결과
groups <- cutree(iris.hclust, k=3)
groups


###########################################################
# 밀도기반 군집화
# install.packages('fpc')
library(fpc)
iris2 <- df[-5] # 5번 필드 제외

# 밀도기반 군집화 eps 중심점과의 거리, MinPts 최소 샘플 개수
ds <- dbscan(iris2, eps=0.42, MinPts=5)

# 클러스터링 결과값과 실제 라벨과의 비교표
table(ds$cluster, iris$Species)

# 클러스터 0: 할당되지 않은 값(outlier)
win.graph(); plot(ds, iris2)
win.graph(); plot(ds, iris2[c(1,4)]) #4행 1열의 그래프만 출력
win.graph(); plot(ds, iris2[c(2,1)]) #1행 2열의 그래프만 출력

# fpc 패키지
# 0 - outlier(밀도 조건에 맞지 않는 샘플들)
win.graph(); plotcluster(iris2, ds$cluster)