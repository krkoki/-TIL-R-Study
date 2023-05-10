#맥주,빵,콜라,기저귀,달걀,우유
x<-data.frame(beer=c(0,1,1,1,0),bread=c(1,1,0,1,1),cola=c(0,0,1,0,1), diapers=c(0,1,1,1,1),eggs=c(0,1,0,0,0),milk=c(1,0,1,1,1))
x
class(x)

# install.packages("arules")
library(arules)
# arules 에서 사용하는 transaction 타입으로 변환
trans <- as.matrix(x, "Transaction")
trans
class(trans)

rules1 <- apriori(trans, parameter=list(supp=0.2, conf=0.6, target='rules'))
rules1
# set transactions ...[6 item(s), 5 transaction(s)] done [0.00s].
# =>5개의 거래와 6개의 물품으로 구성된 데이터, 49개의 규칙
summary(rules1)

# 상위 3개의 룰
as(head(sort(rules1, by=c('confidence', 'support')), n=3),'data.frame')
# 맥주=>기저귀
# 콜라=>우유
# 콜라=>기저귀
# 발견된 연관규칙을 확인
inspect(sort(rules1))
# [5] {beer} => {diapers} 0.6 1.0000000 1.25000003
# 맥주를 산 사람은 기저귀를 산다. 3회

# install.packages("arulesViz")
library(arulesViz)
# method = "graph"
# 물품들 간의 연관성 그래프
# 원(노드) : 규칙
# 원의 크기 : support
# 원의 색상 lift : 진한 색상이면 양의 상관관계, 흐린 색상이면 음의 상관관계
# 연관성이 있는 물품끼리 진열위치에 배치하도록 제안
win.graph(); plot(rules1, method="graph")

# 특정 상품[item] 서브셋 작성과 시각화
# A->B A를 사면 B를 살 것이다
# A lhs(left-hands side), B rhs(right-hands side)
# 오른쪽 item이 diapers인 규칙만 서브셋으로 작성
# 기저귀를 살 때 같이 사는 물품들
rules2 <- subset(rules1, rhs %in% 'diapers')
rules2
inspect(rules2)
win.graph(); plot(rules2, method="graph") # 연관 네트워크 그래프

# 왼쪽 item이 beer or cola인 규칙만 서브셋으로 작성
rules3 <- subset(rules1, lhs %in% c('beer', 'cola'))
rules3  
inspect(sort(rules3, decreasing=T, by="count"))
win.graph(); plot(rules3, method='graph')

