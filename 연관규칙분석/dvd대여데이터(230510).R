# DVD 판매량 데이터
dvd <- read.csv("d:/workspace_kitae/R2/data/basket/dvdtrans.csv")
# 첫번째 열 거래 번호, 두번째 열 대여된 DVD 정보
head(dvd)
class(dvd)
str(dvd)

# 거래아이디 1번에서는 5개의 DVD가 대여되었음
dvd$Item[dvd$ID ==1]

dvd$Item
dvd$ID

# arules 패키지가 사용하는 데이터 형태로 변환하는 작업
# DVD별로 하나의 레코드로 기록된 구조를 거래 ID별로 하나의 레코드가 되도록 변환
# split(데이터, 구분자)
# id별로 합치는 작업
dvd.list <- split(dvd$Item, dvd$ID)
dvd.list

library(arules)
# arules 에서 사용하는 transaction 타입으로 변환
dvd.trans <- as(dvd.list, "transactions")
dvd.trans


# 10개의 거래(transaction)와 10개의 물품(item)으로 구성된 데이터
summary(dvd.trans) #자세한 정보
# 가장 많은 대여횟수 : Gladiator 7회
# element (itemset/transaction) length distribution:
# sizes
# 2 3 4 5
# 3 5 1 1
# => 가장 작은 2개가 3건, 5개가 1건
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# 2.00 2.25 3.00 3.00 3.00 5.00
# => dvd 수의 평균과 중위수는 3건

# y축: 거래횟수, x축: 물품
# 3개의 물품들(x축 - 2번,9번,10번)이 집중적으로 대여되었음
# 또한 3,4,5,6번은 거래 횟수가 적음
win.graph(); image(dvd.trans)

# 연관규칙의 생성
# arules 패키지가 제공하는 apriori() 함수를 이용하여 연관 규칙을 생성
# apriori() 함수는 최소 지지도에 의해 가지치기를 하는 알고리즘
dvd.rules <- apriori(dvd.trans)
# 매개변수 / 의미 / 디폴트
# support : 규칙의 최소 지지도, 기본값 0.1
# confidence : 규칙의 최소 신뢰도, 기본값 0.8
# minlen : 규칙에 포함되는 최소 물품 수, 기본값 1
# maxlen : 규칙에 포함되는 최대 물품 수, 기본값 10
# smax : 규칙의 최대 지지도, 기본값 1
# 상위 3개의 룰
as(head(sort(dvd.rules,by=c('confidence','support')),n=3),'data.frame')
# {Patriot} => {Gladiator}
# {Patriot,Sixth Sense} => {Gladiator}
# {LOTR1} => {LOTR2}
# count 기준 내림차순 정렬 상위 6개 출력
inspect(head(sort(dvd.rules, decreasing=T, by="count")))

# 연관규칙분석의 요약 정보
# set of 77 rules
# => 총 77개의 연관규칙
# rule length distribution (lhs + rhs):sizes
# 2 3 4 5
# 12 36 24 5
# 이 중 물품이 2개 관련된 연관규칙 12개, 3개 관련된 규칙 36개, 4개 관련된 규칙 24개, 5개 관련된 규칙 5개
summary(dvd.rules)

# 발견된 77개의 연관규칙을 확인
inspect(dvd.rules)
# {Harry Potter2} => {Harry Potter1} 해리포터2를 본 사람은 해리포터1도 본다, support 지지도

# 최소 지지도를 0.2로 설정한 모형(2번 이상 거래에 나타나는 연관규칙)
# 신뢰도는 0.6으로 설정
# writing ... [15 rule(s)] done [0.00s].
# =>총 15개의 연관규칙
dvd.rules <- apriori(dvd.trans, parameter = list(support = 0.2, confidence = 0.6))
summary(dvd.rules)
inspect(dvd.rules)

# 연관규칙의 시각화
# install.packages("arulesViz")
library(arulesViz)
# method = "graph"
# 물품들 간의 연관성 그래프
# Gladiator, Six Sense, Patriot가 연관 관계의 중심에 있음
# LOTR1, 2는 다른 물품과는 다르게 둘 간에만 연관 관계가 있음
# 연관성이 있는 물품끼리 진열위치에 배치하도록 제안
win.graph(); plot(dvd.rules, method = "graph")

# 특정 상품[item] 서브셋 작성과 시각화
# A->B A를 사면 B를 살 것이다
# A lhs(left-hands side), B rhs(right-hands side)
# 오른쪽 item이 Gladiator인 규칙만 서브셋으로 작성
rule1 <- subset(dvd.rules, rhs %in% 'Gladiator')
rule1
inspect(rule1)
win.graph(); plot(rule1, method="graph") # 연관 네트워크 그래프

# 왼쪽 item이 Gladiator or Patriot인 규칙만 서브셋으로 작성
rule2 <- subset(dvd.rules, lhs %in% c('Gladiator', 'Patriot'))
rule2
inspect(rule2)
win.graph(); plot(rule2, method="graph")