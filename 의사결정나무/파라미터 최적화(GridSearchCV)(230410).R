#pima indian 당뇨병 데이터셋
df<-read.csv("d:/workspace/Python2/data/pima/data.csv")
head(df)
dim(df)
#############################################################
#불균형 데이터셋
tbl<-table(df$outcome)
tbl
win.graph(); barplot(tbl, beside = TRUE, legend = TRUE, col = rainbow(2))
#############################################################
# under sampling
#install.packages("ROSE")
library(ROSE)
# method: under,over,both N: 샘플링 후의 샘플 개수(적은 쪽 x2) 또는 p=0.5 50:50으로 선택
df_samp <- ovun.sample(outcome ~ . ,data = df, seed=1, method= "under", N=268*2)$data
tbl<-table(df_samp$outcome)
tbl
X<-df_samp[, -9]
y<-df_samp[, 9]
#############################################################
# 랜덤 포레스트 모형
# library(randomForest)
# install.packages('superml')
library(superml)
# XGBTrainer, RFTrainer, NBTrainer
# 클래스이름$new() : 인스턴스 생성하는 문법
rf <- RFTrainer$new() # 랜덤포레스트 분류기
gst <-GridSearchCV$new(trainer = rf, 
                       parameters = list(n_estimators = c(10,50,100), 
                       max_depth = c(2,5,10)), 
                       n_folds = 3,
                       scoring = c('accuracy','auc'))
gst$fit(df_samp,'outcome')
gst$best_iteration()
# 트리(분류기)개수 100, max_depth 2, 정확도 75%, auc: 0.75