library(caret)
df<-read.csv("data/diabetes/data.csv")
head(df)
##########################################
library(dplyr)
# 필드 제거
df<-df %>% select(-target)
df
dim(df)
#불균형 데이터셋
tbl<-table(df$target2)
tbl
win.graph(); barplot(tbl, beside = T, legend = T, col = rainbow(2))
##########################################
# under sampling
library(ROSE)
# method: under,over,both   N: 샘플링 후의 샘플 갯수(적은 쪽 x 2) 또는 p=0.5 50:50으로 선택
df_samp <- ovun.sample(target2 ~ . ,data = df, seed=1, method = "under", N=195*2)$data
tbl<-table(df_samp$target2)
tbl
dim(df_samp)
X <- df_samp[, -11]
y <- df_samp[, 11]
##########################################
# 최적의 sigma(gamma)와 Cost
svm.Grid <- expand.grid(.sigma= c(0.0001,0.001,0.01,0.1), 
                        .C = c(0.001,0.01,0.1,1,10,100) )
model <- train(X, as.factor(y),
                 method = "svmRadial",
                 preProcess = c("center", "scale"),
                              #(평균0, 표준편차 1) = StandardScaler
                 tuneGrid = svm.Grid,
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
model
# sigma 0.1, C 10
##########################################
pred <- predict(model, X)
pred
table(y, pred)
mean(y == pred)