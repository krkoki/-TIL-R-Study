df<-read.csv("d:/workspace/Python2/data/rides/rides.csv")
head(df)

model<-lm(overall~num.child + distance + rides + games +
wait + clean, data=df)
summary(model)

save(model, file="d:/workspace/R2/models/rides_regress.model")

rm(list=ls()) #현재 작업중인 모든 변수들을 제거

load("d:/workspace/R2/models/rides_regress.model")

ls()

summary(model)
