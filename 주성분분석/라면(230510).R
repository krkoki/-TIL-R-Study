data <- read.table("d:/workspace_kitae/R2/data/noodle/noodle.txt", header=T)
data

summary(data)

cor(data)

p1 <- prcomp(data, scale=T) # scale=T 데이터 표준화 처리 포함
p1

win.graph(); plot(p1, type="l")

summary(p1)

predict(p1)

win.graph(); biplot(p1)
