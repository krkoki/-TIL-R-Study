df <- read.csv("d:/workspace/Python2/data/ozone/ozone.csv")
head(df)
is.na(df) # 결측값 여부 확인

is.na(df$Ozone) # 특정 필드의 결측값 확인

df[is.na(df$Ozone),] # Ozone 필드의 결측값이 있는 행

sum(is.na(df)) # 결측값 계수

sum(is.na(df$Ozone)) # 특정 필드의 결측값 계수

complete.cases(df) # 각 샘플의 모든 필드가 NA가 아닐 때 TRUE, 하나라도 있으면 FALSE

df[complete.cases(df),] # 결측값이 없는 샘플 출력

df[!complete.cases(df),] # 결측값이 있는 샘플 출력

mean(df$Ozone) # 결측값이 있어 계산이 안됨

mean(df$Ozone, na.rm = T) # 결측값을 제외하고 계산

mapply(median, df[1:2], na.rm = T) # 1~2번 필드의 중위수 계산

df2 <- na.omit(df) # 결측값을 제외
head(df2)

df3 <- df # 결측값을 0으로 대체
df3[is.na(df)] <- 0
head(df3)

df4 <- df # 특정한 필드만 0으로 대체
df4$Ozone[is.na(df4$Ozone)] <-0
head(df4)

df5 <- df # 결측값을 평균값으로 대체
m1 <- mean(df[,1], na.rm = T)
m2 <- mean(df[,2], na.rm = T)
df5[,1][is.na(df[,1])] <- m1
df5[,2][is.na(df[,2])] <- m2
head(df5)

# 결측값 시각화 패키지
#install.packages('VIM')
#install.packages('mice')
library(VIM)
library(mice)

win.graph(); md.pattern(df)
#결측값이 없는 샘플 111개
#Ozone 필드에만 결측값이 있는 샘플 35개
#Solar.R 필드에만 결측값이 있는 샘플 5개
#2개 필드에 결측값이 있는 샘플 2개

## 결측값의 개수 표시
win.graph(); aggr(df, prop = F, numbers = T)
#prop=T 백분율로 표시, prop=F 샘플개수로 표시


# 결측값의 위치를 시각적으로 표현(red: 결측값, dark: 빈도수가 높은 값)
win.graph(); matrixplot(df)