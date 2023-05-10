# deepnet 패키지 : 은닉층을 2개 이상 만들수 있음
# install.packages('deepnet')
library(deepnet)

# and 문제
input <- matrix(c(0, 0, 1, 1,
                  0, 1, 0, 1),ncol=2) # 4행 2열의 행렬
input
output <- matrix(c(0, 0, 0, 1), ncol=1) # 4행 1열의 행렬

nn <- nn.train(input, output, hidden=c(2))
# nn 신경망 모형, hidden=c(2, 2) 레이어 2개, 노드 2개
nn.predict(nn, input) # 학습이 부족함

# 학습횟수 증가
nn <- nn.train(input, output, hidden=c(2), numepochs=100)
nn.predict(nn, input) # 학습횟수를 늘려서 예측력이 향상됨

# 학습률 증가
nn <- nn.train(input, output, hidden=c(2), learningrate=10, numepochs=10000)
nn.predict(nn, input)

#############################################################################
# or 문제
input<-matrix(c(0,0,1,1,  
                0,1,0,1),ncol=2)
input
output <- matrix(c(0,1,1,1), ncol=1)
nn <- nn.train(input, output, hidden=c(2), learningrate=10, numepochs=1000)
nn.predict(nn, input)

#############################################################################
#xor 문제
input<-matrix(c(0,0,1,1,
                0,1,0,1),ncol=2)
input
output <- matrix(c(0,1,1,0), ncol=1)
nn <- nn.train(input, output, hidden=c(2), learningrate=10, numepochs=10000)
nn.predict(nn, input)
