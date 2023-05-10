# install.packages('RCurl')
# install.packages('h2o')
library(h2o)
localH2O = h2o.init() 
# localH2O = h2o.init(ip="localhost", port = 54321, nthreads=-1)
# http://localhost:54321 접속

train <- h2o.importFile("./data/iris/iris.csv")
test <- h2o.importFile("./data/iris/iris.csv")
X <- names(train)[1:4]
y <- names(train)[6]

train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[,y])

iris_h2o <- as.h2o(iris, destination_frame = "iris_h2o")
h2o.ls()
class(iris_h2o)
head(iris_h2o)

n_rows <- nrow(iris_h2o) # 샘플수
n_cols <- ncol(iris_h2o) # 변수 개수
paste("행의 개수 : ", n_rows)

set.seed(123)
train_idx <- sample(1:nrow(iris), size=0.8 * nrow(iris), replace=F)
# sample(배열, 개수, 중복여부)
train_iris <- iris[train_idx, ]
test_iris <- iris[-train_idx, ]

# 품종별 비율
with(train_iris, prop.table(table(Species)))
with(test_iris, prop.table(table(Species)))

# h2o 타입으로 변환
train_iris_h2o <- as.h2o(train_iris, "train_iris_h2o")
test_iris_h2o <- as.h2o(test_iris, "test_iris_h2o")
target <-"Species"

# 독립변수들의 이름
features <- names(train_iris)[!names(train_iris) %in% target]
features

# 로지스틱 회귀분석
glm_model <- h2o.glm(x = features, y = target, training_frame = train_iris_h2o, model_id = "glm_model", family ="multinomial")
summary(glm_model)
pred_iris_glm <- as.data.frame(h2o.predict(glm_model, newdata = test_iris_h2o))

# 예측값
test_iris$pred_glm <- pred_iris_glm$predict

# 오분류표, dnn(Dimension Names)
# with - 데이터프레임의 필드명으로 직접 접근할 수 있는 함수
with(test_iris, table(Species, pred_glm, dnn = c("Real", "Predict")))

#랜덤 포레스트 모형
rf_model <- h2o.randomForest(x = features, y = target,
training_frame = train_iris_h2o, model_id = "rf_model", ntrees = 100)
pred_iris_rf <- as.data.frame(h2o.predict(rf_model, newdata = test_iris_h2o))
summary(rf_model)

# 예측값
test_iris$pred_rf <- pred_iris_rf$predict

# 오분류표
with(test_iris, table(Species, pred_rf, dnn = c("Real", "Predict")))