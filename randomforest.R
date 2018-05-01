library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(adabag)
library(dplyr)


setwd("C:/R/Machine_Learning/Machine_Learning_Project_Sutda")
test <- readxl::read_excel(path = "masterdata.xlsx",
                           col_names = TRUE,
                           sheet = 1)
test <- as.data.frame(test)
# remove.variables <- c("p2_num2_m", "p2_num2_s", "p3_num2_m", "p3_num2_s", "p1rank", 
#                       "p2rank", "p3rank", "p1jokbo", "p2jokbo", "p3jokbo")
# 
# 
# # 변수 삭제하기
# data <- a %>% 
#   select(-one_of(remove.variables))

# 두번째, 세번째 플레이어 카드 모르니까 빼기
drops <- c("p2_num2_m", "p2_num2_s", "p3_num2_m", "p3_num2_s")
test <- test[ , !(names(test) %in% drops)]
# 중간검정... View(test)
# win or lose 를 1 or 0로...
test$win.lose <- ifelse(test$win.lose == "win",1,0)
test$win.lose <- as.factor(test$win.lose)
test$win.lose <- factor(test$win.lose, labels = c("lose", "win"))
#summary(test)
str(test)
# conversion...
test$p1_num1_s <- ifelse(test$p1_num1_s == 1,0.5,0)
test$p1_num2_s <- ifelse(test$p1_num2_s == 1,0.5,0)
test$p2_num1_s <- ifelse(test$p2_num1_s == 1,0.5,0)
test$p3_num1_s <- ifelse(test$p3_num1_s == 1,0.5,0)
# 중간 정검... View(test)
# new data frame
test$player1card1 <- test$p1_num1_m + test$p1_num1_s
test$player1card2 <- test$p1_num2_m + test$p1_num2_s
test$player2card1 <- test$p2_num1_m + test$p2_num1_s
test$player3card1 <- test$p3_num1_m + test$p3_num1_s
# # 중간 정검... View(test)
# colnames(test$player1card1) <- c("player1card1")
# colnames(test$player1card2) <- "player1card2"
# colnames(test$player2card1) <- "player2card1"
# colnames(test$player3card1) <- "player3card1"

# data frame 따로 추출

DF <- data.frame(test$player1card1, test$player1card2, test$player2card1,
                 test$player3card1, test$win.lose)
head(DF)
str(DF)

# test data 추출
index.train <- createDataPartition(DF$test.win.lose, p=0.7, list = FALSE)
jokbo.train <- DF[index.train,]
jokbo.test <- DF[-index.train,]



# Bagging
#mfinal = 100으로 돌려보기
train.bagging <- bagging(test.win.lose ~ ., data = jokbo.train, mfinal= 100)
summary(train.bagging)  # kyphosis.bagging를 그대로 보면 너무 많음


# 각 분류나무 plot
rpart.plot(train.bagging$trees[[1]], cex = 0.8)
rpart.plot(train.bagging$trees[[2]], cex = 0.8)
# 첫 번째 값: 분류 예측
# 두 번째 값: 미리 정한 기준값으로 분류한 비율 
# 세 번째 값: 해당 관측값들의 비중


# 변수별 중요도
train.bagging$importance
importanceplot(train.bagging)


# 예측
SD.bagging.predict <- predict.bagging(train.bagging, newdata=jokbo.test)
SD.bagging.predict

# 혼동행렬
SD.bagging.predict$confusion
# accuracy : 0.74


library(randomForest)

## default
# ntree: 나무의 개수, mtry: 사용할 변수의 개수(default = sqrt(변수개수)) = 2
SD.rf.n <- randomForest(test.win.lose ~ ., data = jokbo.train, ntree = 100,
                        importance = TRUE)
SD.rf.n

varImpPlot(SD.rf.n)

# mtry = 2일때 (default와 같음)
SD.rf2.n <- randomForest(test.win.lose ~ ., data = jokbo.train, mtry=2, ntree=100, importance=TRUE)
SD.rf2.n

varImpPlot(SD.rf2.n)


### mtry = 3일때 
SD.rf3.n <- randomForest(test.win.lose ~ ., data = jokbo.train, mtry=3, ntree=100, importance=TRUE)
SD.rf3.n

varImpPlot(SD.rf3.n)


###
SD.rf4.n <- randomForest(test.win.lose ~ ., data = jokbo.train, mtry=4,
                         ntree=100, importance=TRUE)
SD.rf4.n

varImpPlot(SD.rf4.n)

##
pred.rf.n <- predict(SD.rf.n, jokbo.test)
confusionMatrix(pred.rf.n, jokbo.test$test.win.lose)

pred.rf2.n <- predict(SD.rf2.n, jokbo.test)
confusionMatrix(pred.rf.n, jokbo.test$test.win.lose)

pred.rf3.n <- predict(SD.rf3.n, jokbo.test)
confusionMatrix(pred.rf.n, jokbo.test$test.win.lose)

pred.rf4.n <- predict(SD.rf4.n, jokbo.test)
confusionMatrix(pred.rf.n, jokbo.test$test.win.lose)



#모든 변수를 팩터로 변환
str(jokbo.train)
for (i in 1:4){
  jokbo.train[ ,i + 5] = as.factor(jokbo.train[ ,i])
  jokbo.test[ ,i + 5] = as.factor(jokbo.test[ ,i])
}
str(jokbo.train)




## default
SD.rf.f <- randomForest(test.win.lose ~ V6 + V7 + V8 + V9, data = jokbo.train, ntree=100, importance=TRUE)
SD.rf.f

varImpPlot(SD.rf.f)

SD.rf1.f <- randomForest(test.win.lose ~ V6 + V7 + V8 + V9, data = jokbo.train, mtry =1, ntree=100, importance=TRUE)
SD.rf1.f

SD.rf2.f <- randomForest(test.win.lose ~ V6 + V7 + V8 + V9, data = jokbo.train, mtry =2, ntree=100, importance=TRUE)
SD.rf2.f

SD.rf3.f <- randomForest(test.win.lose ~ V6 + V7 + V8 + V9, data = jokbo.train, mtry =3, ntree=100, importance=TRUE)
SD.rf3.f

SD.rf4.f <- randomForest(test.win.lose ~ V6 + V7 + V8 + V9, data = jokbo.train, mtry =4, ntree=100, importance=TRUE)
SD.rf4.f


varImpPlot(SD.rf4)

##
pred.rf.f <- predict(SD.rf.f, jokbo.test)
confusionMatrix(pred.rf.f, jokbo.test$test.win.lose)

pred.rf1.f <- predict(SD.rf1.f, jokbo.test)
confusionMatrix(pred.rf1.f, jokbo.test$test.win.lose)


pred.rf2.f <- predict(SD.rf2.f, jokbo.test)
confusionMatrix(pred.rf2.f, jokbo.test$test.win.lose)

pred.rf3.f <- predict(SD.rf3.f, jokbo.test)
confusionMatrix(pred.rf3.f, jokbo.test$test.win.lose)

pred.rf4.f <- predict(SD.rf4.f, jokbo.test)
confusionMatrix(pred.rf4.f, jokbo.test$test.win.lose)






### ----

SD.rf1.nf <- randomForest(test.win.lose ~., data = jokbo.train, mtry =1, ntree=100, importance=TRUE)
SD.rf1.nf

SD.rf2.nf <- randomForest(test.win.lose ~., data = jokbo.train, mtry =2, ntree=100, importance=TRUE)
SD.rf2.nf

SD.rf3.nf <- randomForest(test.win.lose ~., data = jokbo.train, mtry =3, ntree=100, importance=TRUE)
SD.rf3.nf

SD.rf4.nf <- randomForest(test.win.lose ~., data = jokbo.train, mtry =3, ntree=100, importance=TRUE)
SD.rf4.nf


varImpPlot(SD.rf4)

##
pred.rf1.nf <- predict(SD.rf1.nf, jokbo.test)
confusionMatrix(pred.rf1.nf, jokbo.test$test.win.lose)


pred.rf2.nf <- predict(SD.rf2.nf, jokbo.test)
confusionMatrix(pred.rf2.nf, jokbo.test$test.win.lose)

pred.rf3.nf <- predict(SD.rf3.nf, jokbo.test)
confusionMatrix(pred.rf3.nf, jokbo.test$test.win.lose)

pred.rf4.nf <- predict(SD.rf4.nf, jokbo.test)
confusionMatrix(pred.rf4.nf, jokbo.test$test.win.lose)

