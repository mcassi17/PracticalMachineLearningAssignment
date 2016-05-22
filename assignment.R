library(caret)
library(rpart)
library(randomForest)
trainingData <- read.csv("./pml-training.csv", na.strings=c("", "NA", "NULL"))
testingData <- read.csv("./pml-testing.csv", na.strings=c("", "NA", "NULL"))
dim(trainingData)
str(trainingData)
summary(trainingData)
with(trainingData, table(user_name, classe))
set.seed(1726)
smallerTraining <- trainingData[ , colSums(is.na(trainingData)) == 0]
smallestTraining <- subset(smallerTraining, select = -c(X, user_name, new_window, 
                        num_window, raw_timestamp_part_1, 
                        raw_timestamp_part_2, cvtd_timestamp))
dim(smallestTraining)
inTrain <- createDataPartition(y=smallestTraining$classe, p=0.75, list=FALSE)
dataTrain <- smallestTraining[inTrain,]
dataTest <- smallestTraining[-inTrain,]
treeModel1 <- train(classe ~ ., data = dataTrain, method="rpart")
pred1 <- predict(treeModel1, dataTest)
table(pred1, dataTest$classe)
confusionMatrix(pred1, dataTest$classe)
rfModel1 <- train(classe ~ ., data = dataTrain, method="rf")
pred2 <- predict(rfModel1, dataTest)
table(pred2, dataTest$classe)
confusionMatrix(pred2, dataTest$classe)
pred4 <- predict(rfModel1, testingData)