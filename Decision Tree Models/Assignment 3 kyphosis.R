#Installing rpart package#
install.packages("rpart",dependencies = TRUE)
library(rpart)

#####Analysis of Kyphosis Dataset#####
data(kyphosis)
kyphosis
str(kyphosis)

#Creating a rpart model#
k <- rpart(Kyphosis~.,data=kyphosis,method="class",parms = list(split = 'information'), minsplit=2)

#Printing the decision tree plot#
par(mar=rep(0.1,4))
plot(k,uniform=T,compress = T)
text(k)

#Listing the important attributes#
k$variable.importance

#Pruning the tree#
printcp(k)
pruneTree <- prune(k, cp=(k$cptable[which.min(k$cptable[, "xerror"]), "CP"]))
par(mar=rep(0.1,4))
plot(pruneTree,uniform=T,compress = T)
text(pruneTree)

#Dividing the dataset into 2 parts of 80%-20% #
train <- sample(nrow(kyphosis),size = 64)
trainingData <- kyphosis[train,]
testData <- kyphosis[-train,]
nrow(trainingData)
nrow(testData)

trainModel <-rpart(Kyphosis~.,data=trainingData,method="class",parms = list(split = 'information'), minsplit=2)
par(mar=rep(0.1,4))
plot(trainModel,uniform=T,compress = T)
text(trainModel)

#Creating pruned tree of training data#
printcp(trainModel)
trainModel.cp <- trainModel$cptable[which.min(trainModel$cptable[, "xerror"]), "CP"]
pruneTrainModel<- prune(trainModel, trainModel.cp)
par(mar=rep(0.1,4))
plot(pruneTrainModel,uniform=T,compress = T)
text(pruneTrainModel)

#Predicting test data values by using the pruned training dataset$
prediction <- predict(pruneTrainModel,testData,type = "class")
prediction
predTable <- table(prediction,testData$Kyphosis)
predTable

#Calculating accuracy#
accuracy <- sum(diag(predTable))/sum(predTable)
accuracy*100

#Dividing the dataset into 2 parts of 90%-10% #
train2 <- sample(nrow(kyphosis),size = 72)
trainingData2 <- kyphosis[train2,]
testData2 <- kyphosis[-train2,]
nrow(trainingData2)
nrow(testData2)

trainModel2 <-rpart(Kyphosis~.,data=trainingData,method="class",parms = list(split = 'information'), minsplit=2)
par(mar=rep(0.1,4))
plot(trainModel2,uniform=T,compress = T)
text(trainModel2)

#Creating pruned tree of training data#
printcp(trainModel2)
trainModel2.cp <- trainModel2$cptable[which.min(trainModel2$cptable[, "xerror"]), "CP"]
pruneTrainModel2<- prune(trainModel2, trainModel2.cp)
par(mar=rep(0.1,4))
plot(pruneTrainModel2,uniform=T,compress = T)
text(pruneTrainModel2)

#Predicting test data values by using the pruned training dataset$
prediction2 <- predict(pruneTrainModel2,testData2,type = "class")
prediction2
predTable2 <- table(prediction2,testData2$Kyphosis)
predTable2

#Calculating accuracy#
accuracy2 <- sum(diag(predTable2))/sum(predTable2)
accuracy2*100