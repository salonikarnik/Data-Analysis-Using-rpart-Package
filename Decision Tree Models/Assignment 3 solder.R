#Installing rpart package#
install.packages("rpart",dependencies = TRUE)
library(rpart)

#####Analysis of Solder dataset#####
data(solder)
solder
str(solder)

#Creating a rpart model#
s <- rpart(skips~.,data=solder,method="anova",control = rpart.control(minsplit=2,cp = 0.0001))

#Printing the decision tree plot#
par(mar=rep(0.1,4))
plot(s,uniform=T,compress = T)
text(s,xpd=TRUE)

#Listing the important attributes#
s$variable.importance

#Pruning the tree#
printcp(s)
pruneTrees <- prune(s, cp=(s$cptable[which.min(s$cptable[, "xerror"]), "CP"]))
par(mar=rep(0.1,4))
plot(pruneTrees,uniform=T,compress = T)
text(pruneTrees)

#Dividing the dataset into 2 parts of 80%-20% #
set.seed(123)
train <- sample(nrow(solder),size =576 )
trainingData <- solder[train,]
testData <- solder[-train,]
nrow(trainingData)
nrow(testData)

trainModel <-rpart(skips~.,data=solder,method="anova",control = rpart.control(minsplit=2,cp = 0.0001))
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
prediction <- predict(pruneTrainModel,testData,type="vector")
prediction
predTable <- table(prediction,testData$skips)
predTable

#Calculating accuracy#
accuracy <- sum(diag(predTable))/sum(predTable)
accuracy*(100)

#Dividing the dataset into 2 parts of 90%-10% #
train2 <- sample(nrow(solder),size = 648)
trainingData2 <- solder[train2,]
testData2 <- solder[-train2,]
nrow(trainingData2)
nrow(testData2)

trainModel2 <-rpart(skips~.,data=solder,method="anova",control = rpart.control(minsplit=2,cp = 0.0001))
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
prediction2 <- predict(pruneTrainModel2,testData2,type = "vector")
prediction2
predTable2 <- table(prediction2,testData2$skips)
predTable2

#Calculating accuracy#
accuracy2 <- sum(diag(predTable2))/sum(predTable2)
accuracy2*100

