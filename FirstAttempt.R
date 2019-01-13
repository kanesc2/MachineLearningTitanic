library(tidyverse)
library(mice)
library(randomForest)


initialTrain <- read_csv("Data/train.csv")
initialTest <- read_csv("Data/test.csv")

initialTest$Survived <- NA
full <- rbind(initialTrain, initialTest)

factor_cols <- c("Survived","Pclass","Sex","Embarked")
full[,names(full) %in% factor_cols] <- lapply(full[,names(full) %in% factor_cols],factor)
full$Cabin <- NULL

full$Embarked[which(is.na(full$Embarked))] <- "S"

imputed_data <- mice(full[,c(3,5,6,7,8,10,11)])
full[,c(3,5,6,7,8,10,11)] <- complete(imputed_data)

trainIndex <- 1:dim(initialTrain)[1]
train <- full[trainIndex,]
test <- full[-trainIndex,]

set.seed(7)
CVIndex <- sample(trainIndex,0.8*dim(initialTrain)[1])
realTrain <- train[CVIndex,]
realCV <- train[-CVIndex,]

model <- randomForest(data=realTrain, Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked)
predictions <- predict(model,newdata=realCV)

accuracy <- mean(real_cv_eval == realCV$Survived)

testPredict <- predict(model,newdata=test)

submission <- data.frame(cbind(test$PassengerId,testPredict))
names(submission) <- c("PassengerId","Survived")
submission$Survived <- ifelse(submission$Survived==2,1,0)
write.csv(submission,"Data/submission.csv",row.names=FALSE)

