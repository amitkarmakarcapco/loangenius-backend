library(rpart)
library(rpart.plot)

library(tidyverse) # tidyverse for easy data manipulation and visualization'
library(caret) # caret for easy machine learning workflow'
library(party)
library(caret)
library(InformationValue)
library(ISLR)


library(pROC)

train.lp= read.csv(file.choose())
test.lp= read.csv(file.choose())



# choosing required columns  
trainLG = train.lp[,c(2,3,4,7,8,9,10,12,13,14,15,16,18,19,20)]
testLG = test.lp[,c(3,5,6,10,11,12)]

trainLG = train.lp[,c(3,5,12)]
testLG = test.lp[,c(3,5,12)]


summary(trainLG)


summary (trainLG)
summary (testLG)

trainLG$Loan.Status= as.factor(trainLG$Loan.Status)
trainLG$gender= as.factor(trainLG$)


testLG$Loan.Status= as.factor(testLG$Loan.Status)


trainLG$Loan.Status = ifelse(trainLG$Loan.Status=="Fully Paid",1,0)
testLG$Loan.Status = ifelse(testLG$Loan.Status=="Fully Paid",1,0)


##################################################

model.logis <- glm(Loan.Status~., data=trainLG, family = binomial)

summary(model.logis)

#use model to classify between good/bad  
predictedLG <- predict(model.logis, newdata=testLG, type="response")

predictedLG = ifelse(predictedLG < 0.5, 0, 1)

roc_object <- roc( testLG$Loan.Status, predictedLG)
auc(roc_object)

summary(predictedLG)

confusionMatrix(factor(testLG$Loan.Status),factor(predictedLG))

#ROC curve using test data with the logistic model

p1 <- predictedLG
#p1 <- p1[,2]
r <- multiclass.roc(testLG$Loan.Status, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("#696969", "light grey"),
         max.auc.polygon=TRUE,
         auc.polygon.col="#FA8072",
         print.thres=TRUE,
         main= '')

