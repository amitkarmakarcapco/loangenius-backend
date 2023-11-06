library(rpart)
library(rpart.plot)

library(tidyverse) # tidyverse for easy data manipulation and visualization'
library(caret) # caret for easy machine learning workflow'
library(party)
library(caret)
library(InformationValue)
library(ISLR)
library("corrplot")


library(pROC)

train.lp= read.csv(file.choose())
test.lp= read.csv(file.choose())

summary(train.lp)

# choosing required columns  


trainLP = train.lp[,c(3,5,6,10,12)]
#testLP = test.lp[,c(3,5,6,10,12)]



summary (trainLP)
summary (testLP)

trainLP$Loan.Status= as.factor(trainLP$Loan.Status)
#trainLP$relation_type= as.factor(trainLP$relation_type)
#trainLP$cust_segment= as.factor(trainLP$cust_segment)
#trainLP$Current.Loan.Amount= as.factor(trainLP$Current.Loan.Amount)
#trainLP$Credit.Score= as.factor(trainLP$Credit.Score)
#trainLP$creditbuc= as.factor(trainLP$creditbuc)
#trainLP$Hbucket= as.factor(trainLP$Hbucket)
#trainLP$Monthly.Debt= as.factor(trainLP$Monthly.Debt)


testLP$Loan.Status= as.factor(testLP$Loan.Status)
#testLP$Loan_type= as.factor(testLP$Loan_type)

trainLP$Loan.Status = ifelse(trainLP$Loan.Status=="Fully Paid",1,0)
testLP$Loan.Status = ifelse(testLP$Loan.Status=="Fully Paid",1,0)


LoanModelLP = rpart(Loan.Status~., method = "class", data = trainLP)
rpart.plot(LoanModelLP, # middle graph
           extra = 104, # show fitted class, probs, percentages
           box.palette = "RdGy", # color scheme
           branch.lty = 3, # dotted branch lines
           shadow.col = "gray", # shadows under the node boxes
           nn = TRUE) # display the node numbers

rpart.rules(LoanModelLP)
#TRAIN DATA
predictedtrain <- predict(LoanModelLP, trainLP, type="class")
confusionMatrix(factor(trainLP$Loan.Status),factor(predictedtrain))


#use model to predict/Classify the test data 
predictedLP <- predict(LoanModelLP, testLP, type="class")

predictedtLP <- ifelse(predictedLP < 0.5, 0, 1)

summary(predictedLP)

#create confusion matrix
confusionMatrix(factor(testLP$Loan.Status),factor(predictedLP))




5#=================================

p1 <- predictedLP
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


