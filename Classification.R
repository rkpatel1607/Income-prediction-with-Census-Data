library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
set.seed(1000)
train$income<-as.factor(as.character(train$income))
inTrain<- createDataPartition(train$income,p=0.7,list = FALSE)
training<- train[inTrain,]
testing <- train[-inTrain,]
dim(training)
dim(testing)
#Decision Tree
treeFit<- rpart(income~.,data=training,method = 'class')
rpart.plot(treeFit, box.col=c("red", "blue"))
Prediction_Desision_Tree<- predict(treeFit,newdata=testing,type = 'class')
table(Prediction_Desision_Tree, testing$income)
TreeAcu<-confusionMatrix(Prediction_Desision_Tree,testing$income)$overall[1]


# Logistic Regression
# we first Change income to 0, 1
training$income <- as.numeric(training$income)-1
testing$income <- as.numeric(testing$income)-1
lg<-glm(income ~.,family=binomial,data=training)
Prediction2<- predict(lg,newdata=testing,type = 'response')
Pred<- ifelse(Prediction2>0.5,1,0)
lgAcu<-confusionMatrix(Pred,testing$income)$overall[1]
lgAcu
table(Pred, testing$income)
#Re-factoring income
training$income <- factor(training$income, labels=c("<=50k", ">50k"))
testing$income <- factor(testing$income, labels=c("<=50k", ">50k"))

#Random Forest
set.seed(32423)
rfFit<- randomForest(income~.,data= training)
print(rfFit)
Prediction3<- predict(rfFit,newdata = testing,type = 'class')
rfAcu<-confusionMatrix(Prediction3,testing$income)$overall[1]
rfAcu
table(Prediction3, testing$income)
# We evaluate the models by a comparision between accuracy 
Accuracy<-data.frame(Model=c('Decision Tree','Logistic Regression','Random Forest'),Accuracy=c(TreeAcu,lgAcu,rfAcu))
gg<-ggplot(Accuracy,aes(x=Model,y=Accuracy,fill=Model))+geom_bar(stat = 'identity')+theme_bw()+ggtitle('Accuracies of Models')
gg

# we finally Apply the model with best performances on the test set, for evaluation
# we choose to use the Logistic Regression as the most accurate
# we first Change income to 0, 1
test$income <- as.numeric(test$income)-1
PredictionTest<- predict(lg,newdata=test,type = 'response')
PredTest<- ifelse(PredictionTest>0.5,1,0)
lgAcuTest<-confusionMatrix(PredTest,test$income)$overall[1]
# Display confusion matrix to get prediction accuracy
table(PredTest, test$income)
#Re-factoring income
test$income <- factor(test$income, labels=c("<=50k", ">50k"))
# we Evaluate then Decision Tree and Random Forest 
#Decision Tree
Prediction_Desision_Tree_Test<- predict(treeFit,newdata=test,type = 'class')
table(Prediction_Desision_Tree_Test, test$income)
TreeAcuTest<-confusionMatrix(Prediction_Desision_Tree_Test,test$income)$overall[1]
#Random Forest
PredictionTest<- predict(rfFit,newdata = test,type = 'class')
rfAcu<-confusionMatrix(PredictionTest,test$income)$overall[1]
table(PredictionTest, test$income)
