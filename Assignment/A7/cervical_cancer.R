library(dplyr) 
library(ggplot2)
library(scales)
library(caret)

cancer_Data <- read.csv(file='sobar-72.csv')

hist(cancer_Data$behavior_sexualRisk,
     main="",
     col='grey',
     border="black",
     density=c(10,20) , angle=c(45,135),breaks=30
)

hist(cancer_Data$behavior_eating,
     main="",
     col='grey',
     border="black",
     density=c(10,20) , angle=c(45,135),breaks=30
)

hist(cancer_Data$behavior_personalHygine,
     main="",
     col='grey',
     border="black",
     density=c(10,20) , angle=c(45,135),breaks=30
)


#Select the most important features

names()
colnames(cancer_Data)[20] <- c('y')

cancer_corr <- cor(cancer_Data, use="complete.obs", method="pearson")
cancer_corr <- cancer_corr[,'y']
cancer_corr <- data.frame(lapply(cancer_corr,abs))
cancer_corr <- data.frame(t(cancer_corr))
names(cancer_corr) <- 'corr'

cancer_corr <- cbind(feature_name = rownames(cancer_corr), cancer_corr)
rownames(cancer_corr) <- 1:nrow(cancer_corr)
cancer_corr <- cancer_corr[order(-cancer_corr$corr),]
#cancer_corr <- cancer_corr[-1,]
rownames(cancer_corr) <- 1:nrow(cancer_corr)

selected_feature <- cancer_corr[1:10,'feature_name']
selected_feature

#7:3 train test split
set.seed(0)
n = nrow(cancer_Data)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
training_data = cancer_Data[trainIndex,selected_feature]
testing_data = cancer_Data[-trainIndex,selected_feature]

##---------------------Models---------------------------------
#Logistic Regression: Benchmark Feature

acc_list = c()
for (i in c(2:14)){
  selected_feature <- cancer_corr[1:i,'feature_name']
  training_data = cancer_Data[trainIndex,selected_feature]
  testing_data = cancer_Data[-trainIndex,selected_feature]
  print(i)
  logit_model <- glm(y ~ 0+., data = training_data,family=binomial(link='logit'))
  logit_pred <- logit_model %>% predict(testing_data, type = "response")
  logit_pred <- ifelse(logit_pred > 0.5, 1, 0)
  print('Accuracy:')
  print(mean(logit_pred == testing_data$y))
  acc_list <- c(acc_list,mean(logit_pred == testing_data$y))
}
acc_list 

acc_table <- data.frame('num_of_feature' = (1:13),'acc' = acc_list)

acc_table %>%
tail(15) %>%
ggplot( aes(x=num_of_feature, y=acc)) +
geom_line( color="black") +
geom_point(shape=21, color="pink", fill="pink", size=6) +
ggtitle("Logistic Reg Acc vs. Number of Features") +
  scale_x_continuous(breaks = c(1:13))

#Select 4 most correlated features
selected_feature <- cancer_corr[1:5,'feature_name']
training_data = cancer_Data[trainIndex,selected_feature]
testing_data = cancer_Data[-trainIndex,selected_feature]

     
logit_model <- glm(y ~ 0+., data = training_data,family=binomial(link='logit'))
summary(logit_model)

logit_pred <- logit_model %>% predict(testing_data, type = "response")
logit_pred <- ifelse(logit_pred > 0.5, 1, 0)
# Model accuracy
mean(logit_pred == testing_data$y)

#KNN
training_data$y <- ifelse(training_data$y == 1,'yes','no')
testing_data$y <- ifelse(testing_data$y == 1,'yes','no')
#training_data$y <- as.factor(training_data$y)
knnGrid <-  expand.grid(k = c(1,3,5,7,9,11,13,15,17,19,21))
names(training_data)

training_data
set.seed(0)
ctrl <- trainControl(method="cv",number = 3,classProbs = TRUE,summaryFunction = twoClassSummary)
knn_out <- train(y ~ ., data = training_data, method = "knn", trControl = ctrl, 
                 preProcess = c("center","scale"), tuneGrid = knnGrid,metric = ('ROC'))
plot(knn_out)
knn_out

#Choose k = 15 for final KNN model
ctrl <- trainControl(method="cv",number = 10)#classProbs = TRUE,summaryFunction = twoClassSummary)
knn_15 <- train(y ~ ., data = training_data, method = "knn", trControl = ctrl, 
                preProcess = c("center","scale"), tuneGrid = expand.grid(k = c(15)))
knn_15_pred <- predict(knn_15,testing_data)
knn_15_train_acc <- as.numeric(unlist(knn_15["results"])['results.Accuracy'])
knn_15_test_acc <- mean(ifelse(knn_15_pred == testing_data$y,1,0))

#In-sample Accuracy: 0.7783333
#Out-of-sample Accuracy: 0.6818182
knn_15_train_acc
knn_15_test_acc

testing_data$y <- as.factor(testing_data$y)
knn_15_pred <- as.factor(knn_15_pred)
confusionMatrix(data = knn_15_pred, reference = testing_data$y)


#Random Forest
library(randomForest)
library(rpart)
library(rpart.plot)
training_data$y <- as.factor(training_data$y)
forest_out <- randomForest(y ~., data= training_data,ntree=100,replace=TRUE)
#cm for RF
print(forest_out)
#out of sample acc
rf_pred <- forest_out %>% predict(testing_data, type = "response")
rf_pred
#feature importance
rf_feature_importance <- varImp(forest_out)
rf_feature_importance <- cbind(feature_name = rownames(rf_feature_importance), rf_feature_importance)
rownames(rf_feature_importance) <- 1:nrow(rf_feature_importance)
rf_feature_importance
plot(varImp(forest_out))
plot(forest_out, log="y")
rf_feature_importance

?barplot
barplot(rf_feature_importance$Overall,
        names = rf_feature_importance$feature_name,
        main="Feature Importance",
        col='grey',
        border="black",ylim = c(0,6),
        density=c(20,20,20,20) , angle=c(0,45,0,45)
)


rf_acc <- mean(ifelse(rf_pred == testing_data$y,1,0))
rf_acc

#Decision Tree- Demonstration purposes
dtree_out <- rpart(y ~., data= training_data,control=rpart.control(cp=0.003),method = 'class')
dtree_out
rpart.plot(dtree_out,type=5)
?rpart.control