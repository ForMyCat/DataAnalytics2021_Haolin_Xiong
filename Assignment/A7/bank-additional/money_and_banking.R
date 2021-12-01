library(dplyr) 
library(ggplot2)
library(prob)
library(caret)
library(recipes)
install.packages("recipes", dependencies = TRUE)
install.packages(c("caret", "recipes", "klaR", "ipred"))
bank_Data <- read.csv(file='bank-additional-full.csv',header=T,sep=';')
bank_Data

names(bank_Data)

#Count NA Values
for (i in (names(bank_Data))){
  print(i)
  print(sum(is.na(bank_Data$i)))
}

#Plot distributions of some attributes
barplot(table(bank_Data$y),
     main="Bank Client Data",
     xlab="whether the client subsctribed to term deposit",
     col='grey',
     border="black",
     density=c(10,20) , angle=c(45,135)
)

barplot(table(bank_Data$marital),
        main="Marital Status",
        col='grey',
        border="black",
        density=c(10,20) , angle=c(45,135)
)

barplot(table(bank_Data$job),
        main="Vocation",
        col='grey',
        border="black",
        density=c(10,20) , angle=c(45,135)
)

barplot(table(bank_Data$education),
        main="Education",
        col='grey',
        border="black",
        density=c(10,20) , angle=c(45,135)
)

hist(bank_Data$age,
        main="Age",
        col='grey',
        border="black",
        density=c(10,20) , angle=c(45,135),breaks=30
)


numVars <- select_if(bank_Data, is.numeric)             
names(numVars)
numVars

##check unique values in each numeric column
for (i in names(numVars)){
  print(i)
  print(nrow(unique(numVars[i])))
  }

#Split age into three bracket
bank_Data <- bank_Data %>% mutate(age_bracket = ntile(age, 3))
is.numeric(bank_Data$age_bracket)

barplot(table(bank_Data$age_bracket),
        main="age_backet",
        col='grey',
        border="black",
        density=c(10,20) , angle=c(45,135)
)

bank_Data$age_bracket <- ifelse(bank_Data$age_bracket == 1, 'Young', ifelse(bank_Data$age_bracket == 2, 'Medium','Old'))
bank_Data$age_bracket <- as.factor(bank_Data$age_bracket)

##Reduce the dimension of some categorical values
catVars <- select_if(bank_Data, is.character)             
names(catVars)

#Break down job into 5 brackets
unique(bank_Data$job)
no_income <- c('unemployed','student')
low_income <- c('housemaid','blue-collar','services')
mid_income <- c('technician','admin.','self-employed')
high_income <- c('management','entrepreneur')
other_income <- c('retired','unknown')

break_income <- function(x){
  if(x %in% no_income){return('no_income')} 
  else if(x %in% low_income){return('low_income')} 
  else if(x %in% mid_income){return('mid_income')} 
  else if(x %in% high_income){return('high_income')} 
  else {return('other_income')} 
}

bank_Data$job_bracket <- lapply(bank_Data$job,break_income)
bank_Data$job_bracket <- unlist(bank_Data$job_bracket)
bank_Data$job_bracket <- as.factor(bank_Data$job_bracket)
typeof(bank_Data$job_bracket)

barplot(table(bank_Data$job_bracket),
        main="Income Bracket",
        col='grey',
        border="black",
        density=c(10,20) , angle=c(45,135)
)

#Break down education into brackets
unique(bank_Data$education)
low_edu <- c("basic.4y" ,"basic.6y" ,"illiterate" )
mid_edu <- c("high.school","basic.9y")
high_edu <- c("university.degree","professional.course")
break_edu <- function(x){
  if(x %in% low_edu){return('low_edu')} 
  else if(x %in% low_income){return('low_income')} 
  else if(x %in% mid_edu){return('mid_edu')} 
  else if(x %in% high_edu){return('high_edu')}
  else {return('unknown')}
}

bank_Data$edu_bracket <- lapply(bank_Data$education,break_edu)
bank_Data$edu_bracket <- unlist(bank_Data$edu_bracket)
bank_Data$edu_bracket <- as.factor(bank_Data$edu_bracket)
typeof(bank_Data$edu_bracket)


barplot(table(bank_Data$edu_bracket),
        main="Education Bracket",
        col='grey',
        border="black",
        density=c(10,20) , angle=c(45,135),ylim = c(0,20000)
)


##Select all the features
catVars <- select_if(bank_Data, is.character)             
names(catVars)

numVars <- select_if(bank_Data, is.numeric)    
numVars <- names(numVars)[!names(numVars) %in% c('age','duration','pdays')]
numVars

training_feature <- c(c("job_bracket","marital","default","housing","loan","contact",    
                      "month","day_of_week","poutcome","age_bracket","edu_bracket",'y'),numVars)
training_feature

#Convert y label in to boolean
bank_Data$y <- ifelse(bank_Data$y == 'yes',1,0)
bank_Data$y <- as.factor(bank_Data$y)
unique(bank_Data$y)

#7:3 train test split
set.seed(0)
n = nrow(bank_Data)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
training_data = bank_Data[trainIndex,training_feature]
testing_data = bank_Data[-trainIndex,training_feature]

##---------------------Models---------------------------------
#Logistic Regression: Benchmark Feature
logit_model <- glm(y ~ 0+., data = training_data,family=binomial(link='logit'))
length(logit_model)
logit_model$rank
length(logit_model) > logit_model$rank
summary(logit_model)

logit_pred <- logit_model %>% predict(testing_data, type = "response")
logit_pred <- ifelse(logit_pred > 0.5, 1, 0)
# Model accuracy
mean(logit_pred == testing_data$y)

#Summary of Logitstic Reg shows that:
#not significant: nr.employed, previous, marital,education, loan, housing

selected_features <- 
  training_feature[!training_feature %in% c('housing','nr.employed','previous',
                                            'marital','loan','default')]

#Random Forest
library(randomForest)
library(rpart)
library(rpart.plot)
library(varImp)
training_data = bank_Data[trainIndex,selected_features]
testing_data = bank_Data[-trainIndex,selected_features]
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

plot(varImp(forest_out))
plot(forest_out, log="y")
rf_acc <- mean(ifelse(rf_pred == testing_data$y,1,0))
rf_acc

#Decision Tree- Demonstration purposes
dtree_out <- rpart(y ~., data= training_data,control=rpart.control(cp=0.003),method = 'class')
dtree_out
rpart.plot(dtree_out,type=5)
?rpart.control

#KNN with caret select k = 13
training_data$y <- ifelse(training_data$y == 1,'yes','no')
testing_data$y <- ifelse(testing_data$y == 1,'yes','no')
#training_data$y <- as.factor(training_data$y)
knnGrid <-  expand.grid(k = c(1,3,5,7,9,11,13,15,17,19))
names(training_data)

for(i in names(training_data)){
  print(i)
  print(nrow(unique(training_data[i])))
}

training_data
set.seed(0)
ctrl <- trainControl(method="cv",number = 3,classProbs = TRUE,summaryFunction = twoClassSummary)
knn_out <- train(y ~ ., data = training_data, method = "knn", trControl = ctrl, 
                 preProcess = c("center","scale"), tuneGrid = knnGrid)
plot(knn_out)
knn_out

ctrl <- trainControl(method="cv",number = 10)#classProbs = TRUE,summaryFunction = twoClassSummary)
knn_13 <- train(y ~ ., data = training_data, method = "knn", trControl = ctrl, 
                 preProcess = c("center","scale"), tuneGrid = expand.grid(k = c(13)))
knn_13_pred <- predict(knn_13,testing_data)
knn_13_train_acc <- as.numeric(unlist(knn_13["results"])['results.Accuracy'])
knn_13_test_acc <- mean(ifelse(knn_13_pred == testing_data$y,1,0))

#In-sample Accuracy: 0.8947347
knn_13_train_acc
knn_13_test_acc


testing_data$y <- as.factor(testing_data$y)
knn_13_pred <- as.factor(knn_13_pred)
confusionMatrix(data = knn_13_pred, reference = testing_data$y)



