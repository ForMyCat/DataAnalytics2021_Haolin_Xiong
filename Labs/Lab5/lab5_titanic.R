#---------------------------------------------
#----------------Ctree------------------------
#---------------------------------------------

require(rpart)
?filter
titanic_df <- read.csv('train.csv')
titan_rpart <- rpart(Survived ~ Pclass + Sex + Age + Fare, data = titanic_df)
plot(titan_rpart) # try some different plot options
text(titan_rpart) # try some different text options

require(party)
library(party)
titan_ctree <- ctree(Survived ~ Pclass + Sex + Age + Fare, data = titanic_df)
plot(titan_ctree)

titanic_df_dropna <- na.omit(titanic_df)
titanic_df_hc <- titanic_df[,c(2,3,5,6,10)]
hc <- hclust(titanic_df_hc, "ave")
plot(hc)
plot(hc, hang = -1)

titanic_df_hc
View(titanic_df_dropna)
