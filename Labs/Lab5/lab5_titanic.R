#---------------------------------------------
#----------------Ctree------------------------
#---------------------------------------------
data('Titanic')
View('Titanic')

library(rpart)
rpart_out <- rpart(Survived ~ ., data = Titanic)
print(rpart_out)

library(tree)
require(party)
ctree_out <- ctree(Survived ~., data= Titanic)
plot(ctree_out, main = 'Titanic')

library(cluster)
clust_out <- hclust(dist(Titanic))
print(clust_out)

library(randomForest)
forest_out <- randomForest(Survived ~., data= Titanic)
print(forest_out)
