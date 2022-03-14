#iris data decision tree // randomforest
# 7:3ÀÇ ºñÀ²
#mtry=2 or 3, ntree=2001

set.seed(12345)
data<-sample(nrow(iris), size=nrow(iris)*0.7)


iris_train<-iris[data,]
iris_test<-iris[-data,]


iris_model<-rpart(Species ~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                  data=iris_train,
                  method="class")


prediction <- predict(iris_model,newdata=iris_test,type = 'class')


prediction

ylabel<-iris_test$Species

sum(ylabel==prediction) / nrow(iris_test)*100



###############################################
iris_model2<-cforest(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                  newdata=iris_train,
                  controls = cforest_unbiased(mtry=3,ntree=2001))



iris_train



iris_model2 <- randomForest(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                      data=iris_train,
                      mtry=3,
                      ntree=2001)

prediction2 <- predict(iris_model2,iris_test)

prediction2

ylabel2<-iris_test$Species

sum(ylabel2==prediction2) / nrow(iris_test)*100


#######################################################

iris_model3 <- randomForest(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                            data=iris_train,
                            mtry=2,
                            ntree=2001)

prediction3 <- predict(iris_model3,iris_test)


ylabel3<-iris_test$Species

sum(ylabel3==prediction3) / nrow(iris_test)*100
