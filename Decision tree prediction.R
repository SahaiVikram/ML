weather<-read.csv("C:/Users/Administrator/Desktop/Data Sets/weather_data.csv")
library(tree)
tree.model= tree(Play~.,data = weather)
summary(tree.model)

Weather_train<-head(weather,10) #70% train data
weather_test<-tail(weather,4) #30% test data

tree.model= tree(Play~.,data = Weather_train)
plot(tree.model)
text(tree.model)
model_prediction=predict(tree.model,weather_test)

maxidx=function(arr){
  return(which(arr==max(arr)))}
idx=apply(model_prediction,c(1),maxidx)
modelprediction=c('No','Yes')[idx]
confmat=table(modelprediction,weather_test$Play) #confusion matrix
confmat
accuracy=sum(diag(confmat))/sum(confmat)
accuracy