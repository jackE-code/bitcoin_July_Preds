#RANDOM FOREST
setwd("E:/VIT/DS-LAB/COURSE PROJECT")
library(stats)
library(dplyr)
library(randomForest)
library(anytime)
library(xts)
library(Metrics)


f = read.csv("Bitcoin.csv")
#print(summary(f))
str(f)

f$Date = as.Date(anytime(f$Date))
f$Volume = gsub(',','',f$Volume)
f$Market.Cap = gsub(',','',f$Market.Cap)
f$Volume <- NULL
f$Market.Cap = as.numeric(f$Market.Cap)
head(f)

Train = xts(f[, -1], order.by=as.POSIXct(f$Date))
head(Train)



set.seed(123)
index = sample(2, nrow(Train),replace = TRUE,prob=c(0.7,0.3))

Training = Train[index==1,]
Testing = Train[index==2,]

rfm = randomForest(Close~Open+High+Low+Market.Cap,data = Training)
print(rfm)

varImpPlot(rfm)
pred = predict(rfm, Testing)
Testing$Predicted_Value = pred
View(Testing)
x1=1:length(Testing$Close)

plot(x1,pred,type="l",col="green",lty=1,lwd="1",pch=16,frame=FALSE,xlab = "x",ylab = "Predicted",main = "PREDICTION BY RF",sub = "Random Forest")
lines(x1,Testing$Close,lty=1,col="yellow",lwd="1")
legend("topleft", legend = c("Original","Predicted"),col=c("yellow","green"),lty=1:1,cex=0.6)


mse = mse(Testing$Close, pred)
mae = mae(Testing$Close, pred)
rmse = rmse(Testing$Close, pred)
r2 = R2(Testing$Close, pred, form = "traditional")

cat(" RMSE:", rmse, "\n","R-squared:", r2,"\n","MAE:", mae,"\n","MSE:",mse)




# plot(Testing$Date,Testing$Close,type="l",xlab = "Date",ylab="Values")
# confusion_matrix = table(Testing$Close, Testing$Predicted_Value)
# confusion_matrix
# 
# Accuracy = sum(diag(confusion_matrix)/sum(confusion_matrix))
# Accuracy
