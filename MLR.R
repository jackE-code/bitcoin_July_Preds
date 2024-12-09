#MLR
setwd("E:/VIT/DS-LAB/COURSE PROJECT")
library(anytime)
library(xts)
library(e1071)
library(caret)
library(Metrics)



f = read.csv("Bitcoin.csv")
#print(summary(f))
str(f)

f$Date = as.Date(anytime(f$Date))
f$Volume = gsub(',','',f$Volume)
f$Market.Cap = gsub(',','',f$Market.Cap)
f$Volume <- NULL
f$Market.Cap = as.numeric(f$Market.Cap)
# head(f)

Train = xts(f[, -1], order.by=as.POSIXct(f$Date))
# head(Train)

set.seed(123)
index = sample(2, nrow(Train),replace = TRUE,prob=c(0.7,0.3))

Training = Train[index==1,]
Testing = Train[index==2,]

l1 = lm(Close~Open+High+Low+Market.Cap,data=Training)
print(summary(l1))

pred = predict(l1, Testing)
Testing$Predicted_Value = pred
View(Testing)
# View(Training)
x1=1:length(Testing$Close)

plot(x1,pred,type="l",col="blue",lty=1,lwd="1",pch=16,frame=FALSE,xlab = "x",ylab = "Predicted",main = "PREDICTION BY MLR",sub = "Multiple Linear Regression")
lines(x1,Testing$Close,lty=1,col="red",lwd="1")
legend("topleft", legend = c("Original","Predicted"),col=c("red","blue"),lty=1:1,cex=0.6)

mse = mse(Testing$Close, pred)
mae = mae(Testing$Close, pred)
rmse = rmse(Testing$Close, pred)
r2 = R2(Testing$Close, pred, form = "traditional")

cat(" RMSE:", rmse, "\n","R-squared:", r2,"\n","MAE:", mae,"\n","MSE:",mse)

