#SVR
setwd("E:/VIT/DS-LAB/COURSE PROJECT")
library(e1071)
library(caret)
library(Metrics)
library(anytime)
library(xts)

f = read.csv("Bitcoin.csv")
str(f)
set.seed(123)
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

model = svm(Close~Open+High+Low+Market.Cap,data=Training,kernel="linear")
print(model)

pred = predict(model, Testing)
Testing$predicted_value=pred
View(Testing)
x=1:length(Testing$Close)


plot(x,pred,type="l",col="blue",lty=1,lwd="1",pch=16,frame=FALSE,xlab = "x",ylab = "Predicted",main = "PREDICTION BY SVR",sub = "Support Vector Regression")
lines(x,Testing$Close,lty=1,col="orange",lwd="1")
legend("center", legend = c("Original","Predicted"),col=c("orange","blue"),lty=1:1,cex=0.8)

mse = mse(Testing$Close, pred)
mae = mae(Testing$Close, pred)
rmse = rmse(Testing$Close, pred)
r2 = R2(Testing$Close, pred, form = "traditional")

cat(" RMSE:", rmse, "\n","R-squared:", r2,"\n","MAE:", mae,"\n","MSE:",mse)
