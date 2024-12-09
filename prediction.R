setwd("E:/VIT/DS-LAB/COURSE PROJECT")
library(anytime)
library(xts)
library(hts)
library(forecast)

library(rpart)
library(rpart.plot)

f = read.csv("train(1).csv")
print(summary(f))
head(f)
f$Date = as.Date(anytime(f$Date))
head(f)

f$Date = as.Date(anytime(f$Date))
f$Volume = gsub(',','',f$Volume)
f$Market.Cap = gsub(',','',f$Market.Cap)
f$Volume <- NULL
f$Market.Cap = as.numeric(f$Market.Cap)
head(f)

Train = xts(f[, -1], order.by=as.POSIXct(f$Date))
head(Train)







# APPLYING DECISION TREE----

# tree = rpart(Market.Cap ~ Open+High+Low+Close,f)
# a = data.frame(Open=c(144.00,135.78),High=c(146.93,149.78),Low=c(134.05,133.65),Close=c(139.00,137.23))
# result = predict(tree,a)
# all = cbind(a,result)
# print(all)
# rpart.plot(tree)

# MLR----

# bitcoin  <- data.frame(close=f$Close,
#                        open=log(f$Open+1),
#                        high=log(f$High),
#                        low=log(f$Low+1),
#                        market=log(f$Market.Cap+1))
# fit <- lm(close ~ open  + high
#                + low + market, data=bitcoin)

#summary(fit)
#plot(fitted(fit), bitcoin$close,ylab="Closing Price", xlab="Predicted Closing price")

a = data.frame(open=c(144.00,135.78),high=c(146.93,149.78),low=c(134.05,133.65),close=c(139.00,137.23))
result = predict(fit,a)
all = cbind(a,result)
print(all)
