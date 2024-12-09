
library(anytime)
library(xts)
library(hts)
library(forecast)

library(rpart)
library(rpart.plot)

# PACKAGES FOR RANDOM FORESTS:
install.packages("stats")
install.packages("dplyr")
install.packages("randomForest")

library(stats)
library(dplyr)
library(randomForest)

f = read.csv("train(1).csv")
print(summary(f))

f$Date = as.Date(anytime(f$Date))
f$Volume = gsub(',','',f$Volume)
f$Market.Cap = gsub(',','',f$Market.Cap)
f$Volume <- NULL
f$Market.Cap = as.numeric(f$Market.Cap)
head(f)

Train = xts(f[, -1], order.by=as.POSIXct(f$Date))
head(Train)

summary(Train)

# set.seed(123)
# split=sample.split(f,SplitRatio=0.8)
# tr_data = subset(f,split==TRUE)
# ts_data = subset(f,split==FALSE)

l1 = lm(Market.Cap~Open+High+Low+Close,Train)
summary(l1)

# a = data.frame(Open=c(144.00,135.78),High=c(146.93,149.78),Low=c(134.05,133.65),Close=c(139.00,137.23))
# result = predict(l1,a)
# all = cbind(a,result)
# print(all)

# APPLYING DECISION TREE----

# tree = rpart(Market.Cap ~ Open+High+Low+Close,Train)
# a = data.frame(Open=c(144.00,135.78),High=c(146.93,149.78),Low=c(134.05,133.65),Close=c(139.00,137.23))
# result = predict(tree,a)
# all = cbind(a,result)
# print(all)
# rpart.plot(tree)
      

# APPLYING RANDOM FOREST----


