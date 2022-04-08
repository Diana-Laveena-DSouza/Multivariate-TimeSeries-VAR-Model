getwd()
setwd('C:/Users/lloyd/Desktop/simplilearn/pg_program_ds&business_analytics/course/courses/Data Science with R/Machine_Learning_R/multivariate Time Series')
#load file
data=read.csv('Raotbl6.csv')
#formatting date
data$date=as.Date(data$date)
str(data)
#Set index value
library(data.table)
data=as.data.table(data)
setkey(data, date)
head(data)
#Checking missing values
summary(data)
#plot the data
plot(data)
#change to time series
ts_data=ts(data[,2:9], start=c(1959), frequency=4)
ts_data
plot(ts_data)
#Dickey-puller test
library(tseries)
apply(ts_data, 2, adf.test)
#Since p-value>0.05. Therefore it is not stationary
ts_newdata_diff=diff(ts_data, differerences=1)
ts_newdata_diff
#Dickey-puller test
apply(ts_newdata_diff, 2, adf.test)
#Since p-value>0.05. differencing again.
ts_newdata_diff2=diff(ts_newdata_diff, differerences=1)
ts_newdata_diff2
#Dickey-puller test
apply(ts_newdata_diff2, 2, adf.test)
#since p<0.05. It is stationary
library(vars)
VARselect(ts_newdata_diff2, lag.max=10)
model=VAR(ts_newdata_diff2, p=10)
#causality
for (i in colnames(ts_data)) {
  print(causality(model, cause = i))
}
predicted_difference2=model$y
#cumsum
predicted_values=ts(apply(apply(predicted_difference2, 2, cumsum)+ts_newdata_diff[rep(1, 121),], 2, cumsum)+ts_data[rep(1, 121),], start=c(1959), frequency=4)
predicted_values
library(forecast)
autoplot(ts_data)+autolayer(predicted_values, colour = FALSE)
