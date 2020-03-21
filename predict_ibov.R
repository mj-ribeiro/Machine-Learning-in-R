#############################################################################
#                             Predict IBOV
#############################################################################

setwd("D:/Git projects/ML in R")



library(tseries)
library(timeSeries)
library(forecast)   # auto.arima
library(quantmod)
library(fGarch)
library(mFilter)



# Get data



vix = getSymbols('^VIX', src='yahoo', from= '2007-01-01', auto.assign = F)[,4]
ibov = getSymbols('^BVSP', src='yahoo', from= '2007-01-01', auto.assign = F)[,4]
sp500 = getSymbols('^GSPC', src='yahoo', from= '2007-01-01', auto.assign = F)[,4]



########### Processing data


length(vix)
length(sp500)
length(ibov)


# stats


basicStats(vix)
basicStats(sp500)
basicStats(ibov)


# drop nas ibov

filter1 = is.na(ibov)==F
ibov = ibov[filter1]



#  merge and drop nas



ativo = merge(ibov, vix, sp500)

filter2 = is.na(ativo[ ,1])==F & is.na(ativo[ ,2])==F & is.na(ativo[ ,3])==F

ativo = ativo[filter2, ]



basicStats(ativo)    # very good !!


# extract dates


dates = index(ativo)

#dates = as.matrix(dates)



# transform in time series

tsibov=timeSeries(data=ativo$BVSP.Close,charvec=dates,format='%d/%m/%Y')
tssp500=timeSeries(data=ativo$GSPC.Close,charvec=dates,format='%d/%m/%Y')
tsvix=timeSeries(data=ativo$VIX.Close ,charvec=dates,format='%d/%m/%Y')


# plots

windows()
par(mfrow=c(1,3))
plot(tsibov, ylab='IBOV')
plot(tssp500, ylab='SP500')
plot(tsvix, ylab='VIX')




sp500f=as.zoo(tssp500)
sibovf=as.zoo(tsibov)
svixf=as.zoo(tsvix)


#### calculate returns

# IBOV


ret_ibov = log(ativo$BVSP.Close/lag(ativo$BVSP.Close))

hist(ret_ibov, breaks=40, col="slateblue")


# VIX

ret_vix = log(ativo$VIX.Close/lag(ativo$VIX.Close))
ret_vix = lag(ret_vix)


hist(ret_vix, breaks=40, col="slateblue")


# SP&500

ret_sp500 = log(ativo$GSPC.Close/lag(ativo$GSPC.Close))
ret_sp500 = lag(ret_sp500)

hist(ret_sp500, breaks=40, col="slateblue")


### Stats of returns


library(PerformanceAnalytics)

table.Stats(ret_ibov)

table.Stats(ret_vix)

table.Stats(ret_sp500)
  

windows()
par(mfrow=c(3,1))
plot(ret_ibov^2, main= 'Square of IBOV returns (2007 - 2000)')
plot(ret_sp500^2, main= 'Square of SP&500 returns (2007 - 2000)')
plot(ret_vix^2, main= 'Square of VIX returns (2007 - 2000)')


###   binarize ibov returns


ret_bin = ifelse(ret_ibov< 0, 0, 1)

ret_bin_def = lag(ret_bin)


## Machine Learning procedure



library(rpart)
library(rpart.plot)        
library(fBasics)
library(caTools)  # split data
library(caret)    #provide metrics for confusion matrix
library(class)  # knn



ret = merge(ret_sp500, ret_vix, ret_bin_def, ret_bin)
filter3 = is.na(ret[, 1])==F & is.na(ret[, 2])==F & is.na(ret[,3])==F & is.na(ret[,4])==F

ret = ret[filter3, ]


# train and test

set.seed(4)
div = sample.split(ret$BVSP.Close, SplitRatio = 0.75)
df_train = subset(ret, div == TRUE)
df_test = subset(ret, div == FALSE)



#------------ Algorithm Logit


clas = glm(formula = BVSP.Close.1 ~. , family = binomial, data=df_train)


summary(clas)


prob = predict(clas, type='response', newdata = df_test)

risk = ifelse(prob>0.5, 1, 0)



#------- Confusion Matrix


conf_matrix = table(df_test[ ,4], risk)

conf_matrix

confusionMatrix(conf_matrix)








