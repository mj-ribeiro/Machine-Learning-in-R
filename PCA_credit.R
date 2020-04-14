#******************************************************************************************
#                                      PCA
#******************************************************************************************


setwd("D:/Git projects/ML in R")


library(rpart)
library(rpart.plot)        
library(fBasics)
library(caTools)  # split data
library(caret)    #provide metrics for confusion matrix


# data


df = read.csv('credit.csv') 

df$clientid = NULL

attach(df)


# drop na values

m_age = mean(age[age>0 ], na.rm = T)  # average without considering negative values

df$age = ifelse(df$age < 0 |is.na(df$age) , m_age, df$age)




# scaling data

df[, 1:3] = scale(df[, 1:3])

# Encode class

df$default = factor(df$default, levels = c(0,1))



#--------- PCA


pca = preProcess(x= df[-4], method = 'pca', pcaComp = 1)

pca = predict(pca, df[-4])

df$pca = pca


df2 = df[ , c(1,2,3,5,4)]   # reordering coluns



library('correlation')

correlation(df2, pca)










