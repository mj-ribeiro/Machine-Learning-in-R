#***********************************************************************************
#                             Neural Networks
#***********************************************************************************

setwd("D:/Git projects/ML in R")



library(rpart)
library(rpart.plot)        
library(fBasics)
library(caTools)  # split data
library(caret)    #provide metrics for confusion matrix

library(glue)

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



# train and test

set.seed(1)
div = sample.split(df$income, SplitRatio = 0.75)
df_train = subset(df, div == TRUE)
df_test = subset(df, div == FALSE)




#---- Algorithm



library('h2o')
h2o.init(nthreads = -1)


clas = h2o.deeplearning(y = 'default',
                        training_frame = as.h2o(df_train),
                        activation = 'Rectifier',
                        hidden= c(100),
                        epochs = 1000)

summary(clas)


prev = h2o.predict(clas, newdata=as.h2o(df_test[-4]))
prev = prev$predict


prev = as.vector(prev)


View(prev)



#--------- Confusion Matrix

conf_matrix = table(df_test[ ,4], prev)

conf_matrix


confusionMatrix(conf_matrix)




#----------- Plots

library(neuralnet)

NN = neuralnet(default ~ income + loan + age, df_train, hidden = 3, linear.output = T)

# traçar rede neural
plot (NN)


prev2 = predict(NN, df_test[, -4])


#--- Confusion matrix

conf_matrix2 = table(df_test[,4], prev)

confusionMatrix(conf_matrix2)











