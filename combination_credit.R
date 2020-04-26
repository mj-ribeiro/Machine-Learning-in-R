#------------------------------------------------------------------------------------
#                       Combination of classifiers
#------------------------------------------------------------------------------------


setwd("D:/Git projects/ML in R")

library(rpart)
library(rpart.plot)        
library(fBasics)
library(caTools)  # split data
library(caret)    #provide metrics for confusion matrix
library(class)  # knn

#----------  Data


df = read.csv('credit.csv') 

df$clientid = NULL

attach(df)


#---------- Drop na values


m_age = mean(age[age>0 ], na.rm = T)  # average without considering negative values

df$age = ifelse(df$age < 0 |is.na(df$age) , m_age, df$age)




#---------- scaling data


df[ ,1:3] = scale(df[, 1:3])

#---------- Encode class


df$default = factor(df$default, levels = c(0,1))




#------------------ Load classifiers


rf = readRDS('rf.RDS')
nn = readRDS('NN.RDS')



#---------------- Predict 


# Random Forests

prev_rf = predict(rf, newdata= df[,-4])

prev_rf = as.numeric(trimws(prev_rf))



# Neural Net


library(h2o)

h2o.init()

prev_nn = predict(nn, newdata= as.h2o(df[ ,-4]))

prev_nn = as.numeric(as.vector(prev_nn))


# Combination

c0_a = 0
c_dif = 0
c1_a = 0

class = c()


for(i in 1:length(prev_nn) ){
c0 = 0
c1 = 0
  
if ( prev_rf[i]==0){
  c0 = c0 + 1
}else {
  c1 = c1 + 1
}

if ( prev_nn[i]==0){
  c0 = c0 + 1
}else {
  c1 = c1 + 1
}

if (c1 > c0){
  print('Classe 1')
  c1_a = c1_a + 1
  class[i] = 1
}else if(c0 == c1){
  print('Os classificadores encontraram resultados diferentes')
  c_dif = c_dif + 1
  class[i] = prev_nn[i]
}else{
  print('Classe 0')
  c0_a = c0_a + 1
  class[i] = 0
}

}

cat('Os dois classificadores classificaram como c1', c1_a, 'registros')
cat('Os dois classificadores classificaram como c0', c0_a, 'registros')
cat('Os dois classificadores chegaram a resultados direntes em', c_dif, 'registros')



table(class)



#----------- Confiability


# Random forest

prob_rf = predict(rf, newdata=df[,-4], type='prob')

conf_rf= c()


for (i in 1:2000){
conf_rf[i] = max(prob_rf$`0`[i], prob_rf$`1`[i])
}
View(conf_rf)



t.test(conf_rf)


# Neural Networks


prob_nn = predict(nn, newdata=as.h2o(df[,-4]), type='prob')

conf_nn= c()

for (i in 1:2000){
  conf_nn[i] = max(prob_nn$`0`[i], prob_nn$`1`[i])
}


View(conf_nn)




