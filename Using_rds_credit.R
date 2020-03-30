#***********************************************************************************
#                             Cross validation
#***********************************************************************************

setwd("D:/Git projects/ML in R")




#------------ predict using model


age = c(1.3, 1.43, 0.32, 	-1.68605374)
income = c(1.2, 4, -0.1, 1.509185756)
loan = c(1.2, -1, 0.43, 	1.42040957)

t1 = rbind(income, age, loan)


# Random forests

rf = readRDS('rf.RDS')

prev1 = predict(rf, t1)

prev1


# Neural Net


nn = readRDS('NN.RDS')

prev2 = predict(rf, t1)

prev2





