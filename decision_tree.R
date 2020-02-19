#---------------------------------------------------------------------------------
#                             DECISION TREES
#---------------------------------------------------------------------------------

setwd("D:/Git projects/ML in R")

library(rpart)
library(rpart.plot)        


# data

df = read.csv('naive_base.csv')


# classifier


clas = rpart(formula = risco~ ., data=df, control = rpart.control(minbucket = 1))


# plot tree
windows()
plot(clas)
text(clas)


# other form

rpart.plot(clas)




# predict


# história: boa, dívida: alta, garantias: nenhuma, renda: >35
# hitóstia: ruim, dívida: alta, garantias: adequada, renda: <15

historia = c('boa', 'ruim')
divida = c('alta', 'alta')
garantias = c('nenhuma', 'adequada')
renda = c('acima_35', '0_15')
df2 = data.frame(historia, divida, garantias, renda)

prev = predict(clas, newdata=df2)

prev





