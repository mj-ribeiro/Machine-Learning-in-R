setwd("D:/Git projects/ML in R")

df = read.csv('naive_base.csv')


library(e1071) # library to work with naive bayes

clas = naiveBayes(x=df[-5], y = df$risco)

print(clas)


# new swindlers

#hist�ria:boa, d�vida: alta, garantias:nenhuma, renda:>35
#hist�ria:ruim, d�vida: alta, garantias:adequada, renda:<15

hist�ria = c('boa')
d�vida = c('alta')
garantias =  c('nenhuma')
renda = c('acima_35')
          
df2 = data.frame(hist�ria, d�vida, garantias, renda)



prev = predict(clas, newdata = df2, 'raw') # esse raw faz aparecer os valores das probabilidades
print(prev) # a probabilidade do cara ser caloteiro � baixa









