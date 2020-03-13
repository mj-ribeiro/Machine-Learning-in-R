#---------------------------------------------------------------------------------
#                                 Logit
#---------------------------------------------------------------------------------

setwd("D:/Git projects/ML in R")



df = read.csv('naive_base.csv')
df = df[df$risco != 'moderado',  ]


clas = glm(risco ~ . , family = binomial, data=df )

clas



# let's see the new customers are deadbeat

# história: boa, dívida: alta, garantias: nenhuma, renda: >35
# hitóstia: ruim, dívida: alta, garantias: adequada, renda: <15

historia = c('boa', 'ruim')
divida = c('alta', 'alta')
garantias = c('nenhuma', 'adequada')
renda = c('acima_35', '0_15')
df2 = data.frame(historia, divida, garantias, renda)


prev = predict(clas, df2, type='response')  # type make probabilities appear
prev


risco = ifelse(prev < 0.5, 'baixo', 'alto')









