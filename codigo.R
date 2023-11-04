#Análise Fatorial Exploratoria

#Passo 1, obter o banco de dados 
#ja mudando a coluna1 para o nome das linhas
dados <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSIaFykBQ88Ru2YQXsoEvCAYCvBob9GMQZLTr46BRBZVFDiFOWtGE5k-A2qJnM7PGaUVRtFTPc5W7Eq/pub?gid=1016079241&single=true&output=csv",
                  row.names = 1)

#Passo 2, bibliotecas
library(psych)
library(corrplot)

#Passo 3, estatisticas basicas dos dados
describe(dados)

#Passo 4, matriz de correlacao
lowerCor(dados)
corrplot.mixed(cor(dados))

#Passo 5, testes para AF
cortest.bartlett(cor(dados), n=55) #p-valor numericamente = 0, recomenda AF
KMO(dados)#valores muito proximos de 1, logo AF recomendada

#Passo 6, criacao de modelos basicos para descobrir
#quantos fatores seriam recomendados
piloto <- principal(dados, nfactors = 6, rotate = "none",
                     covar = F)
plot(piloto$values, type = "b", pch = 18, col="#5049CC",
     main = "Gráfico Scree",
     xlab = "Autovalor",
     ylab = "Valor do Autovalor")

#Passo 7, modelo 2 fatores sem rotacao
modelo1 <- fa(dados, nfactors = 2,
             rotate = "none",
             n.obs = 55)
modelo1$loadings #Proportion Var representa o percentual da variancia que tal fator explica 

#Passo8, Graficos Sem Rotacao
fa.diagram(modelo1,
           main = "Análise Fatorial - Sem Rotação")
plot(modelo1, main = "Pontuação dos Fatores - Sem Rotação",
     xlab = "Fator 1", ylab = "Fator 2", ylim = c(-0.5, 0.5))

#Passo9, modelo 2 fatores com Rotacao
modelo2 <- fa(dados, nfactors = 2, 
              rotate = "varimax", 
              n.obs = 55)
modelo2$loadings

#Passo10, Graficos Com Rotacao
fa.diagram(modelo2, 
           main = "Análise Fatorial - Com Rotação")
plot(modelo2, main = "Pontuação dos Fatores - Com Rotação",
     xlab = "Fator 1", ylab = "Fator 2", ylim = c(0,1))
