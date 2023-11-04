#Nome: Ana Julia Cunha e Silva
#RA: 236038
#Analise de Discriminante Linear

#bibliotecas
library(MASS)
library(tidyverse)
library(palmerpenguins)

#Estatisticas Basicas/medidas resumo
summary(penguins[3:6])
summary(penguins[c(1:2, 7)])
as.factor(penguins$year) |> summary()

#Tratamento de dados
pinguim <- penguins |>
  mutate(body_mass_kg = body_mass_g/1000) |> 
  select(species,bill_length_mm,
         bill_depth_mm,flipper_length_mm,
         body_mass_kg) |> 
  drop_na()

#teste de homocedasticidade
bartlett.test(body_mass_kg+bill_length_mm+bill_depth_mm+
                flipper_length_mm~species, 
              data = pinguim)

#grafico de correlacao
pairs(pinguim[2:5], gap = 0,
      col = c("orange", "green", "purple")[pinguim$species],
      pch = 21,
      main = "Matriz Gráfica de Dispersão")

#dividindo os dados em amostras treino(0.7) e teste(0.3)
set.seed(236038)
amos <- sample(c(T, F), nrow(pinguim), replace = T, prob=c(0.7,0.3))
treino <- pinguim[amos, ]
teste <- pinguim[!amos, ] 

#lda
mtot <- lda(species~ ., data = pinguim)
mtot

mtreino <- lda(species~., data = treino)
mtreino

#previsao
pvtot <- predict(mtot)
pvteste <- predict(mtreino, teste)

#visualizacao dos resultados da previsao
plot(LD2~LD1, data = pvtot$x, pch = 21,
     col = c("orange", "green", "purple")[pinguim$species],
     main = "Disperção dos grupos na Previsão - Total")

plot(LD2~LD1, data = pvteste$x, pch = 21,
     col = c("orange", "green", "purple")[teste$species],
     main = "Disperção dos grupos na Previsão - Amostrado")

#matriz de Confusao Banco completo e acuracia
mcb <- table(Alocada = pvtot$class, Verdadeira = pinguim$species)
mcb
acuraciab <- round(sum(diag(mcb))/sum(mcb), 3)
acuraciab

mctest <- table(Alocada = pvteste$class, Verdadeira = teste$species)
mctest
acuraciatest <- round(sum(diag(mctest))/sum(mctest), 3)
acuraciatest
