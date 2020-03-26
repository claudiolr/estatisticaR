library(openxlsx)
require(tidyverse)
require(RColorBrewer)



setwd("D:/CienciaDados/Bases/ConsumoCervejaSP")
consumo <- read.csv("Consumo_cerveja.csv", sep = ",", dec = ",")

colnames(consumo) <- c("data",
                       "tempMedia",
                       "tempMin",
                       "tempMax",
                       "precipitacao",
                       "fimDeSemana",
                       "consumoLitros")
consumo <- consumo %>% filter(!is.na(tempMedia))


consumo$consumoLitros <- as.numeric(as.character(consumo$consumoLitros))
consumo$fimDeSemana <- as.factor(consumo$fimDeSemana)


### Matriz de correlação
cor.test(consumo$consumoLitros, consumo$tempMedia, conf.level = 0.99)
cor.test(consumo$consumoLitros, consumo$tempMax, conf.level = 0.99)
cor.test(consumo$consumoLitros, consumo$tempMin, conf.level = 0.99)
cor.test(consumo$consumoLitros, consumo$precipitacao, conf.level = 0.99)

cor(consumo[,c(2:5, 7)])# gera a matriz de correlação das variáveis quantitativas listadas (2, 4:7, 9, 11)

plot(consumo$consumoLitros, consumo$tempMax)


### Modelos de regressão linear
## As variáveis de temperatura apresentam colinearidade. Por isso, o modelo deve escolher apenas uma delas

# Modelo com todas as variáveis
modelo1 <- lm(consumo$consumoLitros ~ consumo$tempMax + consumo$tempMedia + consumo$tempMin + consumo$precipitacao + consumo$fimDeSemana)
summary(modelo1)
step(modelo1, direction = "both", scale = 2.333^2) # indica quais variáveis devem ser usadas no modelo


# Modelo com as variáveis significativas, com a escolha da temperatura mínima
modelo2 <- lm(consumo$consumoLitros ~ consumo$tempMin + consumo$precipitacao + consumo$fimDeSemana)
summary(modelo2)


# Modelo com as variáveis significativas, com a escolha da temperatura máxima
modelo3 <- lm(consumo$consumoLitros ~ consumo$tempMax + consumo$precipitacao + consumo$fimDeSemana)
summary(modelo3)
par(mfrow = c(2,2)) # organiza o painel de gráficos em 2 linhas e 2 duas colunas para que todos os gráficos sejam plotados juntos
plot(modelo3, which = c(1:4), pch = 20)
step(modelo3, direction = "both", scale = 2.326^2)