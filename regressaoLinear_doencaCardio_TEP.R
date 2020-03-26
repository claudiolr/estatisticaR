library(openxlsx)
require(tidyverse)
require(RColorBrewer)


# Exercício 1 -------------------------------------------------------------
setwd("C:/Users/Claudio/OneDrive/CienciaDados/EstatisticaPredicao/Bases")
cardio <- read.xlsx("DoencaCardiaca.xlsx")


### III: Construa uma matriz de correlação com as variáveis quantitativas
cor.test(cardio$pdiasto, cardio$Idade, conf.level = 0.99) # IDade
cor.test(cardio$pdiasto, cardio$IMC, conf.level = 0.99)     # IMC
cor.test(cardio$pdiasto, cardio$ccintura, conf.level = 0.99) # ccintura
cor.test(cardio$pdiasto, cardio$cquadril, conf.level = 0.99) # cquadril
cor.test(cardio$pdiasto, cardio$frqCardiaca, conf.level = 0.99) # frequência cardíaca
cor.test(cardio$pdiasto, cardio$atvFisica, conf.level = 0.99) # Atividade física

cor(cardio[, c(2, 4:7, 9, 11)]) # gera a matriz de correlação das variáveis quantitativas listadas (2, 4:7, 9, 11)


### IV: 
# Modelo com todas as variáveis
modelo1 <- lm(cardio$pdiasto ~ cardio$Idade+cardio$Sexo+cardio$IMC+cardio$ccintura+cardio$cquadril+cardio$frqCardiaca+
                   cardio$fumo+cardio$atvFisica+cardio$stress+cardio$pdiasto)
summary(modelo1)

# a função step indica quais variáveis devem ser usadas no modelo (utilizar em 'scale' o residual standard error fornecido no summary)
step(modelo1, direction = "both", scale = 11.36^2)


# Modelo com as variáveis significativas
modelo2 <- lm(cardio$pdiasto ~ cardio$Idade+cardio$Sexo+cardio$IMC+cardio$frqCardiaca)
summary(modelo2)
par(mfrow = c(2,2))
plot(modelo2, which = c(1:4), pch = 20)



### Parte II: nova base
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


# Temperatura média
ggplot(consumo, aes(x = tempMedia)) +
     geom_histogram(breaks = seq(10, 37, by=1),
                    col = "white",
                    alpha = 1,
                    fill = "#9394A3") + 
     labs(title = "Histograma de temperatura média",
          x = "Temperatura média", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13))

summary(consumo$tempMedia)


# Temperatura mínima
ggplot(consumo, aes(x = tempMin)) +
     geom_histogram(breaks = seq(10, 37, by=1),
                    col = "white",
                    alpha = 1,
                    fill = "#3AB1FF") + 
     labs(title = "Histograma de temperatura mínima",
          x = "Temperatura mínima", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13))

summary(consumo$tempMin)


# Temperatura média
ggplot(consumo, aes(x = tempMax)) +
     geom_histogram(breaks = seq(10, 37, by=1),
                    col = "white",
                    alpha = 1,
                    fill = "#A31411") + 
     labs(title = "Histograma de temperatura máxima",
          x = "Temperatura máxima", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13))

summary(consumo$tempMax)


# Precipitação
ggplot(consumo, aes(x = precipitacao)) +
     geom_histogram(breaks = seq(0, 95, by=10),
                    col = "white",
                    alpha = 1,
                    fill = "#2E58FF") + 
     labs(title = "Histograma de precipitação",
          x = "Precipitação (ml)", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13))

summary(consumo$precipitacao)


# Consumo de cerveja
ggplot(consumo, aes(x = consumoLitros)) +
     geom_histogram(breaks = seq(14, 37, by=2),
                    col = "white",
                    alpha = 1,
                    fill="#CCA100") + 
     labs(title = "Histograma de consumo de cerveja",
          x = "Consumo (l)", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13))

summary(consumo$consumoLitros)


# Fim de semana
ggplot(consumo, aes(x = fimDeSemana, fill = fimDeSemana)) +
     geom_bar() +
     labs(title = "Fim de semana",
          x = "", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
           legend.text = element_text(size = 11), axis.text.x = element_text(size = 11)) +
     scale_fill_manual(values = c("#2C3E50", "#16A085"), labels = c("Fim de semana","Meio de semana"))


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