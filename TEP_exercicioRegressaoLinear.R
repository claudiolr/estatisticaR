library(openxlsx)
require(tidyverse)
require(RColorBrewer)
require(knitr)


###########################################################################
# Exercício 1 -------------------------------------------------------------
###########################################################################

cardio <- read.csv("DoencaCardiaca.csv", sep = ";", dec = ",")

colnames(cardio) = c("id",
                     "idade",
                     "sexo",
                     "IMC",
                     "ccintura",
                     "cquadril",
                     "freqCardiaca",
                     "fumo",
                     "ativFisica",
                     "stress",
                     "pDiastolica",
                     "doencaCardiaca")


cardio <- cardio %>% 
        mutate(sexo = as.factor(sexo),
               fumo = as.factor(fumo)) %>% 
        arrange(stress) %>% 
        mutate(stress = as.factor(stress),
               doencaCardiaca = as.factor(doencaCardiaca))


### II: realize uma análise descritiva de cada variável
# Idade
cardio %>% 
        ggplot(aes(x = idade)) +
        geom_histogram(breaks = seq(16, 63, by=2),
                       col = "white",
                       alpha = 1) + 
        geom_smooth(method = "") +
        labs(title = "Histograma de idade",
             x = "Idade", y = "Frequência") + 
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
              legend.text = element_text(size = 11), axis.text.x = element_text(size = 11))

summary(cardio$Idade)


# Sexo
ggplot(cardio, aes(Sexo, fill = Sexo)) +
     geom_bar() + 
     labs(title = "Sexo",
          x = "", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
           legend.text = element_text(size = 11), axis.text.x = element_text(size = 11))

summary(cardio$Sexo)


# IMC
ggplot(cardio, aes(x = IMC)) +
     geom_histogram(breaks = seq(16, 45, by=2),
                    col = "white",
                    alpha = 1) + 
     labs(title = "Histograma de IMC",
          x = "IMC", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           legend.text = element_text(size = 11), axis.text.x = element_text(size = 11))

summary(cardio$IMC)


# Circunferência da cintura
ggplot(cardio, aes(x = ccintura)) +
     geom_histogram(breaks = seq(48, 126, by=4),
                    col = "white",
                    alpha = 1) + 
     labs(title = "Histograma de cincunferência da cintura",
          x = "Cincunferência da cintura", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13))

summary(cardio$ccintura)


# Cincunferência do quadril
ggplot(cardio, aes(x = cquadril)) +
     geom_histogram(breaks = seq(70, 132, by=4),
                    col = "white",
                    alpha = 1) + 
     labs(title = "Histograma de cincunferência do quadril",
          x = "Cincunferência do quadril", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13))

summary(cardio$cquadril)


# Frequência cardíaca
ggplot(cardio, aes(x = frqCardiaca)) +
     geom_histogram(breaks = seq(47, 122, by=4),
                    col = "white",
                    alpha = 1) + 
     labs(title = "Histograma de frequência cardíaca",
          x = "Frequência cardíaca", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13))

summary(cardio$frqCardiaca)


# Atividade física
ggplot(cardio, aes(atvFisica, fill = atvFisica)) +
     geom_bar() + 
     labs(title = "Atividade física",
          x = "Dias por semana", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
           legend.text = element_text(size = 11), axis.text.x = element_text(size = 11))

summary(cardio$atvFisica)


# Fumo
ggplot(cardio, aes(x = as.factor(fumo)), fill = fumo) +
     geom_bar() + 
     labs(title = "Fumo",
          x = "", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
           legend.text = element_text(size = 11), axis.text.x = element_text(size = 11))


# Stress
ggplot(cardio, aes(x = as.factor(stress), fill = stress)) +
     geom_bar() + 
     labs(title = "Níveis de stress",
          x = "", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
           legend.text = element_text(size = 11), axis.text.x = element_text(size = 11))



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
modelo1 <- lm(pDiastolica ~ idade + sexo + IMC + ccintura + cquadril + freqCardiaca + fumo + ativFisica + stress, data = cardio)
summary(modelo1)
writeanova(modelo1) %>% knitr::kable()

# a função step indica quais variáveis devem ser usadas no modelo (utilizar em 'scale' o residual standard error fornecido no summary)
step(modelo1, direction = "both", scale = 11.36^2)


# Modelo com as variáveis significativas
modelo2 <- lm(pDiastolica ~ idade + sexo + IMC + freqCardiaca, data = cardio)
summary(modelo2)
anova(modelo2) %>% kable()

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