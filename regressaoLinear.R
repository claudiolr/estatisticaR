library(openxlsx)
require(tidyverse)
require(RColorBrewer)



# setwd("C:/Users/1247955/OneDrive/CienciaDados/EstatisticaPredicao")
setwd("C:/Users/Claudio/OneDrive/CienciaDados/EstatisticaPredicao/Bases")
dados1 <- read.csv("RegressaoExemplo1.csv", sep = ";")

colnames(dados1) <- c("funcionario",
                      "tempo",
                      "quantClientes")

summary(dados1$tempo) # Estatistica descritiva no R
summary(dados1$quantClientes) # Estatistica descritiva R


dados1 %>% ggplot() + 
     geom_histogram(aes(x= tempo),
                    na.rm = TRUE,
                    bins = 5,
                    col = "white") + 
     labs(title = "Histograma de tempo", x = "Tempo", y = "Frequência")
     


# Análises estatísticas ---------------------------------------------------

cor(dados1$tempo,dados1$quantClientes) #correlação de Pearson no R

cor.test(dados1$tempo,dados1$quantClientes,
         conf.level = 0.99) # teste da correlação de Pearson no R

modelo1 <- lm(dados1$quantClientes ~ dados1$tempo,dados1) # ajuste do modelo de regressão no R

summary(modelo1) #Resultado do modelo de regressão no R
# Estatísticas do erro: 

plot(modelo1) # gráficos de resíduos no R
# Resíduos Vs Ajustados: resultado mostra aleatoriedade
# QQ Plot: gráfico de distribuição normal e testa se os dados estão distribuídos de maneira normal
# Resíduos padronizados Vs Valores ajustados



