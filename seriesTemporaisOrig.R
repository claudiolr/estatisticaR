library(forecast)
library(ggplot2)
library(tidyverse)
library(TTR)
library(tidyquant)
library(lubridate)


dados1 <- read.csv("EmpregoSetorEconomia.csv", sep = ";", stringsAsFactors = FALSE, dec = ",")


serie2 = ts(dados1$Comercio,start = c(2003,1), end=c(2007,12), frequency=12)
autoplot(serie2)

serie3 = ts(dados1$Alimento,start = c(2003,1), end=c(2007,12), frequency=12)
autoplot(serie3)

serie4 = ts(dados1$Metalurgia,start = c(2003,1), end=c(2007,12), frequency=12)
autoplot(serie4)



#explorando parte da serie
window(serie2,2003,c(2004,12))
plot(window(serie2,2003,c(2004,12)))
hist(window(serie2,2003,c(2004,12)))
boxplot(window(serie2,2003,c(2004,12)))



# Modelo de média móvel

# calculando a media movel com ordem 3
ma(serie4, order = 3)
autoplot(ma(serie4, order = 3))
forecast(ma(serie4, order = 3), h = 12)
autoplot(forecast(ma(serie4, order = 5), h = 12))

# calculando a media movel com ordem 5
ma(serie4, order = 5)
autoplot(ma(serie4, order = 5))


# calculando a media movel com ordem 12
ma(serie4, order = 12)
autoplot(ma(serie4, order = 12))


#comparando 
plot(serie4)
lines(ma(serie4, order = 3), col="red")
lines(ma(serie4, order = 5), col="blue")
lines(ma(serie4, order = 12), col="green")

#legenda
legend("bottomright",legend=c("Orig.","MM3", "MM5","MM12"), col = c("black","red","blue","green"), lty=1:2, cex=0.8)



# 1. alisamento exponencial Simples ANN
# ajusta a série em função de três parâmetros (três letras: ver ajuda)
# valor de alpha próximo de 1: o peso está quase todo na última observação
# AIC, AICc e BIC: servem para comparação com outros modelos. Quanto menor, melhor.
autoplot(ets(serie4, "ANN")$residuals)
autoplot(ets(serie4, "ANN")$fitted)

forecast(ets(serie4, "ANN"), h=16,levels=c(85,90))
print(forecast(ets(serie4, "ANN"), h=16,levels=c(85,90)))
autoplot(forecast(ets(serie4, "ANN"), h=16,levels=c(85,90)))



# 2. alisamento exponencial Duplo
autoplot(ets(serie2, "AAN")$residuals)
autoplot(ets(serie2, "AAN")$fitted)

forecast(ets(serie2, "AAN"), h=16,levels=c(85,90))
print(forecast(ets(serie2, "AAN"), h=16,levels=c(85,90))$mean)
autoplot(forecast(ets(serie2, "AAN"), h=16,levels=c(85,90)))



# 3. alisamento exponencial Duplo - Holt
autoplot(ets(serie2, "AAN")$residuals)
autoplot(ets(serie2, "AAN")$fitted)

forecast(ets(serie2, "AAN"), h=16,levels=c(85,90))
print(forecast(ets(serie2, "AAN"), h=16,levels=c(85,90))$mean)
autoplot(forecast(ets(serie2, "AAN"), h=16,levels=c(85,90)))



# 4. alisamento exponencial  - holt - Winters - Aditivo
autoplot(ets(serie2, "AAA")$residuals)
autoplot(ets(serie2, "AAA")$fitted)

forecast(ets(serie2, "AAA"), h=16,levels=c(85,90))
print(forecast(ets(serie2, "AAA"), h=16,levels=c(85,90))$mean)
autoplot(forecast(ets(serie2, "AAA"), h=16,levels=c(85,90)))



# 5. alisamento exponencial  - holt - Winters - Multiplicativo
ets(serie2, "MAM")
autoplot(ets(serie2, "MAM")$residuals)
autoplot(ets(serie2, "MAM")$fitted)

forecast(ets(serie2, "MAM"), h=16,levels=c(85,90))
print(forecast(ets(serie2, "MAM"), h=16,levels=c(85,90))$mean)
autoplot(forecast(ets(serie2, "MAM"), h=16,levels=c(85,90)))



# 6. alisamento exponencial  - holt - Winters - Aditivo
ets(serie3, "ANA")
autoplot(ets(serie3, "ANA")$residuals)
autoplot(ets(serie3, "ANA")$fitted)

forecast(ets(serie3, "ANA"), h=16,levels=c(85,90))
print(forecast(ets(serie3, "ANA"), h=16,levels=c(85,90))$mean)
autoplot(forecast(ets(serie3, "ANA"), h=16,levels=c(85,90)))



# 7. alisamento exponencial  - holt - Winters - Multiplicativo
ets(serie3 ,"MNM")
autoplot(ets(serie3 ,"MNM")$residuals)
autoplot(ets(serie3 ,"MNM")$fitted)

forecast(ets(serie3 ,"MNM"), h=16,levels=c(85,90))
print(forecast(ets(serie3 ,"MNM"), h=16,levels=c(85,90))$mean)
autoplot(forecast(ets(serie3 ,"MNM"), h=16,levels=c(85,90)))




#comparando modelos
treino = window(serie2, 2003, c(2005, 12))
teste  = window(serie2, 2006, c(2007, 12))


# 8. alisamento exponencial  - holt - Winters - Aditivo
ets(treino, "AAA")

autoplot(ets(treino, "AAA")$residuals)
autoplot(ets(treino, "AAA")$fitted)

forecast(ets(treino, "AAA"), h=24)
print(forecast(ets(treino, "AAA"), h=24)$mean)
autoplot(forecast(ets(treino, "AAA"), h=24))



#alisamento exponencial  - holt - Winters - Multiplicativo
ets(treino, "MAM")

autoplot(ets(treino, "MAM")$residuals)
autoplot(ets(treino, "MAM")$fitted)

forecast(ets(treino, "MAM"), h=24)
print(forecast(ets(treino, "MAM"), h=24)$mean)
autoplot(forecast(ets(treino, "MAM"), h=24))


#################################################################################

plot(serie2)
lines(forecast(ets(treino, "AAA"), h=24)$mean, col="blue")
lines(forecast(ets(treino, "MAM"), h=24)$mean, col="red")
lines(teste, col="green")
legend("topleft",legend=c("AAA","MAM","Teste"), col = c("blue","red","green"), lty=1:2, cex=0.8)

accuracy(forecast(ets(treino, "AAA"), h=24),teste )
accuracy(forecast(ets(treino, "MAM"), h=24),teste )
