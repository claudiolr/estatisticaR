library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
library(lubridate)
library(mfx)
library(caret)
library(pROC)
library(ResourceSelection)
library(modEvA)
library(foreign)
library(stargazer)
library(haven)



# Carregamento e tratamento -----------------------------------------------
setwd("~/GitHub/estatisticaR")
emprego <- read.csv("EmpregoFormalIndicadoresEconomicos.csv", sep = ";", header = TRUE, dec = ",", stringsAsFactors = FALSE)

colnames(emprego) = c("mes", "PIB", "inflacao", "taxaJuros", "taxaCambio", "patentes", "saldoEmprego")
emprego$saldoEmprego <- as.factor(emprego$saldoEmprego)

emprego$mes <- paste0("01-",emprego$mes)
emprego$mes <- as.Date(emprego$mes, tryFormats = "%d-%b-%y")



# Análise das variáveis ---------------------------------------------------
gridExtra::grid.arrange(
     emprego %>% 
          ggplot(aes(x = PIB)) +
          geom_histogram(aes(y = ..density..), breaks = seq(-2, 3, by = 0.5), colour = "white") + 
          stat_function(fun = dnorm, args = list(mean = mean(emprego$PIB), sd = sd(emprego$PIB))) +
          labs(title = "Histograma de PIB", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5))
     ,
     emprego %>% 
          ggplot(aes(x = "")) +
          geom_boxplot(aes(y = PIB)) +
          labs(title = "Boxplot de PIB", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5),
                axis.ticks.x = element_blank())
     , ncol = 2)


gridExtra::grid.arrange(
     emprego %>% 
          ggplot(aes(x = inflacao)) +
          geom_histogram(aes(y = ..density..), breaks = seq(-0.5, 1.5, by = 0.1), colour = "white") + 
          stat_function(fun = dnorm, args = list(mean = mean(emprego$inflacao), sd = sd(emprego$inflacao))) +
          labs(title = "Histograma de inflação", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5))
     ,
     emprego %>% 
          ggplot(aes(x = "")) +
          geom_boxplot(aes(y = inflacao)) +
          labs(title = "Boxplot de inflação", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5),
                axis.ticks.x = element_blank())
     , ncol = 2)


gridExtra::grid.arrange(
     emprego %>% 
          ggplot(aes(x = taxaJuros)) +
          geom_histogram(aes(y = ..density..), breaks = seq(5, 20, by = 1), colour = "white") + 
          stat_function(fun = dnorm, args = list(mean = mean(emprego$taxaJuros), sd = sd(emprego$taxaJuros))) +
          labs(title = "Histograma de taxa de juros", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5))
     ,
     emprego %>% 
          ggplot(aes(x = "")) +
          geom_boxplot(aes(y = taxaJuros)) +
          labs(title = "Boxplot de taxa de juros", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5),
                axis.ticks.x = element_blank())
     , ncol = 2)


gridExtra::grid.arrange(
     emprego %>% 
          ggplot(aes(x = taxaCambio)) +
          geom_histogram(aes(y = ..density..), breaks = seq(1.5, 4.5, by = 0.3), colour = "white") + 
          stat_function(fun = dnorm, args = list(mean = mean(emprego$taxaCambio), sd = sd(emprego$taxaCambio))) +
          labs(title = "Histograma de taxa de câmbio", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5))
     ,
     emprego %>% 
          ggplot(aes(x = "")) +
          geom_boxplot(aes(y = taxaCambio)) +
          labs(title = "Boxplot de taxa de câmbio", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5),
                axis.ticks.x = element_blank())
     , ncol = 2)


gridExtra::grid.arrange(
     emprego %>% 
          ggplot(aes(x = patentes)) +
          geom_histogram(aes(y = ..density..), breaks = seq(4, 200, by = 20), colour = "white") + 
          stat_function(fun = dnorm, args = list(mean = mean(emprego$patentes), sd = sd(emprego$patentes))) +
          labs(title = "Histograma de patentes", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5))
     ,
     emprego %>% 
          ggplot(aes(x = "")) +
          geom_boxplot(aes(y = patentes)) +
          labs(title = "Boxplot de taxa de câmbio", x = "", y = "") + 
          theme(plot.title = element_text(size = 16, hjust = 0.5),
                axis.ticks.x = element_blank())
     , ncol = 2)



emprego %>% 
     group_by(saldoEmprego) %>% summarise(Total = n()) %>% 
     mutate(Percentual = round(Total/sum(Total), digits = 3)*100) %>% 
     ggplot(aes(x = saldoEmprego, y = Total, fill = saldoEmprego, label = paste0(saldoEmprego,"\n",Total," (",Percentual,"%)"))) +
     geom_col() + 
     labs(title = "Gráfico de saldo de emprego", x = "", y = "") + 
     geom_label(aes(y = Total), colour = "white", fontface = "bold", position = position_stack(vjust = 0.60)) +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           axis.text = element_blank(), axis.ticks = element_blank(),
           legend.position = "none")



# Regressão ---------------------------------------------------------------
emprego$saldoEmprego <- as.factor(ifelse(emprego$saldoEmprego == "Negativo", "0", "1"))

### Modelo 1
modeloEmpr1 <- glm(saldoEmprego ~ PIB + inflacao + taxaJuros + taxaCambio + patentes, data = emprego, family = binomial(link = logit))
summary(modeloEmpr1)
coef(modeloEmpr1)


ggplot(emprego, aes(x = taxaCambio, y = ajuste2)) +
     geom_point() + 
     stat_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
     labs(title = "Distribuição de probabilidades pela taxa de câmbio",
          x = "taxa de câmbio", y = "valores ajustados") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))

ggplot(emprego, aes(x = PIB, y = ajuste2)) +
     geom_point() + 
     stat_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
     labs(title = "Distribuição de probabilidades pelo número de patentes",
          x = "número de patentes", y = "valores ajustados") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))



# Matriz de confusão (tabela de classificação)
emprego$ajuste1 <- predict(modeloEmpr1, type = "response")
emprego$ajuste1_1 <- as.factor(ifelse(emprego$ajuste1 > 0.5, "1" , "0"))

confusionMatrix(emprego$ajuste1_1, emprego$saldoEmprego, positive="1")


# Pseudo R2
RsqGLM(modeloEmpr1)


### Modelo 2
modeloEmpr2 <- glm(saldoEmprego ~ taxaCambio + patentes, data = emprego, family = binomial(link = logit))
summary(modeloEmpr2)
coef(modeloEmpr2)
exp(coef(modeloEmpr2))


# Matriz de confusão (tabela de classificação)
emprego$ajuste2 <- predict(modeloEmpr2, type = "response")
emprego$ajuste2_1 <- as.factor(ifelse(emprego$ajuste2 > 0.5, "1" , "0"))

confusionMatrix(emprego$ajuste2_1, emprego$saldoEmprego, positive="1")


# Pseudo R2
RsqGLM(modeloEmpr2)



# Séries temporais --------------------------------------------------------
empregoPV <- emprego
colnames(empregoPV) = c("mes", "PIB", "Inflação", "Taxa de Juros", "Taxa de Câmbio", "Patentes", "saldoEmprego")

empregoPV <- empregoPV %>% 
     select(-saldoEmprego) %>% 
     pivot_longer(cols = c("PIB", "Inflação", "Taxa de Juros", "Taxa de Câmbio", "Patentes"), names_to = "tipo", values_to = "valores")


### PIB
# Série

empregoPV %>% 
     filter(tipo == "PIB") %>% 
     mutate(mes = floor_date(mes, "3 months")) %>% 
     group_by(mes) %>% 
     summarise(PIB = mean(valores)) %>% 
     ggplot(aes(x = mes, y = PIB)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_labels = "%Y") +
     labs(title = "PIB", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           axis.text.x = element_text(angle = 0, hjust = 0.5))


# Média móvel
empregoPV %>% 
     filter(tipo == "PIB") %>% 
     mutate(mes = floor_date(mes, "3 months")) %>% 
     group_by(mes) %>% 
     summarise(valores = mean(valores)) %>% 
     ggplot(aes(x = mes, y = valores)) + 
     geom_line(size = .7) +
     geom_ma(aes(colour = 'MM3'), ma_fun = SMA, n = 3, size = 1, show.legend = TRUE) + # media movel com ordem 3
     geom_ma(aes(colour = 'MM5'), ma_fun = SMA, n = 5, size = 1, show.legend = TRUE) + # media movel com ordem 5
     geom_ma(aes(colour = 'MM12'), ma_fun = SMA, n = 12, size = 1, show.legend = TRUE) + # media movel com ordem 12
     labs(title = "Predição dos valores do PIB", subtitle = "(média móvel simples)", x = "", y = "") +
     scale_colour_manual(name = "", values = c('MM3' = "red",
                                               'MM5' = "blue",
                                               'MM12' = "darkgreen"), labels = c("MMS(12)",
                                                                                 "MMS(3)",
                                                                                 "MMS(5)")) +
     scale_x_date(breaks = "year", date_labels = "%Y") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           legend.position = "bottom")


pib = ts(emprego$PIB, start = c(2005,1), end = c(2017, 12), frequency = 12)
pibTreino = window(pib, 2005, c(2016, 12))
pibTeste = window(pib, 2017, c(2017, 12))

autoplot(forecast(ets(pib), h = 16,levels = c(85, 90)))
autoplot(forecast(ets(pib, "AAA")), h = 16, levels = c(85, 90))
autoplot(forecast(ets(pib, "AAN")), h = 16, levels = c(85, 90))

knitr::kable(accuracy(forecast(ma(pib, order = 12)), h = 16, levels = c(85, 90)))
knitr::kable(accuracy(forecast(ets(pib, "AAN")), h = 16, levels = c(85, 90)))

autoplot(forecast(ma(pib, order = 12)), h = 16, levels = c(85, 90))

plot(pib)
lines(forecast(ets(pibTreino, "AAA"), h=16, levels=c(85,90))$mean, col = "blue")
lines(forecast(ets(pibTreino, "AAN"), h=16, levels=c(85,90))$mean, col = "red")
lines(pibTeste, col = "green")

knitr::kable(accuracy(forecast(ma(pibTreino, order = 12), h = 24), pibTeste))
knitr::kable(accuracy(forecast(ets(pibTreino, "AAN"), h = 24), pibTeste))


### Inflação
# Série
emprego %>% 
     ggplot(aes(x = mes, y = inflacao)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Inflação", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


# Média móvel
empregoPV %>% 
     filter(tipo == "Inflação") %>% 
     group_by(mes) %>% 
     summarise(valores = mean(valores)) %>% 
     ggplot(aes(x = mes, y = valores)) + 
     geom_line(size = .7) +
     geom_ma(aes(colour = 'MM3'), ma_fun = SMA, n = 3, size = 1, show.legend = TRUE) + # media movel com ordem 3
     geom_ma(aes(colour = 'MM5'), ma_fun = SMA, n = 5, size = 1, show.legend = TRUE) + # media movel com ordem 5
     geom_ma(aes(colour = 'MM12'), ma_fun = SMA, n = 12, size = 1, show.legend = TRUE) + # media movel com ordem 12
     labs(title = "Predição dos valores da Inflação", subtitle = "(média móvel simples)", x = "", y = "") +
     scale_colour_manual(name = "", values = c('MM3' = "red",
                                               'MM5' = "blue",
                                               'MM12' = "darkgreen"), labels = c("MMS(12)",
                                                                                 "MMS(3)",
                                                                                 "MMS(5)")) +
     scale_x_date(breaks = "year", date_labels = "%Y") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           legend.position = "bottom")
ggsave(filename = "Inflação_MédiaMovel.png", path = "~/GitHub/estatisticaR/TEP_ts", width = 20, height = 12, units = "cm")


inflacao = ts(emprego$inflacao, start = c(2005,1), end = c(2017, 12), frequency = 12)
inflacaoTreino = window(inflacao, 2005, c(2016, 12))
inflacaoTeste = window(inflacao, 2017, c(2017, 12))


knitr::kable(accuracy(forecast(ets(inflacao, "AAA")), h = 16, levels = c(85, 90)))
knitr::kable(accuracy(forecast(ets(inflacao, "ANA")), h = 16, levels = c(85, 90)))

autoplot(forecast(ets(inflacao), h = 16,levels = c(85, 90)))
autoplot(forecast(ets(inflacao, "AAA")), h = 16, levels = c(85, 90))
autoplot(forecast(ets(inflacao, "ANA")), h = 16, levels = c(85, 90))

autoplot(forecast(ma(inflacao, order = 12)), h = 16, levels = c(85, 90))

plot(inflacao)
lines(forecast(ets(inflacaoTreino, "AAA"), h=16, levels=c(85,90))$mean, col = "blue")
lines(forecast(ets(inflacaoTreino, "ANA"), h=16, levels=c(85,90))$mean, col = "red")
lines(pibTeste, col = "green")

knitr::kable(accuracy(forecast(ets(inflacaoTreino, "AAA"), h = 24), inflacaoTeste))
knitr::kable(accuracy(forecast(ets(inflacaoTreino, "ANA"), h = 24), inflacaoTeste))



### Taxa de juros
# Série
emprego %>% 
     mutate(ano = year(mes)) %>% 
     group_by(mes) %>% 
     mutate(taxaJurosMes = mean(taxaJuros)) %>% 
     group_by(ano) %>% 
     mutate(taxaJurosAno = mean(taxaJuros)) %>% 
     ggplot(aes(x = mes)) +
     geom_line(aes(y = taxaJurosMes)) + 
     geom_point(aes(y = taxaJurosMes)) +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Taxa de Juros", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


taxaJuros = ts(emprego$taxaJuros, start = c(2005,1), end = c(2017, 12), frequency = 12)
taxaJurosTreino = window(taxaJuros, 2005, c(2016, 12))
taxaJurosTeste = window(taxaJuros, 2017, c(2017, 12))

autoplot(forecast(ets(taxaJuros), h = 16,levels = c(85, 90)))
autoplot(forecast(ets(taxaJuros, "AAA")), h = 16, levels = c(85, 90))
autoplot(forecast(ets(taxaJuros, "MAM")), h = 16, levels = c(85, 90))

autoplot(forecast(ma(taxaJuros, order = 12)), h = 16, levels = c(85, 90))

knitr::kable(accuracy(forecast(ets(taxaJuros, "AAA")), h = 16, levels = c(85, 90)))
knitr::kable(accuracy(forecast(ets(taxaJuros, "MAM")), h = 16, levels = c(85, 90)))


plot(taxaJuros)
lines(forecast(ets(taxaJurosTreino, "AAA"), h=16, levels = c(85, 90))$mean, col = "blue")
lines(forecast(ets(taxaJurosTreino, "MAM"), h=16, levels = c(85, 90))$mean, col = "red")
lines(taxaJurosTeste, col = "green")


knitr::kable(accuracy(forecast(ets(taxaJurosTreino, "AAA"), h = 24), taxaJurosTeste))
knitr::kable(accuracy(forecast(ets(taxaJurosTreino, "MAM"), h = 24), taxaJurosTeste))




# Taxa de câmbio
emprego %>% 
     ggplot(aes(x = mes, y = taxaCambio)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Taxa de Câmbio", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


taxaCambio = ts(emprego$taxaCambio, start = c(2005,1), end = c(2017, 12), frequency = 12)
taxaCambioTreino = window(taxaCambio, 2005, c(2016, 12))
taxaCambioTeste = window(taxaCambio, 2017, c(2017, 12))

autoplot(forecast(ets(taxaCambio), h = 16,levels = c(85, 90)))
autoplot(forecast(ets(taxaCambio, "AAN")), h = 16, levels = c(85, 90))
autoplot(forecast(ets(taxaCambio, "ANN")), h = 16, levels = c(85, 90))

autoplot(forecast(ma(taxaCambio, order = 7)), h = 16, levels = c(85, 90))

knitr::kable(accuracy(forecast(ets(taxaCambio, "AAN")), h = 16, levels = c(85, 90)))
knitr::kable(accuracy(forecast(ets(taxaCambio, "ANN")), h = 16, levels = c(85, 90)))


knitr::kable(accuracy(forecast(ets(taxaCambio, "AAN")), h = 16, levels = c(85, 90)))
knitr::kable(accuracy(forecast(ets(taxaCambio, "ANN")), h = 16, levels = c(85, 90)))

plot(taxaCambio)
lines(forecast(ets(taxaCambioTreino, "AAN"), h=16, levels = c(85, 90))$mean, col = "blue")
lines(forecast(ets(taxaCambioTreino, "ANN"), h=16, levels = c(85, 90))$mean, col = "red")
lines(taxaCambioTeste, col = "green")


knitr::kable(accuracy(forecast(ets(taxaCambioTreino, "AAN"), h = 24), taxaCambioTeste))
knitr::kable(accuracy(forecast(ets(taxaCambioTreino, "ANN"), h = 24), taxaCambioTeste))



# Patentes
emprego %>% 
     ggplot(aes(x = mes, y = patentes)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Patentes", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))

autoplot(forecast(ets(ts(emprego$PIB, start = c(2005,1), end = c(2017, 12), frequency = 12), "ANN")), h=16,levels=c(85,90))



patentes = ts(emprego$patentes, start = c(2005,1), end = c(2017, 12), frequency = 12)
patentesTreino = window(patentes, 2005, c(2016, 12))
patentesTeste = window(patentes, 2017, c(2017, 12))

autoplot(forecast(ets(patentes), h = 16,levels = c(85, 90)))
autoplot(forecast(ets(patentes, "AAA")), h = 16, levels = c(85, 90))
autoplot(forecast(ets(patentes, "MAM")), h = 16, levels = c(85, 90))

autoplot(forecast(ma(patentes, order = 12)), h = 16, levels = c(85, 90))


knitr::kable(accuracy(forecast(ets(patentes, "AAA")), h = 16, levels = c(85, 90)))
knitr::kable(accuracy(forecast(ets(patentes, "MAM")), h = 16, levels = c(85, 90)))


plot(patentes)
lines(forecast(ets(patentesTreino, "AAA"), h=16, levels = c(85, 90))$mean, col = "blue")
lines(forecast(ets(patentesTreino, "MAM"), h=16, levels = c(85, 90))$mean, col = "red")
lines(patentesTeste, col = "green")


knitr::kable(accuracy(forecast(ets(patentesTreino, "AAA"), h = 24), patentesTeste))
knitr::kable(accuracy(forecast(ets(patentesTreino, "MAM"), h = 24), patentesTeste))



