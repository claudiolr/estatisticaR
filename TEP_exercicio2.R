library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
library(lubridate)

options(scipen = 1)


# Parte I - cardio --------------------------------------------------------

setwd("~/GitHub/estatisticaR")
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

cardio$sexo <- as.factor(cardio$sexo)
cardio$fumo <- as.factor(cardio$fumo)

cardio <- cardio %>% 
        arrange(stress) %>% 
        mutate(stress = factor(stress))

cardio$doencaCardiaca <- as.factor(cardio$doencaCardiaca)



### II: realize uma análise descritiva de cada variável
# Idade
gridExtra::grid.arrange(
        ggplot(cardio, aes(x = idade)) +
                geom_histogram(aes(y = ..density..), breaks = seq(min(cardio$idade), max(cardio$idade), by = 2),
                               col = "white",
                               alpha = 0.8) + 
                stat_function(fun = dnorm, args = list(mean = mean(cardio$idade), sd = sd(cardio$idade))) +
                labs(title = "Histograma de idade",
                     x = "", y = "") + 
                theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 16),
                      legend.text = element_text(size = 11),
                      axis.text.x = element_text(size = 11, vjust = 0),axis.title = element_text(hjust = 0.5))
        ,
        ggplot(cardio, aes(x = "")) +
                geom_boxplot(aes(y = idade)) +
                labs(title = "Boxplot de idade", x = "", y = "") + 
                theme(plot.title = element_text(size = 16, hjust = 0.5),
                      axis.ticks.x = element_blank())
        , ncol = 2)
summary(cardio$idade)


# Sexo
cardio %>% 
        group_by(sexo) %>% summarise(Total = n()) %>% 
        mutate("%" = round(Total/sum(Total), digits = 3)*100) %>% 
        ggplot(aes(x = sexo, y = Total, fill = sexo, label = paste0(sexo,"\n", Total," (", `%`, "%)"))) +
        geom_col() + 
        labs(title = "Sexo",
             x = "", y = "") + 
        geom_label(aes(y = Total), colour = "white", fontface = "bold", position = position_stack(vjust = 0.50)) +
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
              legend.text = element_text(size = 11), axis.text.x = element_blank(),
              axis.ticks = element_blank(), legend.position = "none")

summary(cardio$sexo)


# IMC
gridExtra::grid.arrange(
ggplot(cardio, aes(x = IMC)) +
        geom_histogram(aes(y = ..density..), breaks = seq(16, 45, by=2),
                       col = "white",
                       alpha = 1) + 
        stat_function(fun = dnorm, args = list(mean = mean(cardio$idade), sd = sd(cardio$idade))) +
        labs(title = "Histograma de IMC",
             x = "", y = "") + 
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 16),
              legend.text = element_text(size = 11), axis.text.x = element_text(size = 11),
              axis.ticks = element_blank())
,
ggplot(cardio, aes(x = "")) +
        geom_boxplot(aes(y = IMC)) +
        labs(title = "Boxplot de IMC", x = "", y = "") + 
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              axis.ticks.x = element_blank())
, ncol = 2)


summary(cardio$IMC)


# Circunferência da cintura
gridExtra::grid.arrange(
ggplot(cardio, aes(x = ccintura)) +
        geom_histogram(breaks = seq(48, 126, by=4),
                       col = "white",
                       alpha = 1) + 
        labs(title = "Histograma de cincunferência da cintura",
             x = "Cincunferência da cintura", y = "Frequência") + 
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 16),
              axis.text = element_text(size = 11), axis.title.y = element_text(size = 13),
              axis.ticks = element_blank())
,
ggplot(cardio, aes(x = "")) +
        geom_boxplot(aes(y = ccintura)) +
        labs(title = "Boxplot de cincunferência da cintura", x = "", y = "") + 
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              axis.ticks.x = element_blank())
, ncol = 2)

summary(cardio$ccintura)


# Cincunferência do quadril
gridExtra::grid.arrange(
ggplot(cardio, aes(x = cquadril)) +
        geom_histogram(breaks = seq(70, 132, by=4),
                       col = "white",
                       alpha = 1) + 
        labs(title = "Histograma de cincunferência do quadril",
             x = "Cincunferência do quadril", y = "Frequência") + 
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
              axis.text = element_text(size = 11), axis.title = element_text(size = 13),
              axis.ticks = element_blank())
,
ggplot(cardio, aes(x = "")) +
        geom_boxplot(aes(y = cquadril)) +
        labs(title = "Boxplot de cincunferência do quadril", x = "", y = "") + 
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              axis.ticks.x = element_blank())
, ncol = 2)

summary(cardio$cquadril)


# Frequência cardíaca
gridExtra::grid.arrange(
ggplot(cardio, aes(x = freqCardiaca)) +
        geom_histogram(breaks = seq(47, 122, by=4),
                       col = "white",
                       alpha = 1) + 
        labs(title = "Histograma de frequência cardíaca",
             x = "Frequência cardíaca", y = "Frequência") + 
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 16),
              axis.text = element_text(size = 11), axis.title = element_text(size = 13),
              axis.ticks = element_blank())
,
ggplot(cardio, aes(x = "")) +
        geom_boxplot(aes(y = freqCardiaca)) +
        labs(title = "Boxplot de frequência cardíaca", x = "", y = "") + 
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              axis.ticks.x = element_blank())
, ncol = 2)

summary(cardio$frqCardiaca)


# Atividade física
cardio %>% 
        group_by(ativFisica) %>% 
        summarise(Total = n()) %>% 
        mutate("%" = round(Total/sum(Total), digits = 3)*100) %>% 
        ggplot(aes(x = ativFisica, y = Total, fill = ativFisica, label = paste0(`%`,"%"))) +
        geom_col() + 
        labs(title = "Atividade física",
             x = "Dias por semana", y = "Frequência") + 
        geom_label(aes(y = `%`), colour = "white", fontface = "bold", position = position_stack(vjust = 2)) +
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
              legend.text = element_text(size = 11), axis.text.x = element_text(size = 11),
              axis.ticks = element_blank(), legend.position = "none")

summary(cardio$ativFisica)


# Fumo
cardio %>% 
        group_by(fumo) %>% 
        summarise(Total = n()) %>% 
        mutate("%" = round(Total/sum(Total), digits = 3)*100,
               fumo = ifelse(fumo == 0, "Não fuma", "Fuma")) %>% 
        ggplot(aes(x = as.factor(fumo), y = Total, fill = fumo, label = paste0(`%`,"%"))) +
        geom_col() + 
        labs(title = "Fumo",
             x = "", y = "Frequência") + 
        geom_label(aes(y = `%`), colour = "white", fontface = "bold", position = position_stack(vjust = 0.5)) +
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
              legend.text = element_text(size = 11), axis.text.x = element_text(size = 11),
              legend.position = "none")


# Stress
cardio %>% 
        group_by(stress) %>% 
        summarise(Total = n()) %>% 
        mutate("%" = round(Total/sum(Total), digits = 3)*100) %>% 
        ggplot(aes(x = as.factor(stress), y = Total, fill = stress, label = paste0(`%`,"%"))) +
        geom_col() + 
        labs(title = "Níveis de stress",
             x = "", y = "Frequência") + 
        geom_label(aes(y = `%`), label.padding = unit(0.2, "lines"), label.r = unit(0.15, "lines"),
                   colour = "white", fontface = "bold", position = position_stack(vjust = 2)) +
        theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
              legend.text = element_text(size = 11), axis.text.x = element_text(size = 11),
              axis.ticks = element_blank(), legend.position = "none")


knitr::kable(
        addmargins(table(cardio$sexo, cardio$doencaCardiaca)
        ))



# Modelo 1 (todas as variáveis)
glm(doencaCardiaca ~ idade + sexo + IMC + ccintura + cquadril + freqCardiaca + fumo + ativFisica +
            stress + pDiastolica, data = cardio, family = binomial(link = "logit"))

summary(glm(doencaCardiaca ~ idade + sexo + IMC + ccintura + cquadril + freqCardiaca + fumo + ativFisica +
                    stress + pDiastolica, data = cardio, family = binomial(link = "logit")))

1 - pchisq(542.90 - 289.08, 399-386)

# Modelo 2 (com as variáveis significativas)
glm(cardio$doencaCardiaca ~ cardio$sexo + cardio$stress + cardio$pDiastolica, family = binomial(link = "logit"), model = TRUE)
summary(glm(cardio$doencaCardiaca ~ cardio$sexo + cardio$stress + cardio$pDiastolica, family = binomial(link = "logit"), model = TRUE))
exp(coef(glm(cardio$doencaCardiaca ~ cardio$sexo + cardio$stress + cardio$pDiastolica, family = binomial(link = "logit"), model = TRUE)))


table(cardio$stress, cardio$doencaCardiaca)

cardio$ajustes <- glm(cardio$doencaCardiaca ~ cardio$sexo + cardio$stress + cardio$pDiastolica, family = binomial(link = "logit"), model = TRUE)[["fitted.values"]]


cardio$ajustesRecod1 <- ifelse(cardio$ajustes < 0.5, 0, 1)
knitr::kable(addmargins(table(cardio$ajustesRecod1, cardio$doencaCardiaca)))


cardio$ajustesRecod2 <- ifelse(cardio$ajustes < 0.6, 0, 1)
knitr::kable(addmargins(table(cardio$ajustesRecod2, cardio$doencaCardiaca)))


cardio$ajustesRecod3 <- ifelse(cardio$ajustes < 0.7, 0, 1)
knitr::kable(addmargins(table(cardio$ajustesRecod3, cardio$doencaCardiaca)))





# Parte II - séries temporais ---------------------------------------------
setwd("~/GitHub/estatisticaR")
emprego <- read.csv("EmpregoFormalIndicadoresEconomicos.csv", sep = ";", header = TRUE, dec = ",", stringsAsFactors = FALSE)

colnames(emprego) = c("mes", "PIB", "inflacao", "taxaJuros", "taxaCambio", "patentes", "saldoEmprego")
emprego$saldoEmprego <- as.factor(emprego$saldoEmprego)

emprego$mes <- paste0("01-",emprego$mes)
emprego$mes <- as.Date(emprego$mes, tryFormats = "%d-%b-%y")


# Análise das variáveis

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



# Regressão
glm(saldoEmprego ~ PIB + inflacao + taxaJuros + taxaCambio + patentes, data = emprego, family = binomial(link = logit))
summary(glm(saldoEmprego ~ PIB + inflacao + taxaJuros + taxaCambio + patentes, data = emprego, family = binomial(link = logit)))
coef(modelo1)

1 - pchisq(203.68 - 143.83, 155-150)


modelo2 <- glm(saldoEmprego ~ taxaCambio + patentes, data = emprego, family = binomial(link = logit))
summary(glm(saldoEmprego ~ taxaCambio + patentes, data = emprego, family = binomial(link = logit)))
coef(glm(saldoEmprego ~ taxaCambio + patentes, data = emprego, family = binomial(link = logit)))
exp(coef(glm(saldoEmprego ~ taxaCambio + patentes, data = emprego, family = binomial(link = logit))))




##### Séries temporais para cada série de dados

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



