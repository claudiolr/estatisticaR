require(tidyverse)
require(RColorBrewer)
require(openxlsx)

options(scipen = 999)

setwd("C:/Users/Claudio/OneDrive/CienciaDados/EstatisticaPredicao/Bases")
cardio <- read.xlsx("DoencaCardiacaRegLog.xlsx")
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
cardio$stress <- as.factor(cardio$stress)
cardio$doencaCardiaca <- as.factor(cardio$doencaCardiaca)


### II: realize uma análise descritiva de cada variável
# Idade
ggplot(cardio, aes(x = idade)) +
     geom_histogram(breaks = seq(min(cardio$idade), max(cardio$idade), by = 2),
                    col = "white",
                    alpha = 0.8) + 
     labs(title = "Histograma de idade",
          x = "", y = "") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
           legend.text = element_text(size = 11),
           axis.text.x = element_text(size = 11, vjust = 0),axis.title = element_text(hjust = 0.5))

summary(cardio$idade)


# Sexo
cardio %>% 
     group_by(sexo) %>% summarise(Total = n()) %>% 
     mutate("%" = round(Total/sum(Total), digits = 3)*100) %>% 
     ggplot(aes(x = sexo, y = Total, fill = sexo, label = paste0(sexo,"\n", Total," (", `%`, "%)"))) +
     geom_col() + 
     labs(title = "Sexo",
          x = "", y = "") + 
     geom_label(aes(y = Total), colour = "white", fontface = "bold", position = position_stack(vjust = 0.90)) +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17),
           legend.text = element_text(size = 11), axis.text.x = element_blank(),
           axis.ticks = element_blank(), legend.position = "none")

summary(cardio$sexo)


# IMC
ggplot(cardio, aes(x = IMC)) +
     geom_histogram(breaks = seq(16, 45, by=2),
                    col = "white",
                    alpha = 1) + 
     labs(title = "Histograma de IMC",
          x = "", y = "") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           legend.text = element_text(size = 11), axis.text.x = element_text(size = 11),
           axis.ticks = element_blank())

summary(cardio$IMC)


# Circunferência da cintura
ggplot(cardio, aes(x = ccintura)) +
     geom_histogram(breaks = seq(48, 126, by=4),
                    col = "white",
                    alpha = 1) + 
     labs(title = "Histograma de cincunferência da cintura",
          x = "Cincunferência da cintura", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text = element_text(size = 11), axis.title.y = element_text(size = 13),
           axis.ticks = element_blank())

summary(cardio$ccintura)


# Cincunferência do quadril
ggplot(cardio, aes(x = cquadril)) +
     geom_histogram(breaks = seq(70, 132, by=4),
                    col = "white",
                    alpha = 1) + 
     labs(title = "Histograma de cincunferência do quadril",
          x = "Cincunferência do quadril", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text = element_text(size = 11), axis.title = element_text(size = 13),
           axis.ticks = element_blank())

summary(cardio$cquadril)


# Frequência cardíaca
ggplot(cardio, aes(x = freqCardiaca)) +
     geom_histogram(breaks = seq(47, 122, by=4),
                    col = "white",
                    alpha = 1) + 
     labs(title = "Histograma de frequência cardíaca",
          x = "Frequência cardíaca", y = "Frequência") + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
           axis.text = element_text(size = 11), axis.title = element_text(size = 13),
           axis.ticks = element_blank())

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
modelo1 <- glm(doencaCardiaca ~ idade + sexo + IMC + ccintura + cquadril + freqCardiaca + fumo + ativFisica +
                    stress + pDiastolica, data = cardio, family = binomial(link = "logit"))
summary(modelo1)
anova(modelo1, test="Chisq")


plot(fitted(modelo1),
     rstandard(modelo1))



# Modelo 2 (com as variáveis significativas)
modelo2 <- glm(cardio$doencaCardiaca ~ cardio$sexo + cardio$stress + cardio$pDiastolica, family = binomial(link = "logit"), model = TRUE)
summary(modelo2)

cardio$ajustes <- modelo2[["fitted.values"]]
exp(coef(modelo2))

cardio$ajustesRecod1 <- ifelse(cardio$ajustes < 0.5, 0, 1)
knitr::kable(addmargins(table(cardio$ajustesRecod1, cardio$doencaCardiaca)))


cardio$ajustesRecod2 <- ifelse(cardio$ajustes < 0.6, 0, 1)
knitr::kable(addmargins(table(cardio$ajustesRecod2, cardio$doencaCardiaca)))


cardio$ajustesRecod3 <- ifelse(cardio$ajustes < 0.7, 0, 1)
knitr::kable(addmargins(table(cardio$ajustesRecod3, cardio$doencaCardiaca)))





# Parte II - séries temporais ---------------------------------------------
setwd("C:/Users/Claudio/OneDrive/CienciaDados/EstatisticaPredicao/Bases")
emprego <- read.xlsx("EmpregoFormalSeriesTemporais.xlsx", detectDates = TRUE)

colnames(emprego) = c("mes", "PIB", "inflacao", "taxaJuros", "taxaCambio", "patentes", "saldoEmprego")


emprego$saldoEmprego <- as.factor(emprego$saldoEmprego)



# Modelo de regressão total

modeloEmprego1 <- glm(saldoEmprego ~ PIB + inflacao + taxaJuros + taxaCambio + patentes, data = emprego, family = binomial(link = logit))
summary(modeloEmprego1)
