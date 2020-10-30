library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
library(lubridate)


options(scipen = 1)



# Carregamento e tratamento -----------------------------------------------
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



# II: realize uma análise descritiva de cada variável ---------------------
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



# Regressão ---------------------------------------------------------------

#### Modelo 1 (todas as variáveis)
modeloCardio1 <- glm(doencaCardiaca ~ idade + sexo + IMC + ccintura + cquadril + freqCardiaca + fumo + ativFisica +
                             stress + pDiastolica, data = cardio, family = binomial(link = "logit"))

modelsummary(modeloCardio1)


# Matriz de confusão (tabela de classificação)
cardio$ajuste1 <- predict(modeloCardio1, type = "response")
cardio$ajuste1_1 <- as.factor(ifelse(cardio$ajuste1 > 0.5, "1" , "0"))

confusionMatrix(cardio$ajuste1_1, cardio$doencaCardiaca, positive="1")

ggplot(cardio, aes(x = pDiastolica, y = ajuste1)) +
        geom_point() + 
        stat_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
        labs(title = "Distribuição de probabilidades pela pressão diastólica",
             x = "pressão diastólica", y = "valores ajustados") +
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))

ggplot(cardio, aes(x = IMC, y = ajuste1)) +
        geom_point() + 
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
        labs(title = "Distribuição de probabilidades pelo IMC",
             x = "IMC", y = "valores ajustados") +
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))

        

# Pseudo R2
RsqGLM(modeloCardio1)



#### Modelo 2 (com as variáveis significativas)
modeloCardio2 <- glm(doencaCardiaca ~ sexo + stress + pDiastolica, data = cardio,
                     family = binomial(link = "logit"), model = TRUE)
summary(modeloCardio2)
exp(coef(modeloCardio2))


cardio$ajuste2 <- predict(modeloCardio2, type = "response")
cardio$ajuste2_1 <- as.factor(ifelse(cardio$ajuste2 > 0.5, "1" , "0"))


confusionMatrix(cardio$ajuste2_1, cardio$doencaCardiaca, positive="1")


cardio$ajuste2_2 <- as.factor(ifelse(cardio$ajuste2 > 0.6, "1" , "0"))
confusionMatrix(cardio$ajuste2_2, cardio$doencaCardiaca, positive="1")




