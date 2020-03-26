library(tidyverse)
library(RColorBrewer)
library(MASS)
library(foreign)


# Informarções do pacote RColorBrewer -------------------------------------
RColorBrewer::display.brewer.all()
brewer.pal.info


# Escala Likert -----------------------------------------------------------
legConcordancia <- c("Discorda muito","Discorda",
                     paste("Não discorda","\n","nem concorda"),
                     "Concorda","Concorda muito")
legFrequencia <- c("Nunca","Quase nunca","Às vezes","Quase sempre","Sempre")


# Tratamento da base ------------------------------------------------------
setwd("C:/Users/Claudio/OneDrive/CienciaDados")
base <- read.csv("wiki4HE.csv", sep=";")


### Substitui não-respostas ("?") por NA
base$DOMAIN <- factor(base$DOMAIN, levels = c("1","2","3","4","5","6"))
base$USERWIKI <- factor(base$USERWIKI, levels = c("0", "1"))
base$ENJ1 <- factor(base$ENJ1, levels = c("1","2","3","4","5"))
base$ENJ2 <- factor(base$ENJ2, levels = c("1","2","3","4","5"))



### Transforma variáveis categóricas (factor) em numéricas
base$YEARSEXP <- as.numeric(base$YEARSEXP)


# Transforma variável dependente em binária -------------------------------
base$ENJ1Recod <- as.factor(ifelse(is.na(base$ENJ1), NA,
                                   ifelse(base$ENJ1 %in% (1:3), 0, 1)))

base$ENJ2Recod <- as.factor(ifelse(is.na(base$ENJ2), NA,
                                   ifelse(base$ENJ2 %in% (1:3), 0, 1)))





# Análises ----------------------------------------------------------------

summary(base$AGE)
summary(base$ENJ1)
summary(base$ENJ2)

cor(base$USERWIKI, base$ENJ1, use = "na.or.complete")
cor(base$USERWIKI, base$ENJ2, use = "na.or.complete")
cor(base$USERWIKI, base$ENJ, use = "na.or.complete")

summary(lm(ENJ ~ USERWIKI, data = base))


modelo1 = glm(base$ENJ1Recod ~ base$AGE + base$GENDER + base$DOMAIN + base$PhD + base$YEARSEXP + base$UNIVERSITY + base$UOC_POSITION,
              family = binomial(link = "logit"))
summary(modelo1)


modelo2 = glm(base$ENJ1Recod ~ base$AGE + base$DOMAIN, family = binomial(link = "logit"))
summary(modelo2)
# -0.030620. Exp = 0.969844: cada ano a mais de idade do professor reduz em 3% a chance de ele concordar com a variável ENJ1
# 1.037115. Exp = 2.821066: ser da área de ciências aumenta 182% a chance de o professor concordar com a pergunta ENJ1
# y = 1.990016 - 0.030620 + 1.037115(DOMAIN==2)
# Uma pessoa de Ciências com 45 anos de idade:
# y = 1.990016 - 0.030620(45) + 1.037115 = 1.649231
# Probabilidade = exp(1.649231)/1 + exp(1.649231) = 0.8387871
# Essa pessoa tem 83% de chance de concordar com a variável ENJ1