# Inadimplência -----------------------------------------------------------
setwd("C:/Users/1247955/OneDrive/CienciaDados/EstatisticaPredicao")

inadimplencia <- read_xls("Inadimplencia_BD.xls")

modelo8 <- glm(inadimplencia$Inadimplente ~ inadimplencia$Renda + 
                    inadimplencia$Dependente + inadimplencia$Vinculo, family = binomial(link="logit"))
modelo8
summary(modelo8)
modelo8$coefficients
odds <- exp(2.8220971)
odds


modelo9 <- glm(inadimplencia$Inadimplente ~ inadimplencia$Renda + 
                    inadimplencia$Vinculo, family = binomial(link="logit"))
modelo9
summary(modelo9)
modelo9$coefficients
odds <- exp(2.8220971)
odds
