require(tidyverse)
require(RColorBrewer)
require(openxlsx)


setwd("C:/Users/Claudio/OneDrive/CienciaDados/EstatisticaPredicao/Bases")
titanic <- read.csv("Titanic.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ",")

titanic$sobrevivente <- as.factor(titanic$sobrevivente)
titanic$sexo <- as.factor(titanic$sexo)
titanic$classe <- as.factor(titanic$classe)



# Regressão logística simples ---------------------------------------------

modelo1 = glm(titanic$sobrevivente ~ titanic$sexo, family = binomial(link = "logit"))
summary(modelo1)
exp(modelo1[["coefficients"]][["titanic$sexomasculino"]])
exp(-2.477825)
# A chance de sobrevivência de homens é 92% menor que a chance de mulheres
# O inverso é igual a 1/0.08392557 = 11.9 (odds ratio): mulheres têm 11,9 vezes mais chance de sobrevivência que homens


modelo2 = glm(titanic$sobrevivente ~ titanic$idade, family = binomial(link = "logit"))
summary(modelo2)



modelo3 = glm(titanic$sobrevivente ~ titanic$classe, family = binomial(link = "logit"))
summary(modelo3)
# Passar da classe 1 para a classe 2 reduz a chance de sobrevivência em 72%



### Regressão logística  Multipla -------------------------------------------

# Modelo 5: Sexo, idade, irmãos, pais, tarifa e classe
modelo5 = glm(titanic$sobrevivente ~ titanic$sexo + titanic$idade +
                   titanic$irmaos +
                   titanic$pais +
                   titanic$tarifa +
                   titanic$classe, family = binomial(link="logit"))
summary(modelo5)


# Modelo 6: Sexo, idade
modelo6 = glm(titanic$sobrevivente ~ titanic$sexo +
                   titanic$idade, family = binomial(link="logit"))
summary(modelo6)


# Modelo 7: Sexo, idade, irmãos, pais, classe
modelo7 = glm(titanic$sobrevivente ~ titanic$sexo +
                   titanic$idade +
                   titanic$irmaos +
                   titanic$pais +
                   titanic$classe, family = binomial(link="logit"))
summary(modelo7)


# Modelo 8: Sexo, idade, irmãos, classe
modelo8 = glm(titanic$sobrevivente ~ titanic$sexo +
                   titanic$idade +
                   titanic$irmaos +
                   titanic$classe, family = binomial(link = "logit"))
summary(modelo8)
# Sexo: -2.628266. Exp(-2.628266) = 0.07220355: ser do sexo masculino reduz em 93% a chance de sobrevivência
# Idade: -0.044730. Exp(-0.044730) = 0.9562556: cada ano a mais de idade reduz em 5% a chance de sobrevivência
# Irmãos: -0.380261. Exp(-0.380261) = 0.6836829: cada pessoa a mais na família (filhos, irmãos ou cônjuges) reduz em 32% a chance de sobrevivência
# Classe 2: -1.414967. Exp(-1.414967) = 0.2429336: passar da classe 1 para a classe 2 reduz em 75% a chance de sobrevivência
# Classe 3: -2.652896. Exp(-2.652896) = 0.0704469: passar da classe 1 para a classe 3 reduz em 92,9% a chance de sobrevivência
# Uma pessoa que viajou na classe 2, tem 30 anos de idade, é do sexo feminino e viajou com 2 irmãos:
# Y = 4.333074 -2.628266*(0) - 0.044730*(30) - 0.380261*(2) - 1.414967*(1) - 2.652896*(0)
# Y = -0.815685.
# Probabilidade = exp(0.815685)/1 + exp(0.815685) = 0.6933196 (69%) é a probabilidade de essa pessoa sobreviver.

# A seguir, os valores ajustados do modelo foram armazenados em um data frame
titanic$ajustes <- modelo8[["fitted.values"]]

# Valores ajustados são recodificados para 0 e 1 em corte de 50%
titanic$ajustesRecod1 <- ifelse(titanic$ajustes < 0.5, 0, 1)

# Tabela cruzada: na linha, os valores ajustados recodificados, e na coluna os valores reais
table(titanic$ajustesRecod1, titanic$sobrevivente)
round(prop.table(table(titanic$ajustesRecod1, titanic$sobrevivente))*100, digits = 1)

# Valores ajustados são recodificados para 0 e 1 em corte de 60%
titanic$ajustesRecod2 <- ifelse(titanic$ajustes < 0.35, 0, 1)
# Na linha, os valores ajustados, e na coluna os valores reais
table(titanic$ajustesRecod2, titanic$sobrevivente)
round(prop.table(table(titanic$ajustesRecod2, titanic$sobrevivente))*100, digits = 1)