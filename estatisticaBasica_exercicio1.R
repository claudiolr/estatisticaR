library(readxl)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Claudio/OneDrive/CienciaDados/EstatisticaBasica")
azulejos <- read_excel("Azulejo_CDBD.xlsx", sheet = NULL)

class(azulejos$Espessura)
class(azulejos$Turma)

summary(azulejos$Espessura)

azulejos$EspessuraRecod <- ifelse(azulejos$Espessura>2.0 & azulejos$Espessura<= 2.5, 2.5,
                                  ifelse(azulejos$Espessura>2.5 & azulejos$Espessura<= 3.0, 3,
                                         ifelse(azulejos$Espessura>3.0 & azulejos$Espessura<= 3.5, 3.5,
                                                ifelse(azulejos$Espessura>3.5 & azulejos$Espessura<=4.0, 4,
                                                       ifelse(azulejos$Espessura>4.0 & azulejos$Espessura<=4.5, 4.5,
                                                              ifelse(azulejos$Espessura>4.5 & azulejos$Espessura<=5.0, 5,
                                                                     ifelse(azulejos$Espessura>5.0 & azulejos$Espessura<=5.5, 5.5,
                                                                            ifelse(azulejos$Espessura>5.5 & azulejos$Espessura<=6, 6,
                                                                                   ifelse(azulejos$Espessura>6.0 & azulejos$Espessura<=6.5, 6.5, 7)))))))))

ggplot(azulejos, aes(Espessura)) + 
     geom_histogram(show.legend = TRUE, colour="white", aes(y = ..count..), breaks=seq(2.0, 7.0, by=0.5)) +
     labs(x = "Espessura", y = "quantidade") +
     ggtitle("Histograma de espessuras")

azulejos %>% filter(Turma=="A") %>% 
     ggplot(aes(Espessura)) + 
     geom_histogram(col="white", fill="darkred", breaks=seq(2.0, 7.0, by=0.5)) +
     labs(x="espessura", y="quantidade") + 
     ggtitle("Espessuras dos azulejos da Turma A")

azulejos %>% filter(Turma=="B") %>% 
     ggplot(aes(Espessura)) + 
     geom_histogram(bins = 10, col="white", fill="darkgreen", breaks=seq(2.0, 7.0, by=0.5)) +
     labs(x="espessura", y="quantidade") + 
     ggtitle("Espessuras dos azulejos da Turma B")

count(azulejos$Espessura > 6.5 | azulejos$Espessura <3.5)
count(azulejos$Espessura < 3.5)
count(azulejos$Espessura > 6.5)

count((azulejos$Espessura > 6.5 | azulejos$Espessura <3.5) & azulejos$Turma == "A")
count((azulejos$Espessura > 6.5 | azulejos$Espessura <3.5) & azulejos$Turma == "B")

table(azulejos$EspessuraRecod, azulejos$Turma)
summary(azulejos$Espessura)

azulejos %>% filter(azulejos$Turma=="A") %>% summarize(média=mean(Espessura, na.rm = TRUE),
                                                       mediana=median(Espessura, na.rm = TRUE),
                                                       desvioPadrao=sd(Espessura, na.rm = TRUE))

azulejos %>% filter(azulejos$Turma=="B") %>% summarize(média=mean(Espessura, na.rm = TRUE),
                                                       mediana=median(Espessura, na.rm = TRUE),
                                                       desvioPadrao=sd(Espessura, na.rm = TRUE))

cv = 100*sd(azulejos$Espessura, na.rm = TRUE)/mean(azulejos$Espessura, na.rm = TRUE)
subset(table(azulejos$TurmaA), table(azulejos$TurmaA)==max(table(azulejos$TurmaA)))
subset(table(azulejos$TurmaB), table(azulejos$TurmaB)==max(table(azulejos$TurmaB)))


airbnb <- read_excel("Dados airbnb_CDBD.xlsx")
colnames(airbnb) = c("id","idHospedagem","nomeHospedagem","idHospede","nomeHospede","grupoVizinhanca","latitude",
                     "longitude","tipoHospedagem","preco","locacaoMinima","numeroAvaliacoes","taxaMensalOCupacao",
                     "numeroMaximoHospedes","disponibilidadeAnual")

airbnb %>% summarize(média = mean(preco),
                     mediana = median(preco),
                     desvioPadrão = sd(preco),
                     mínimo=min(preco),
                     máximo=max(preco))
cv = 100*sd(airbnb$preco, na.rm = TRUE)/mean(airbnb$preco, na.rm = TRUE)


ggplot(airbnb, aes(preco)) + 
     geom_histogram(show.legend = FALSE, bins = 15, colour="white", fill="black", breaks=seq(50, 149, 5)) +
     scale_fill_discrete(h = c(0, 280), c=10, l=10) +
     labs(x = "Preço", y = "quantidade") +
     ggtitle("Histograma de preços")

airbnb %>% summarize(média = mean(taxaMensalOCupacao),
                     mediana = median(taxaMensalOCupacao),
                     desvioPadrão = sd(taxaMensalOCupacao),
                     mínimo=min(taxaMensalOCupacao),
                     máximo=max(taxaMensalOCupacao))
cv = 100*sd(airbnb$taxaMensalOCupacao, na.rm = TRUE)/mean(airbnb$taxaMensalOCupacao, na.rm = TRUE)


ggplot(airbnb, aes(taxaMensalOCupacao)) + 
     geom_histogram(show.legend = FALSE, bins = 10, fill="darkblue", colour="white", breaks=seq(0.0, 1, 0.05)) +
     scale_fill_discrete(h = c(0, 280), c=10, l=10) +
     labs(x = "Taxa", y = "quantidade") +
     ggtitle("Taxa mensal de ocupação")

airbnb %>% summarize(média = mean(locacaoMinima),
                     mediana = median(locacaoMinima),
                     desvioPadrão = sd(locacaoMinima),
                     mínimo=min(locacaoMinima),
                     máximo=max(locacaoMinima))
cv = 100*sd(airbnb$locacaoMinima, na.rm = TRUE)/mean(airbnb$locacaoMinima, na.rm = TRUE)


ggplot(airbnb, aes(locacaoMinima)) + 
     geom_histogram(show.legend = FALSE, bins = 10, fill="darkgreen", colour="white", breaks=seq(1, 90, 3)) +
     scale_fill_discrete(h = c(0, 280), c=10, l=10) +
     labs(x = "Locação mínima", y = "quantidade") +
     ggtitle("Locação mínima")


airbnb %>% summarize(média = mean(numeroAvaliacoes),
                     mediana = median(numeroAvaliacoes),
                     desvioPadrão = sd(numeroAvaliacoes),
                     mínimo=min(numeroAvaliacoes),
                     máximo=max(numeroAvaliacoes))
cv = 100*sd(airbnb$numeroAvaliacoes, na.rm = TRUE)/mean(airbnb$numeroAvaliacoes, na.rm = TRUE)

ggplot(airbnb, aes(numeroAvaliacoes)) + 
     geom_histogram(show.legend = FALSE, bins = 10, fill="darkgray", colour="white", breaks=seq(1, 100, 5)) +
     scale_fill_discrete(h = c(0, 280), c=10, l=10) +
     labs(x = "Avaliações", y = "quantidade") +
     ggtitle("Número de avaliações")


airbnb %>% summarize(média = mean(numeroMaximoHospedes),
                     mediana = median(numeroMaximoHospedes),
                     desvioPadrão = sd(numeroMaximoHospedes),
                     mínimo=min(numeroMaximoHospedes),
                     máximo=max(numeroMaximoHospedes))
cv = 100*sd(airbnb$numeroMaximoHospedes, na.rm = TRUE)/mean(airbnb$numeroMaximoHospedes, na.rm = TRUE)

ggplot(airbnb, aes(numeroMaximoHospedes)) + 
     geom_histogram(show.legend = FALSE, bins = 10, fill="black", colour="white", breaks=seq(1, 30, 1)) +
     scale_fill_discrete(h = c(0, 280), c=10, l=10) +
     labs(x = "Número de hóspedes", y = "quantidade") +
     ggtitle("Numero máximo de Hóspedes")


airbnb %>% summarize(média = mean(disponibilidadeAnual),
                     mediana = median(disponibilidadeAnual),
                     desvioPadrão = sd(disponibilidadeAnual),
                     mínimo=min(disponibilidadeAnual),
                     máximo=max(disponibilidadeAnual))
cv = 100*sd(airbnb$disponibilidadeAnual, na.rm = TRUE)/mean(airbnb$disponibilidadeAnual, na.rm = TRUE)

ggplot(airbnb, aes(disponibilidadeAnual)) + 
     geom_histogram(show.legend = FALSE, bins = 10, fill="darkred", colour="white", breaks=seq(10, 365, 10)) +
     scale_fill_discrete(h = c(0, 280), c=10, l=10) +
     labs(x = "Dias", y = "quantidade") +
     ggtitle("Disponibilidade anual")


ggplot(airbnb, aes(grupoVizinhanca, taxaMensalOCupacao)) + 
     geom_point()


airbnb %>% filter(grupoVizinhanca=="Brooklyn") %>% summarize(média=mean(taxaMensalOCupacao),
                                                             mediana=median(taxaMensalOCupacao),
                                                             desvioPadrão=sd(taxaMensalOCupacao))

airbnb %>% filter(grupoVizinhanca=="Manhattan") %>% summarize(média=mean(taxaMensalOCupacao),
                                                              mediana=median(taxaMensalOCupacao),
                                                              desvioPadrão=sd(taxaMensalOCupacao))


airbnb %>% filter(tipoHospedagem=="Ap/Casa inteira") %>% summarize(média=mean(taxaMensalOCupacao),
                                                                   mediana=median(taxaMensalOCupacao),
                                                                   desvioPadrão=sd(taxaMensalOCupacao))

airbnb %>% filter(tipoHospedagem=="Quarto Privativo") %>% summarize(média=mean(taxaMensalOCupacao),
                                                                    mediana=median(taxaMensalOCupacao),
                                                                    desvioPadrão=sd(taxaMensalOCupacao))



ggplot(airbnb, aes(preco, taxaMensalOCupacao)) + 
     geom_point() +
     labs(x = "Preço", y= "Taxa mensal de ocupação") +
     geom_smooth(method = "lm") +
     ggtitle("Taxa de ocupação x Preço")

ggplot(airbnb, aes(locacaoMinima, taxaMensalOCupacao)) + 
     geom_point() +
     labs(x = "Locação mínima", y = "Taxa mensal de ocupação") +
     geom_smooth(method = "lm") +
     ggtitle("Taxa de ocupação x Locação mínima")

ggplot(airbnb, aes(numeroAvaliacoes, taxaMensalOCupacao)) + 
     geom_point() +
     labs(x = "Número de avaliações", y = "Taxa mensal de ocupação") +
     geom_smooth(method = "lm") +
     ggtitle("Taxa de ocupação x Número de avaliações")


ggplot(airbnb, aes(numeroMaximoHospedes, taxaMensalOCupacao)) + 
     geom_point() +
     labs(x = "Número máximo de hóspedes", y = "Taxa mensal de ocupação") +
     geom_smooth(method = "lm") +
     ggtitle("Taxa de ocupação x Número máximo de hóspedes")


ggplot(airbnb, aes(disponibilidadeAnual, taxaMensalOCupacao)) + 
     geom_point() +
     labs(x = "Disponibilidade anual", y = "Taxa mensal de ocupação") +
     geom_smooth(method = "lm") +
     ggtitle("Taxa de ocupação x Disponibilidade anual")


