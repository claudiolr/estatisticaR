library(tidyverse)
library(lubridate)
library(mfx)
library(caret)
library(pROC)
library(ResourceSelection)
library(modEvA)
library(foreign)
library(stargazer)
library(haven)


chd <- read_delim("https://github.com/Smolski/livroavancado/raw/master/cdh.csv", 
                  ";", escape_double = FALSE, col_types = cols(CHD = col_factor(levels = c())), 
                  trim_ws = TRUE)

summary(chd)


ggplot(chd, aes(x = AGE, y = CHD)) + 
     geom_point() + 
     stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)


m1 = glm(CHD ~ AGE, family = binomial(link="logit"), data = chd)
summary(m1)


# Criando campo de predição para cada idade dos indivíduos 
chd$PRED = predict(m1, type = "response")

# Plotando a probabilidade predita pelo modelo
ggplot(chd, aes(x=AGE, y=PRED)) + 
     geom_point()


logitor(CHD ~ AGE, data = chd)


exp(cbind(OR=coef(m1), confint(m1)))


chd$pdata <- as.factor(
     ifelse(
          predict(m1, 
                  newdata = chd, 
                  type = "response")
          >0.5,"1","0"))


confusionMatrix(chd$pdata, chd$CHD, positive="1")

roc1=plot.roc(chd$CHD,fitted(m1))

plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)

RsqGLM(m1)




mydata <- read_dta("http://dss.princeton.edu/training/Panel101.dta") 





logit = glm(y_bin ~ x1 + x2 + x3, data = mydata, family = binomial(link="logit"))
summary(logit)



step(logit, direction = 'both')




binary <- read_csv("http://www.karlin.mff.cuni.cz/~pesta/prednasky/NMFM404/Data/binary.csv")

binary$rank <- factor(binary$rank)


mylogit <- glm(admit ~ gre + gpa + rank, data = binary, 
               family = binomial(link="logit"))
summary(mylogit)

exp(cbind(OR = coef(mylogit), confint(mylogit)))


pred=data.frame(gre=700,
                gpa=3.67,
                rank=factor(1)
)

pred$prob = predict(mylogit, newdata=pred, type="response")
pred



# Criação da tabela
novosdados=with(binary,
                data.frame(gre=mean(gre),
                           gpa=mean(gpa),
                           rank=factor(1:4)))

# Incluindo a predição dos valores
novosdados=cbind(novosdados,predict(mylogit, 
                                    newdata=novosdados,
                                    type="response",
                                    se.fit=TRUE))
# Renomeando as variáveis
names(novosdados)[names(novosdados)=='fit']="prob"
names(novosdados)[names(novosdados)=='se.fit']="se.prob"

# Estimando os intervalos de confiança

novosdados$LL=novosdados$prob-1.96*novosdados$se.prob
novosdados$UL=novosdados$prob+1.96*novosdados$se.prob

# Vizualização dos dados
novosdados


ggplot(novosdados, aes(x=rank,y=prob))+
     geom_errorbar(aes(ymin=LL, ymax=UL), width=0.2,lty=1,lwd=1,col="red")+
     geom_point(shape=18, size=5, fill="black")+
     scale_x_discrete(limits=c("1","2","3","4"))+
     labs(title="Probabilidades preditas", x="Ranking",y="Pr(y=1)")


