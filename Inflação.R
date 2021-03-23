######### Prevendo inflação ##########

# Apagando variaveis antigas
rm(list = ls(all = T))

# Confirmando que o wd está limpo
ls(all.names = T)

# Definindo o wd
wd <- "C:/Users/Matheus/Documents/R/Macro"
setwd(wd)

# Confirma o working directory
ifelse(getwd() == wd, "OK", "Algo errado")

# Carregando diversos pacotes
load_package <- function(n){
  n <- as.character(match.call()[[2]])
  if (!require(n,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
  }
  require(n,character.only=TRUE)
}

load_package('openxlsx')
load_package('readxl')
load_package('xts')
load_package('class')
load_package('zoo')
load_package('fBasics')
load_package('qrmtools')
load_package('corrplot') 
load_package('tidyverse')
load_package('PerformanceAnalytics')
load_package('tseries')
load_package('ecoseries')
load_package('pdftools')
load_package('tm')
load_package('pdfsearch')
load_package('ggplot2')
load_package('forecast')
load_package('mFilter')
load_package('BETS')

# Vamos baixar a série do IPCA usando o pacote BETS 
# Indices de precos gerais (leve em consideracao preco ao produtor, por exemplo)
IGP_M <- as.zoo(BETSget(189))
IGP_DI <- as.zoo(BETSget(190))
INPC <- as.zoo(BETSget(188))

# Indices ao consumidor amplo (IPCA vai ate 40 sm)
IPCA_geral <- as.zoo(BETSget(433))
IPCA_core <- as.zoo(BETSget(4466))

# IPCA destrinchado (alguns desses nao vem o dado mais atualizado?????)
IPCA_alimentacao_bebidas <- as.zoo(BETSget(1635))
IPCA_habitacao <- as.zoo(BETSget(1636))
IPCA_artigos_residencia <- as.zoo(BETSget(1637))
IPCA_vestuario <- as.zoo(BETSget(1638))
IPCA_transportes <- as.zoo(BETSget(1639))
IPCA_comunicacao <- as.zoo(BETSget(1640))
IPCA_saude_cuidados_pessoais <- as.zoo(BETSget(1641))
IPCA_despesas_pessoais <- as.zoo(BETSget(1642))
IPCA_educacao <- as.zoo(BETSget(1643))

#Quem variou mais no ultimo mes?

m <- which.max()
## Vamos dar um merge nessas séries
# Queremos essas que começam com IPCA
startsWith(ls(),"IPCA_") == T

# Colocando em uma lista o que não é objeto wd e load_package
lista_de_variaveis <- mget(setdiff(ls(),c("load_package","wd","IGP_M","IGP_DI","INPC")))

# Verificando que só pegou as séries de interesse mesmo
for (i in 1:length(lista_de_variaveis)) {
ifelse(startsWith(names(lista_de_variaveis)[i],"IPCA_") == T, print("ok"),print("eita"))
}

# Vamos usar o merge em todas essas series
DF <- Reduce(merge,lista_de_variaveis)

# Nomeando as colunas
colnames(DF) <- names(lista_de_variaveis)

# Vendo o DF
View(DF)
str(DF)


# Vamos plotar o IPCA_Geral e uma curva suave
par(mfrow = c(1,1))
plot(IPCA_geral, main = "IPCA mensal - Série histórica", xlab = "Tempo"
     ,ylab = " Variação %", cex.lab=0.8, cex.axis=0.7, pch=3, cex=0.5, col = "blue")
smoothingSpline = smooth.spline(na.omit(IPCA_geral), spar=0.45)
lines(time(DF$IPCA_geral),smoothingSpline$y, col = "red")

# Notando que os valores antes do plano real (maio/jun 1994) são absurdos
# vamos olhar só a partir de 1995
IPCA_geral <- window(IPCA_geral,start = 1995)
IPCA_core <- window(IPCA_core,start = 1995)

m <- median(IPCA_geral)

# Plotando novamente
plot(IPCA_geral, main = "IPCA - Série histórica",sub = "Após Plano Real",lwd = 1, xlab = "Tempo"
     ,ylab = " Variação % frente mês anterior", cex.lab=0.8, cex.axis=0.7, pch=3, cex=0.5, col = "blue")
smoothingSpline = smooth.spline(na.omit(IPCA_geral), spar=0.45)
lines(time(IPCA_geral),smoothingSpline$y, col = "red1")
abline(h = m, lwd = 1, lty = 2, col = "black")
abline(h = 0, lwd = 1, lty = 2, col = "grey")


# Resumo dessa serie após plano real
basicStats(IPCA_geral)

basicStats(IPCA_core)

#Olhando para a pac e pacf
acf(IPCA_geral)
pacf(IPCA_geral)[-1]

#Vamos ver a distribuicao do IPCA
dist<-function(n){
  dnorm(n, mean(IPCA_geral), sd(IPCA_geral))
}

d<-density(IPCA_geral)


par(mfrow=c(1,1))

hist(IPCA_geral, breaks = 50, freq=F,
     xlab='', ylab='', main='', ylim = c(0,2))

curve(dist, add=T, col='red')

lines(d, col='blue')


#Testando estacionariedade

# outra através do qq-plot usando com distribuição teórica a normal
qq_plot(IPCA_geral, FUN=qnorm, method = "empirical")

#Testando estacionariedade
install.packages(urca)
library(urca)
summary(ur.df(IPCA_geral, type='none', lags=0))


#teste de media e variancia
media <- mean(IPCA_geral)
var <- var(IPCA_geral)
xma <- sma(IPCA_geral,h=3, interval=TRUE)
ymv <- roll_var(IPCA_geral,3)
ymvm <- roll_var(IPCA_geral,6)
ymvl <- roll_var(IPCA_geral,12)


yma <- sma(as.numeric(ymv),h=3, interval = TRUE)

#Ciclo
library(mFilter)

# HP filter
hp_ipca <- hpfilter(IPCA_geral, freq = 129600)

#Alternativa sofisticada EMD
y <- as.numeric(IPCA_geral)

#Wavelets tambem é sofisticado
wipca <- mra(na.omit(lipca))
wipca_ciclo <- cumsum(wipca$D1+wipca$D2+wipca$D3+wipca$D4)

# Run EMD
c_ipca <- emd(xt = y, boundary = "none")
c_ipca_emd <- IPCA_geral - c_ipca$residue

#Vol instantanea
ipcav <- lag(IPCA_geral^2,1)

#Plotando
par(mfrow = c(2,2))

plot(xma$fitted,type = "l")
abline(h= media, col = "blue")+lines(time(IPCA_geral),ipcav,type="l",col = "darkred")

plot(ymv,type = "l")
abline(h= var, col = "blue")
plot(time(IPCA_geral),hp_ipca$cycle,type = "l")+lines(time(IPCA_geral),c_ipca_emd,type = "l", col = "blue")
+lines(time(IPCA_geral)[-1],wipca_ciclo,type = "l", col = "red")
plot(time(IPCA_geral),yma$fitted, col = "blue",type = "l")

#Vamos decompor um pouco mais rapido
decomposicao <- decompose(as.ts(IPCA_geral),  "additive")

#Observando tendencia, sazonalidade e ciclo
par(mfrow = c(3,1))

plot(decomposicao$trend)
plot(decomposicao$seasonal)
plot(time(IPCA_geral),hp_ipca$cycle,type = "l")+lines(time(IPCA_geral),c_ipca_emd,type = "l", col = "blue")
+lines(time(IPCA_geral)[-1],wipca_ciclo,type = "l", col = "red")

#Olhar sempre para a volatilidade
par(mfrow = c(1,1))
plot(ymv,type = "l",ylim = c(0,1.5),ylab = "Vol ipca")+lines(ymvm, col = "blue")+lines(ymvl, col = "red")

# Testando heteroscedasticidade
#Calculando os residuos regredindo o IPCA em uma constante
byd.mean <- dynlm(IPCA_geral~1)

#residuo quadrado
ehatsq <- ts(resid(byd.mean)^2)

#Regredir nos residuos sq lagados
byd.ARCH <- dynlm(ehatsq~L(ehatsq))

T <- nobs(byd.mean)
q <- length(coef(byd.ARCH))-1
Rsq <- glance(byd.ARCH)[[1]]

#Testar essa estatistica contra a chi sq
LM <- (T-q)*Rsq

#nivel de significancia
alpha <- 0.05
Chicr <- qchisq(1-alpha, q)

library(FinTS)
#Teste arch - aqui a gente assume que os residuos tem media zero, correto? (Ruido branco)
#Teste ta rejeitando normalidade
bydArchTest <- ArchTest(IPCA_geral, lags=1, demean=TRUE)
bydArchTest

#Vamos estimar a VOl
byd.arch <- garch(IPCA_geral,c(1,1))
hhat <- ts(2*byd.arch$fitted.values[-1,1]^2)
smoothingSpline = smooth.spline(hhat, spar=0.15)

#Olhar sempre para a volatilidade
par(mfrow = c(1,1))
plot(ymv,type = "l",ylim = c(0,3),ylab = "Vol ipca")+lines(ymvm, col = "blue")+lines(ymvl, col = "lightblue")+lines(index(ymv)[-1],smoothingSpline$y, col = "red")


smoothingSpline = smooth.spline(hhat, spar=0.05)


#Prevendo a vol 3 steps
plot(time(IPCA_geral)[-1],hhat,type = "l",ylim = c(0,9),ylab = "VOL IPCA",main = "Usando um GARCH (1,1)",col = "blue",pch=3,cex=0.5,
     cex.lab=0.8,cex.axis=0.7)+abline(h = 1.02,col = "darkblue")+abline(h = 1.28,col = "darkred")

#Previsão
library(forecast)
library(MLmetrics)

i <- scale(IPCA_geral)
training=window(i, start = c(2014,1), end = c(2019,12))
validation=window(i, start = c(2018,1))

naive = snaive(training, h=length(validation))
MAPE(naive$mean, validation) * 100

# simple exponential - models level
fit1 <- HoltWinters(IPCA_geral, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit2 <- HoltWinters(IPCA_geral, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(IPCA_geral)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))


plot(IPCA_geral, col="blue", xlab="Year", ylab="IPCA", main="Seasonal Naive Forecast", type='l')
plot(time(IPCA_geral)[278:314],naive$fitted, col="red", type = "l")

#2012
ipca_2012 <- (window(IPCA_geral, start = 2012, end = 2013)/100)[-13]
ipcac_2012 <- (window(IPCA_core, start = 2012, end = 2013)/100)[-13]
#Acumulado no ano
cipca_2012 <- (cumprod(1+ipca_2012)-1)*100
cipcac_2012 <- (cumprod(1+ipcac_2012)-1)*100

#2013
ipca_2013 <- (window(IPCA_geral, start = 2013, end = 2014)/100)[-13]
ipcac_2013 <- (window(IPCA_core, start = 2013, end = 2014)/100)[-13]
#Acumulado no ano
cipca_2013 <- (cumprod(1+ipca_2013)-1)*100
cipcac_2013 <- (cumprod(1+ipcac_2013)-1)*100

#2014
ipca_2014 <- (window(IPCA_geral, start = 2014, end = 2015)/100)[-13]
ipcac_2014 <- (window(IPCA_core, start = 2014, end = 2015)/100)[-13]
#Acumulado no ano
cipca_2014 <- (cumprod(1+ipca_2014)-1)*100
cipcac_2014 <- (cumprod(1+ipcac_2014)-1)*100

#2015
ipca_2015 <- (window(IPCA_geral, start = 2015, end = 2016)/100)[-13]
ipcac_2015 <- (window(IPCA_core, start = 2015, end = 2016)/100)[-13]

#Acumulado no ano
cipca_2015 <- (cumprod(1+ipca_2015)-1)*100
cipcac_2015 <- (cumprod(1+ipcac_2015)-1)*100

#2016
ipca_2016 <- (window(IPCA_geral, start = 2016, end = 2017)/100)[-13]
ipcac_2016 <- (window(IPCA_core, start = 2016, end = 2017)/100)[-13]

#Acumulado no ano
cipca_2016 <- (cumprod(1+ipca_2016)-1)*100
cipcac_2016 <- (cumprod(1+ipcac_2016)-1)*100

#2017
ipca_2017 <- (window(IPCA_geral, start = 2017, end = 2018)/100)[-13]
ipcac_2017 <- (window(IPCA_core, start = 2017, end = 2018)/100)[-13]

#Acumulado no ano
cipca_2017 <- (cumprod(1+ipca_2017)-1)*100
cipcac_2017 <- (cumprod(1+ipcac_2017)-1)*100

#2018
ipca_2018 <- (window(IPCA_geral, start = 2018, end = 2019)/100)[-13]
ipcac_2018 <- (window(IPCA_core, start = 2018, end = 2019)/100)[-13]

#Acumulado no ano
cipca_2018 <- (cumprod(1+ipca_2018)-1)*100
cipcac_2018 <- (cumprod(1+ipcac_2018)-1)*100

#2019
ipca_2019 <- (window(IPCA_geral, start = 2019, end = 2020)/100)[-13]
ipcac_2019<- (window(IPCA_core, start = 2019, end = 2020)/100)[-13]

#Acumulado no ano
cipca_2019 <- (cumprod(1+ipca_2019)-1)*100
cipcac_2019 <- (cumprod(1+ipcac_2019)-1)*100

#2020
di <- as.character(Sys.Date() - months(12))

ipca_2020 <- window(IPCA_geral, start = di)/100

ipcac_2020 <- window(IPCA_core, start = di)/100

# Acumulado ultimos 12 meses
cipca <- (cumprod(1+ipca_2020)-1)*100
cipcac <- (cumprod(1+ipcac_2020)-1)*100


#Anos anteriores
e <- as.zoo(rbind(cipca_2012,cipca_2013,cipca_2014,cipca_2015,cipca_2016,cipca_2017,cipca_2018,cipca_2019,cipca[-1]))
e1 <- as.zoo(rbind(cipca_2012,cipca_2013,cipca_2014,cipcac_2015,cipcac_2016,cipcac_2017,cipcac_2018,cipcac_2019,cipcac[-1]))

ey <- smooth.spline(e,spar = 0.5)
ey1 <- smooth.spline(e1,spar = 0.5)

# Vamos plotar esses dados
target <- 4
par(mfrow = c(1,1))
plot(e, main = "IPCA",lwd = 1, xlab = "Tempo"
     ,ylab = "Acumulado ultimos meses (%)", cex.lab=0.8, cex.axis=0.7, pch=3, cex=0.5, col = "darkblue")
points(time(e),ey$y, cex.lab=0.8, cex.axis=0.7, pch=3, cex=0.5, col = "black")
lines(time(e),ey1$y, col = "red1",lty = 2)
abline(h = target, lwd = 1, lty = 2, col = "black")
abline(h = 0, lwd = 0.5, lty = 2, col = "darkgrey")
legend("topright", legend = c("IPCA","IPCA core","MA"), col = c("darkblue","red1","black"), lty=1, cex=0.6)

aumento_frente_mesmo_mês_ano_anterior <- as.numeric(tail(cipca,1))/as.numeric(cipca_2019[11])


# A previsáo mais tosca que da pra fazer aqui é usar o auto arima
## O certo é olhar a funcao de auto correlacao e a parcial, escolher o melhor modelo
## baseado no AIC ou schwarz e rodar esse arima. Dá pra ver que não é estacionario I(1)
## é interessante comparar também com outros ciclos, tendencias e outros modelos arma

a <- na.approx(as.ts(e))
decomposicao <- decompose(e,  "additive")

#Observando tendencia, sazonalidade e ciclo
par(mfrow = c(3,1))

plot(decomposicao$trend)
plot(decomposicao$seasonal)
plot(decomposicao$random)

#Olhar sempre para a volatilidade
par(mfrow = c(1,1))
plot(ymv,type = "l",ylim = c(0,1.5),ylab = "Vol ipca")+lines(ymvm, col = "blue")+lines(ymvl, col = "red")

# Testando heteroscedasticidade
#Calculando os residuos regredindo o IPCA em uma constante
byd.mean <- dynlm(IPCA_geral~1)

#residuo quadrado
ehatsq <- ts(resid(byd.mean)^2)

#Regredir nos residuos sq lagados
byd.ARCH <- dynlm(ehatsq~L(ehatsq))

T <- nobs(byd.mean)
q <- length(coef(byd.ARCH))-1
Rsq <- glance(byd.ARCH)[[1]]

#Testar essa estatistica contra a chi sq
LM <- (T-q)*Rsq

#nivel de significancia
alpha <- 0.05
Chicr <- qchisq(1-alpha, q)

library(FinTS)
#Teste arch - aqui a gente assume que os residuos tem media zero, correto? (Ruido branco)
#Teste ta rejeitando normalidade
bydArchTest <- ArchTest(IPCA_geral, lags=1, demean=TRUE)
bydArchTest

#Vamos estimar a VOl
byd.arch <- garch(IPCA_geral,c(1,1))
hhat <- ts(2*byd.arch$fitted.values[-1,1]^2)
smoothingSpline = smooth.spline(hhat, spar=0.15)

#Olhar sempre para a volatilidade
par(mfrow = c(1,1))
plot(ymv,type = "l",ylim = c(0,3),ylab = "Vol ipca")+lines(ymvm, col = "blue")+lines(ymvl, col = "lightblue")+lines(index(ymv)[-1],smoothingSpline$y, col = "red")


smoothingSpline = smooth.spline(hhat, spar=0.05)


#Prevendo a vol 3 steps
plot(time(IPCA_geral)[-1],hhat,type = "l",ylim = c(0,9),ylab = "VOL IPCA",main = "Usando um GARCH (1,1)",col = "blue",pch=3,cex=0.5,
     cex.lab=0.8,cex.axis=0.7)+abline(h = 1.02,col = "darkblue")+abline(h = 1.28,col = "darkred")

#Previsão

library(forecast)
library(MLmetrics)

i <- scale(IPCA_geral)
training=window(i, start = c(2014,1), end = c(2019,12))
validation=window(i, start = c(2018,1))

naive = snaive(training, h=length(validation))
MAPE(naive$mean, validation) * 100


# simple exponential - models level
fit1 <- HoltWinters(IPCA_geral, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit2 <- HoltWinters(IPCA_geral, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(IPCA_geral)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))



1,96 5%
1,98 rejeita a hipotese nula 
1,95 falha em rejeitar a hipotese nula
