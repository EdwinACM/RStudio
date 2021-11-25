rm(list=ls(all=TRUE))

##LIBRERIAS
options(repos='http://cran.rstudio.com/')
library(RCurl)
library(packrat)
library(rsconnect)
library(markdown)
library(rmarkdown)
library(knitr)
library(quantmod)
library(xts)
library(tseries)
library(forecast)
library(timeSeries)
library(tframePlus)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(ggthemes)
library(tidyverse)
library(dygraphs)
library(gridExtra)
library(rugarch)
library(aTSA)
library(zoo)
library(rmdformats)
library(forecast)

##TABLA DE PONDERACIONES
Ponderaciones<-read.csv(file.choose())
Ponderaciones

##DATOS DE YAHOO FINANCE
TickerList<-("IVW") #CAMBIAR TICKER
Precio<- NULL
for (Ticker in TickerList)
  Precio <- cbind(Precio,
                  getSymbols(Ticker, from="2015-01-01", to="2020-03-20", auto.assign =F ,src='yahoo') [,4]) #Mantiene solo el precio de cierre

View(Precio)

##QUITAR NA
PrecioCierre <- Precio[apply(Precio ,1,function(x) all(!is.na(x))),]
View(PrecioCierre)

##PONER NOMBRE A LAS COLUMNAS ES  IMPORTANTE QUE LO HAGAS
colnames(PrecioCierre)<- ("IVW")
View(PrecioCierre)

##PASARLO A RENDIMIENTOS
rendimientos <- as.timeSeries(tail(PrecioCierre,-1) / as.numeric(head(PrecioCierre,-1))-1)
rendimientos<- as.xts(rendimientos)
View(rendimientos)

##PASARLO A LOGARITMOS
precios_log <-diff(log(PrecioCierre))
precios_log <-precios_log[-1,]

## DARLE NOMBRE DE NUEVO A IVW E IVW_R 
IVW<-PrecioCierre[,1]
IVW_R<-precios_log[,1]

##GRAFICO DE PRECIO DE CIERRE
dygraph(PrecioCierre$IVW) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.5, drawGrid = F, colors="lightseagreen") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = F)  %>%
  dyRoller(rollPeriod = 1)

##GRAFICO DE RENDIMIENTOS
RENDIMIENTOS <- ggplot(precios_log, aes(Index)) +
  geom_line(aes(y = IVW_R, colour = "IVW"))+
  scale_y_continuous(labels=scales::percent)

RENDIMIENTOS 

##HISTOGRAMA A NIVELES
hist_IVW<-ggplot(IVW, aes(x=IVW)) +
  geom_histogram( binwidth=10, color="blue", alpha=0.9) +
  ggtitle("Histograma de IVW a niveles") +
  xlab("Puntos de IVW")+
  ylab("Frecuencia")

ggplotly(hist_IVW)

##HISTOGRAMA EN RENDIMIENTOS
hist_IVW_R<-ggplot(IVW_R, aes(x=IVW_R)) +
  geom_histogram( binwidth=0.001, color="blue", alpha=0.9) +
  ggtitle("Histograma de IVW en rendimientos") +
  xlab("Rendimientos IVW")+
  ylab("Frecuencia")

ggplotly(hist_IVW_R)

##QQPLOT A NIVELES
IVW_QQ<-ggplot(IVW, aes(sample=IVW)) +
  stat_qq() + stat_qq_line(colour="blue") +
  ggtitle("Q-Q plot de IVW a niveles") +
  xlab("Cuantiles teóricos")+
  ylab("Muestra de cuantil")

ggplotly(IVW_QQ)

##QQPLOT A RENDIMIENTOS
IVW_QQ_R<-ggplot(precios_log, aes(sample=IVW)) +
  stat_qq() + stat_qq_line(colour="blue") +
  ggtitle("Q-Q plot de IVW en rendimientos") +
  xlab("Cuantiles teóricos")+
  ylab("Muestra de cuantil")

ggplotly(IVW_QQ_R)

#TEST QUE SE PIDEN, PONERLOS EN TABLA
adf.test(IVW)
PP.test(IVW, lshort = TRUE)

adf.test(IVW_R)
PP.test(IVW_R, lshort = TRUE)

kpss.test(IVW)

kpss.test(IVW_R)

##MODELOS ARIMA
IVW %>% ggtsdisplay(main="Función de Autocorrelación (MA) y Función de Autocorrelación parcial (AR)")
IVW %>% diff() %>% ggtsdisplay(main="Función de Autocorrelación (MA) y Función de Autocorrelación parcial (AR)")


##PROPUESTA 1
fit<-auto.arima(IVW, seasonal=FALSE) #NOS DA EL MEJOR MODELO ARIMA SEGÚN R
fit

checkresiduals(fit,lag=30,test="LB")

autoplot(fit, title = "Raices invertidas sobre AR y MA")


##PROPUESTA 2
fit1= arima(IVW, order=c(4,1,26))
fit1

checkresiduals(fit1,lag=30,test="LB")
autoplot(fit1, title = "Raices invertidas sobre AR y MA")

##PRONOSTICO 1
fit %>% forecast(h=20) %>% autoplot(200)

##PRONOSTICO 2
fit1 %>% forecast(h=20) %>% autoplot(200)

fit[["aic"]]
fit1[["aic"]]

install.packages("forecast", dependencies = TRUE)