Sys.setenv(LANGUAGE="es")
rm(list=ls(all=TRUE))

library(prettydoc)
library(caTools)
library(bitops)
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
library(backports)
library(lubridate)

getSymbols("AXTELCPO.MX",src="yahoo",from="2015-01-01",to="2020-02-19")

AXTELCPO.MX <- AXTELCPO.MX[apply(AXTELCPO.MX ,1,function(x) all(!is.na(x))),]
View(AXTELCPO.MX)

dygraph(AXTELCPO.MX$AXTELCPO.MX.Close) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.5, drawGrid = F, colors="darkblue") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = F)  %>%
  dyRoller(rollPeriod = 1)

###DESCOMPOSICION###
Cierre<-AXTELCPO.MX$AXTELCPO.MX.Close
AXTEL_des <- ts(Cierre, start = c(2015,01,02), frequency = 252) 
AXTEL_stl<-stl(AXTEL_des[, 1], s.window = "periodic")

##SI SE MUESTRA####
plot(AXTEL_stl, main="Descomposición de AXTEL a w=252",col= "darkblue")

####PRONOSTICOS####
########1##########
pronostico<-forecast(AXTEL_des, h=252)
View(pronostico)

fecha<-format(date_decimal(as.numeric(row.names(as.data.frame(pronostico)))),"%Y-%m-%d")

pronostico_completo<-cbind(fecha,as.data.frame(pronostico))
View(pronostico_completo)

#######2#########
pronostico1<-forecast(AXTEL_des, h=230)
View(pronostico1)

fecha<-format(date_decimal(as.numeric(row.names(as.data.frame(pronostico1)))),"%Y-%m-%d")

pronostico1_completo<-cbind(fecha,as.data.frame(pronostico1))
View(pronostico1_completo)

####GRAFICO ESTACIONAL POR AÑO######
ggseasonplot(AXTEL_des,year.labels=TRUE,year.labels.left=T) +
  ylab("Precio de cierre") +
  xlab("Componente estacional") +
  ggtitle("Gráfico estacional de AXTEL por año")

######GRAFICOS PRONOSTICOS#####
####1######
subconjunto <- window(AXTEL_des, start = 2018)
autoplot(subconjunto) + geom_forecast(h=252)+
  ggtitle("Pronóstico de AXTEL a 252 días, w= 252")


#######2#######
subconjunto1 <- window(AXTEL_des, start = 2018)
autoplot(subconjunto1) + geom_forecast(h=230)+
  ggtitle("Pronóstico de AXTEL a 230 días, w= 230")

######ARCHIVOS CSV######
####1#####
write.csv(pronostico_completo, "E:/Axtel_252.csv")

####2####
write.csv(pronostico1_completo, "E:/Axtel a 230 dias.csv")


####AIC#####
###1####
pronostico[["model"]][["aic"]]

#####2######
pronostico1[["model"]][["aic"]]


#######MSE########
####1######
pronostico[["model"]][["mse"]]

######2######
pronostico1[["model"]][["mse"]]
