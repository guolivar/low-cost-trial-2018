#' ---
#' title: "PM2.5 in Waterview tunnel"
#' author: Gustavo Olivares
#' output: html_document
#' ---

#' ## Overview
#' This document presents a summary of the PM2.5 data during the co-location of ODIN, ES-642 and 
#' Beta Attenuation Monitor at Auckland's Waterview tunnel in November 2018.
#'  The main goal is to evaluate compare the response of the different sensors to traffic emissions.

#' ## Prepare the environment
# Load packages
library(ggplot2)
library(openair)
# Fetch and load data
load(url('https://ndownloader.figshare.com/files/17860082'))
#' You can download the data here:
#' 
#' https://ndownloader.figshare.com/files/17860082
#' 
#' 


#' ## Numeric summary
summary(colo.data[,c('PM2.5.BAM','PM2.5.odin','PM2.5.grimm','PM2.5.mote')],digits=2)

#' ## Graphic summary
#' ### ODIN vs ES-642
scatterPlot(colo.data,x='PM2.5.odin',y='PM2.5.mote',linear = TRUE)
#' Now using a linear fit, what is the prediction confidence interval between odin and ES-642
odin.mote.lm <- lm(data=colo.data,PM2.5.mote ~ PM2.5.odin + 1)
pred.int <- predict(odin.mote.lm,newdata = colo.data, interval = "prediction",level = 0.90)
plot1.data <- cbind(colo.data, pred.int)
ggplot(plot1.data, aes(PM2.5.odin, PM2.5.mote)) +
  geom_point() +
  stat_smooth(method = lm) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

#' ### ODIN against BAM
scatterPlot(colo.data,x='PM2.5.odin',y='PM2.5.BAM',linear = TRUE)
odin.bam.lm <- lm(data=colo.data,PM2.5.BAM ~ PM2.5.odin + 1)
pred.int <- predict(odin.bam.lm,newdata = colo.data, interval = "prediction",level = 0.90)
plot2.data <- cbind(colo.data, pred.int)
ggplot(plot2.data, aes(PM2.5.odin, PM2.5.BAM)) +
  geom_point() +
  stat_smooth(method = lm) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

#' ### ES-642 against BAM
scatterPlot(colo.data,x='PM2.5.mote',y='PM2.5.BAM',linear = TRUE)
odin.bam.lm <- lm(data=colo.data,PM2.5.BAM ~ PM2.5.mote + 1)
pred.int <- predict(odin.bam.lm,newdata = colo.data, interval = "prediction",level = 0.90)
plot3.data <- cbind(colo.data, pred.int)
ggplot(plot3.data, aes(PM2.5.mote, PM2.5.BAM)) +
  geom_point() +
  stat_smooth(method = lm) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")








