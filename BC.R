#' ---
#' title: "BC in Waterview tunnel"
#' author: Gustavo Olivares
#' output: pdf_document
#' ---

#' ## Overview
#' This document presents a summary of the PM2.5 measurements performed inside the Waterview tunnel.
#' 
#' The instrumentation used was:
#' * AE22 aethalometer - ae22
#' * MA350 MicroAeth - ma350
#' 
#' All the raw data will be averaged up to 10 minute values for the analysis.
#' 
#' ## Prepare the environment

# Load relevant packages #####
library(readr)
library(ggplot2)
library(openair)


# Base path
data_path <- path.expand("~/data/Waterview2018/")
###########
#' ## AE22
# Load data ####
ae22.raw <- read_csv(paste0(data_path,"ae22.txt"),
                 col_names = FALSE, col_types = cols(X2 = col_character()))
names(ae22.raw) <- c('day',
                 'time',
                 'bc',
                 'uvbc',
                 'flow',
                 'sz1',
                 'sb1',
                 'rz1',
                 'rb1',
                 'fraction1',
                 'attenuation1',
                 'sz2',
                 'sb2',
                 'rz2',
                 'rb2',
                 'fraction2',
                 'attenuation2')
ae22.raw$date <- as.POSIXct(paste(ae22.raw$day, ae22.raw$time),format = '%d-%b-%y %H:%M',tz='NZST')

ae22<- timeAverage(ae22.raw,avg.time = '10 min')
ae22$deltaBC <- ae22$uvbc - ae22$bc
ae22$bc.ov.uvbc <- ae22$bc/ae22$uvbc
ae22$uvbc.ov.bc <- ae22$uvbc/ae22$bc

timePlot(subset(ae22,date<as.POSIXct("2018-10-22")),pollutant = c('bc','uvbc'), avg.time = '10 min', group = TRUE)
timeVariation(subset(ae22,date<as.POSIXct("2018-10-22")),pollutant = c('bc','uvbc'))
timeVariation(subset(ae22,date<as.POSIXct("2018-10-22")),pollutant = c('deltaBC'),normalise = TRUE)
timeVariation(subset(ae22,date<as.POSIXct("2018-10-22")),pollutant = c('uvbc.ov.bc'),normalise = TRUE)
timeVariation(subset(ae22,date<as.POSIXct("2018-10-22")),pollutant = c('bc.ov.uvbc'),normalise = TRUE)
#' Could these be related to traffic patterns?
#' 

uvplot <- ggplot(data = ae22,aes(x=date)) + geom_point(aes(y=bc))

