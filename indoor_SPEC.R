#' ---
#' title: "Indoor SPEC sensors online"
#' author: Gustavo Olivares
#' output: pdf_document
#' ---

#' ## Prepare the environment

# Fetch data from several units

##### Load relevant packages #####

library(readr)
library(reshape2)
library(ggplot2)
library(scales)
library(gstat)
library(RJSONIO)
library(curl)
library(base64enc)
library(zoo)
library(openair)
library(stringi)
library(viridis)
library(dplyr)
library(RColorBrewer)
library(purrr)
library(magick)


# Load API data
data.api <- read_delim("~/data/Waterview2018/APINOx/tunnel/LOF01.txt", 
                    "\t", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
data.api$X4 <- NULL
start_date <- as.POSIXct("2018/11/21 22:15:00",tz = 'UTC')
idx_vec <- (1:length(data.api$X1)) - 1
data.api$date <- start_date + idx_vec

data.api.1min <- timeAverage(data.api,avg.time = '1 min')

## Get the SPEC data #####
# CHOOSE IF GETTING ONLINE DATA
go_online <- FALSE
if (go_online){
  # Fetch from PHANT server
  req1 <- curl_fetch_memory(read_address)
  jreq1 <- fromJSON(rawToChar(req1$content))
  
  # UTC time start
  x_now <- Sys.time()
  print(x_now)
  # UTC time start
  t_start <- (as.POSIXct("2018/11/12 00:00:00",tz = "GMT-12"))
  # UTC time end
  t_end <- (as.POSIXct("2018/12/01 20:00:00",tz = "GMT-12"))
  # Set the averaging interval
  time_avg <- '15 min'
  
  xx2 <- unlist(jreq1)
  ndata <- length(xx2)
  data.spec <- data.frame(date = as.POSIXct(xx2[seq(7,ndata,7)],format="%Y-%m-%dT%H:%M:%OS", tz='UTC'))
  data.spec$CO <- as.numeric(xx2[seq(1,ndata,7)])
  data.spec$T_CO <- as.numeric(xx2[seq(2,ndata,7)])
  data.spec$NO2 <- as.numeric(xx2[seq(3,ndata,7)])
  data.spec$T_NO2 <- as.numeric(xx2[seq(4,ndata,7)])
  data.spec$SN_CO <- as.numeric(xx2[seq(5,ndata,7)])
  data.spec$SN_NO2 <- as.numeric(xx2[seq(6,ndata,7)])
  data.spec$CO.NO2 <- data.spec$CO/data.spec$NO2
  
  data.spec <- subset(data.spec,(date>t_start)&(date < t_end))
} else {
  #Load from local file
  data.spec.raw <- read_csv("/mnt/bigdisk/data/Waterview2018/SPEC/tunnel-data.txt")
  data.spec.raw$date <- as.POSIXct(data.spec.raw$timestamp,tz='UTC')
  data.spec <- data.spec.raw[,c('date','ppb1','adc1','ppb2','adc2')]
  names(data.spec) <- c('date',
                        'COspec',
                        'COspecRAW',
                        'NO2spec',
                        'NO2specRAW')
  
}
#### Merge data ####
names(data.spec) <- c('date','COspec','T_CO','NO2spec','T_NO2','SN_CO','SN_NO2','CO.NO2')
names(data.api.1min) <- c('date','NOx','NO2raw','NO')

all.data <- merge(data.api.1min,data.spec,by = 'date',all = TRUE)
all.data$NO2 <- all.data$NO2raw * 0.63 - 58.8

#### Plot data ####

timeVariation(all.data,pollutant = c('NO2','NO2spec'))


