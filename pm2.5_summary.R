#' ---
#' title: "PM2.5 in Waterview tunnel"
#' author: Gustavo Olivares
#' output: pdf_document
#' ---

#' ## Overview
#' This document presents a summary of the PM2.5 measurements performed inside the Waterview tunnel.
#' 
#' The instrumentation used was:
#' * Beta Attenuation Monitor (ThermoFisher) - BAM
#' * ODIN-SD (Plantower PMS3003) - ODIN
#' * Grimm spectrometer (11-E) - Grimm
#' * Mote Dust Sentries - Mote
#' 
#' All the raw data will be averaged up to 10 minute values for the analysis.
#' 
#' ## Prepare the environment

# Load relevant packages #####
library(readr)
library(ggplot2)
library(RJSONIO)
library(curl)
library(base64enc)
library(openair)
library(stringr)
library(parallel)
library(doParallel)




# Base path
data_path <- path.expand("~/data/Waterview2018/")
###########
#' ## BAM
# Load data ####
rawdata.BAM <- read_csv(paste0(data_path,"BAM/tunnel/bam_data.txt"),
                     col_types = cols(date = col_datetime(format = "%Y/%m/%d %H:%M:%S GMT")))
rawdata.BAM$errorcode <- as.factor(rawdata.BAM$errorcode)
# Clean data

#' Here are the summary statistics for relevant measurements.
#'  The "errorcode" variable reports the internal status of the instrument.

summary(rawdata.BAM[,c('pm2.5','pressure','temp4','flow','errorcode')],maxsum = 13)

#' The errorcode "0000000000000000000000" corresponds to the normal operation of the instrument
#'  and the errorcode "0000000000000000002000" indicates a higher than expected temperature compensation. The rest of the error codes
#'   correspond to less than 2% of the data and indicate temporary conditions of the instrument. For this application, we will remove
#'   all datapoints that have an errorcode other than "0000000000000000000000" and "0000000000000000002000"

data.BAM <- subset(rawdata.BAM,errorcode == '0000000000000000000000' | errorcode == '0000000000000000002000')

# Timeseries of raw data ####
timePlot(data.BAM,pollutant = 'pm2.5',
         ylim = c(-100,200),
         main = 'RAW PM2.5 1min data')
timePlot(data.BAM,pollutant = 'pm2.5',
         ylim = c(-50,100),
         avg.time = '10 min',
         main = 'PM2.5 10 min data')
timePlot(data.BAM,pollutant = 'pm2.5',
         ylim = c(-50,200),
         avg.time = '1 hour',
         main = 'PM2.5 1 hour average')
timeVariation(data.BAM,pollutant = 'pm2.5')

# Working dataset ####
data.BAM.10min <- timeAverage(data.BAM,avg.time = '10 min')[,c('date','pm2.5')]
names(data.BAM.10min) <- c('date','PM2.5.BAM')
###########
#' ## ODIN
# Read the secrets for ODIN data ####
secret_hologram <- read_delim("./secret_hologram.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
# Get the devices ID #####
base_url <- "https://dashboard.hologram.io/api/1/devices?"
built_url <- paste0(base_url,
                    "orgid=",secret_hologram$orgid,"&",
                    "limit=1000&",
                    "apikey=",secret_hologram$apikey)
req1 <- curl_fetch_memory(built_url)
jreq1 <- fromJSON(rawToChar(req1$content))$data
nsites <- length(jreq1)
curr_data <- data.frame(deviceid = (1:nsites),ODIN = NA)
for (i in (1:nsites)){
  curr_data$deviceid[i] <- jreq1[[i]]$id
  curr_data$ODIN[i] <- jreq1[[i]]$name
}

odin_nrs <- c("0053",
              "0052",
              "0075",
              "0076",
              "0077",
              "0074",
              "0072",
              "0059")

odin <- curr_data[1:length(odin_nrs),]
for (i in (1:length(odin_nrs))){
  odin_idx <- which(str_detect(curr_data$ODIN,odin_nrs[i]))
  odin[i,] <-curr_data[odin_idx,]
}

deviceids <- paste(odin$deviceid,collapse = ",")

## Get the timeseries data #####
# UTC time start

# UTC time start
t_start <- as.numeric(as.POSIXct("2018/10/29 12:00:00",tz = "GMT-12"))
# UTC time end ... now
t_end <- floor(as.numeric(as.POSIXct("2018/12/23 12:00:00",tz = "GMT-12")))
# Set the averaging interval
time_avg <- '10 min'

ndata <- 1
nstep <- 1
print("Getting data")
base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"

nstep <- 1
while (ndata >= 1){
  if (nstep == 1){
    built_url <- paste0(base_url,
                        "deviceids=",deviceids,"&",
                        "timestart=",t_start,"&",
                        "timeend=",t_end,"&",
                        "limit=1000&",
                        "orgid=",secret_hologram$orgid,"&",
                        "apikey=",secret_hologram$apikey)
    req2 <- curl_fetch_memory(built_url)
    jreq2_tmp <- fromJSON(rawToChar(req2$content))$data
    jreq2 <- jreq2_tmp
  } else {
    built_url <- paste0(base_url,
                        "deviceids=",deviceids,"&",
                        "timestart=",t_start,"&",
                        "timeend=",t_end,"&",
                        "limit=1000&",
                        "startat=",startat,"&",
                        "orgid=",secret_hologram$orgid,"&",
                        "apikey=",secret_hologram$apikey)
    req2 <- curl_fetch_memory(built_url)
    jreq2_tmp <- fromJSON(rawToChar(req2$content))$data
    jreq2 <- append(jreq2,fromJSON(rawToChar(req2$content))$data)
  }
  print(ndata <- length(jreq2_tmp))
  if (ndata >=1){
    startat <- jreq2_tmp[[ndata]]$id
    print(jreq2_tmp[[ndata]]$logged)
  }
  nstep <- nstep + 1
}






#### STILL TESTING ######
# We'll do this in parallel because it takes A LONG time with a few 100k records
#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(2) #not to overload your computer
registerDoParallel(cl)

all_data <- foreach(i=1:ndata,
                    .packages=c("base64enc","RJSONIO"),
                    .combine=rbind,
                    .errorhandling = 'remove') %dopar%
                    {
                      c_data <- data.frame(id = 1)
                      c_data$PM1 <- NA
                      c_data$PM2.5 <- NA
                      c_data$PM10 <- NA
                      c_data$PMc <- NA
                      c_data$GAS1 <- NA
                      c_data$Tgas1 <- NA
                      c_data$GAS2 <- NA
                      c_data$Temperature <- NA
                      c_data$RH <- NA
                      c_data$date <- NA
                      c_data$timestamp <- NA
                      c_data$deviceid <- NA
                      c_data$tags <- NA
                      xxx <- rawToChar(base64decode(fromJSON(jreq2[[i]]$data)$data))
                      x_payload <- try(fromJSON(xxx),silent = TRUE)
                      if (inherits(x_payload,"try-error")) {
                        c_data
                        next
                      }
                      payload <- unlist(x_payload)
                      if (length(payload)<5){
                        c_data
                        next
                      }
                      # {"PM1":4,"PM2.5":6,"PM10":6,"GAS1":-999,"Tgas1":0,"GAS2":204,"Temperature":7.35,"RH":80.85,"recordtime":"2018/07/11;00:21:01"}
                      c_data$PM1 <- as.numeric(payload[1])
                      c_data$PM2.5 <- as.numeric(payload[2])
                      c_data$PM10 <- as.numeric(payload[3])
                      c_data$PMc <- as.numeric(payload[3]) - as.numeric(payload[2])
                      c_data$GAS1 <- as.numeric(payload[4])
                      c_data$Tgas1 <- as.numeric(payload[5])
                      c_data$GAS2 <- as.numeric(payload[6])
                      c_data$Temperature <- as.numeric(payload[7])
                      c_data$RH <- as.numeric(payload[8])
                      c_data$date <- as.POSIXct(as.character(payload[9]),format = "%Y/%m/%d;%H:%M:%S",tz="UTC")
                      c_data$timestamp <- as.POSIXct(jreq2[[i]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
                      c_data$deviceid <- jreq2[[i]]$deviceid
                      c_data$tags <- paste((jreq2[[i]]$tags),collapse = ",")
                      c_data
                    }

stopCluster(cl)


######################################################3333












ndata <- length(jreq2)
c_data <- data.frame(id = (1:ndata))
c_data$PM1 <- NA
c_data$PM2.5 <- NA
c_data$deviceid <- NA
c_data$PM10 <- NA
c_data$PMc <- NA
c_data$GAS1 <- NA
c_data$Tgas1 <- NA
c_data$GAS2 <- NA
c_data$Temperature <- NA
c_data$RH <- NA
c_data$date <- as.POSIXct(jreq2[[ndata]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
c_data$timestamp <- c_data$date


for (i in (1:ndata)){
  xxx <- rawToChar(base64decode(fromJSON(jreq2[[i]]$data)$data))
  x_payload <- try(fromJSON(xxx),silent = TRUE)
  if (inherits(x_payload,"try-error")) {
    next
  }
  
  payload <- unlist(x_payload)
  if (length(payload)<5){
    next
  }
  # {"PM1":4,"PM2.5":6,"PM10":6,"GAS1":-999,"Tgas1":0,"GAS2":204,"Temperature":7.35,"RH":80.85,"recordtime":"2018/07/11;00:21:01"}
  c_data$PM1[i] <- as.numeric(payload[1])
  c_data$PM2.5[i] <- as.numeric(payload[2])
  c_data$PM10[i] <- as.numeric(payload[3])
  c_data$PMc[i] <- as.numeric(payload[3]) - as.numeric(payload[2])
  c_data$GAS1[i] <- as.numeric(payload[4])
  c_data$Tgas1[i] <- as.numeric(payload[5])
  c_data$GAS2[i] <- as.numeric(payload[6])
  c_data$Temperature[i] <- as.numeric(payload[7])
  c_data$RH[i] <- as.numeric(payload[8])
  c_data$timestamp[i] <- as.POSIXct(as.character(payload[9]),format = "%Y/%m/%d;%H:%M:%S",tz="UTC")
  c_data$date[i] <- as.POSIXct(jreq2[[i]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
  c_data$deviceid[i] <- jreq2[[i]]$deviceid
}


wrong_dates <- which(is.na(c_data$date) | (c_data$date <= as.POSIXct("2018/01/01")) | c_data$date > as.POSIXct(Sys.time()))
tmp_error_catching <- try(c_data$date[wrong_dates] <- c_data$timestamp[wrong_dates],
                          silent = TRUE)
wrong_dates <- which(c_data$date <= as.POSIXct("2010/01/01"))
tmp_error_catching <- try(c_data$date[wrong_dates] <- NA,
                          silent = TRUE)

devices <- unique(c_data$deviceid)
print(devices)


# Working dataset ####
data.odin <- merge(subset(c_data,deviceid == odin$deviceid[1])[,c(2,4)],
                   subset(c_data,deviceid == odin$deviceid[1])[,c(2,4)],
                   by = "date",
                   all=TRUE)
names(data.odin) <- c('date',
                      paste0(substr(names(data.odin)[2],1,5),'.',substr(odin$ODIN[1],6,9)),
                      paste0(substr(names(data.odin)[3],1,5),'.',substr(odin$ODIN[2],6,9)))
for (i_dev in (3:length(odin_nrs))) {
  print(i_dev)
  c_names <- names(data.odin)
  data.odin <- merge(data.odin,
                     subset(c_data,deviceid == odin$deviceid[i_dev])[,c(2,4)],
                     by = "date",
                     all=TRUE)
  names(data.odin) <- c(c_names,
                        paste0(substr(names(c_data)[2],1,5),'.',substr(odin$ODIN[i_dev],6,9)))
}
data.odin.10min <- timeAverage(data.odin,avg.time = '10 min')
######
#' Grimm
# Load data ####
rawdata.grimm <- read_delim(paste0(data_path,"Grimm/tunnel/parsed_grimm_data.txt"),delim = '\t')
grimm.datestring <- with(rawdata.grimm,paste0("20",
                                              Year,"-",
                                              Month,"-",
                                              Day," ",
                                              Hour,":",
                                              Minute,":",
                                              Second))
rawdata.grimm$date <- as.POSIXct(grimm.datestring,tz="NZDT")
rawdata.grimm <- rawdata.grimm[,c(12,7:11)]
names(rawdata.grimm) <- c('date',paste0(names(rawdata.grimm)[2:6],".grimm"))
rawdata.grimm <- subset(rawdata.grimm,date>as.POSIXct("2018-10-29 00:00:00"))
# Summary ####
summary(rawdata.grimm[,2:6],maxsum = 13)
timePlot(rawdata.grimm,
         pollutant = c('PM1.grimm','PM2.5.grimm','PM10.grimm','TSP.grimm'),
         avg.time = '10 min',
         ylim = c(0,150),
         group = TRUE)
timePlot(rawdata.grimm,
         pollutant = 'n300.grimm',
         avg.time = '10 min',
         ylim = c(0,500000))
timeVariation(rawdata.grimm,
              pollutant = c('PM1.grimm','PM2.5.grimm','PM10.grimm','TSP.grimm'))
# Working dataset ####
data.grimm.10min <- timeAverage(rawdata.grimm,avg.time = '10 min')

######
#' Mote
# Load data ####
rawdata.mote1 <- read_delim(paste0(data_path,"Mote/mote_PM10.txt"),delim = '\t')
rawdata.mote2 <- read_delim(paste0(data_path,"Mote/mote_PM2.5.txt"),delim = '\t')
# Remove Flow data
rawdata.mote2$Flow <- NULL
# Remove less than 75% samples
rawdata.mote1 <- subset(rawdata.mote1,NsamplesPM10>=(60*0.75))
rawdata.mote2 <- subset(rawdata.mote2,NsamplesPM2.5>=(60*0.75))
# Turn date into date
rawdata.mote1$date <- as.POSIXct(rawdata.mote1$date,tz="UTC")
rawdata.mote2$date <- as.POSIXct(rawdata.mote2$date,tz="UTC")
# Calculate metrics
rawdata.mote1$PM10.mote <- rawdata.mote1$SumPM10/rawdata.mote1$NsamplesPM10
rawdata.mote2$PM2.5.mote <- rawdata.mote2$SumPM2.5/rawdata.mote2$NsamplesPM2.5
# Summary ####
summary(rawdata.mote1[,4:7],maxsum = 13)
summary(rawdata.mote2[,4:7],maxsum = 13)
timePlot(rawdata.mote1,
         pollutant = 'PM10.mote',
         avg.time = '10 min',
         ylim = c(0,150),
         group = TRUE)
timePlot(rawdata.mote2,
         pollutant = 'PM2.5.mote',
         avg.time = '10 min',
         ylim = c(0,100))
timeVariation(rawdata.mote1,
              pollutant = 'PM10.mote')
timeVariation(rawdata.mote2,
              pollutant = 'PM2.5.mote')
# Working dataset ####
# Merge the two mote channels
rawdata.mote <- merge(rawdata.mote1[,c(1,4,5,6,7)],rawdata.mote2[,c(1,7)],by = 'date',all = TRUE)
data.mote.10min <- timeAverage(rawdata.mote,avg.time = '10 min')
##########
#' ## Merging ALL data

data.merged <- merge(data.BAM.10min,
                     data.odin.10min,
                     by = 'date',
                     all = TRUE)
data.merged <- merge(data.merged,
                     data.grimm.10min,
                     by = 'date',
                     all = TRUE)
data.merged <- merge(data.merged,
                     data.mote.10min,
                     by = 'date',
                     all = TRUE)
#data.merged

#' # Results

#timePlot(data.merged,pollutant = c('PM2.5.BAM','PM2.5.0074'),avg.time = '1 hour',group = TRUE)
