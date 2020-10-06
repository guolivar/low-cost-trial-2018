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

if (!file.exists('./ODIN_full.RData')){
  source('download_odin_hologram.R')
}
load('ODIN_full.RData')

# Working dataset ###
long.data.odin <- all_data[,c("date","PM2.5","Temperature","RH","serialn")]
wide.PM2.5.data.odin <- reshape2::dcast(long.data.odin, date ~ serialn, value.var = "PM2.5",fun.aggregate = mean)
names(wide.PM2.5.data.odin) <- c("date", paste0("PM2.5_",substr(names(wide.PM2.5.data.odin)[2:8],start = 6,stop = 9)))
wide.Temp.data.odin <- reshape2::dcast(long.data.odin, date ~ serialn, value.var = "Temperature",fun.aggregate = mean)
names(wide.Temp.data.odin) <- c("date", paste0("T_",substr(names(wide.Temp.data.odin)[2:8],start = 6,stop = 9)))
wide.RH.data.odin <- reshape2::dcast(long.data.odin, date ~ serialn, value.var = "RH",fun.aggregate = mean)
names(wide.RH.data.odin) <- c("date", paste0("RH_",substr(names(wide.RH.data.odin)[2:8],start = 6,stop = 9)))

data.odin <- merge(wide.PM2.5.data.odin,wide.Temp.data.odin,by='date',all=TRUE)
data.odin <- merge(data.odin,wide.RH.data.odin,by='date',all=TRUE)

data.odin.10min <- timeAverage(data.odin,avg.time = '1 min',
                               start.date = strftime(min(data.odin$date, na.rm = TRUE),format = "%Y-%m-%d 00:00:00",tz="UTC"))
data.odin.10min$PM2.5.odin <- rowMeans(data.odin.10min[,c(2,3,6,7)],na.rm = TRUE)
data.odin.10min$T.odin <- rowMeans(data.odin.10min[,c(9,10,13,14)],na.rm = TRUE)
data.odin.10min$RH.odin <- rowMeans(data.odin.10min[,c(16,17,20,21)],na.rm = TRUE)


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
data.grimm.10min <- timeAverage(rawdata.grimm,avg.time = '1 min',
                                start.date = strftime(min(rawdata.grimm$date, na.rm = TRUE),format = "%Y-%m-%d 00:00:00",tz="UTC"))

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
data.mote.10min <- timeAverage(rawdata.mote,avg.time = '1 min',
                               start.date = strftime(min(rawdata.mote$date, na.rm = TRUE),format = "%Y-%m-%d 00:00:00",tz="UTC"))
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
data.merged <- timeAverage(data.merged,avg.time = '10 min')

#' # Results
# Start date
date1 <- "2018-11-01"
date2 <- "2018-12-03"
timePlot(selectByDate(data.merged,start=date1,end=date2),pollutant = names(data.merged)[c(2,24,29,36)],avg.time = '15 min', group = TRUE)
timePlot(selectByDate(data.merged,start=date1,end=date2),pollutant = names(data.merged)[c(24,2,29,36)],avg.time = '15 min',group = TRUE)

timePlot((data.merged),pollutant = names(data.merged)[c(12,13,16)],avg.time = '15 min',group = TRUE)

scatterPlot(data.merged,x = "PM2.5.grimm",y = "PM2.5.BAM",linear = TRUE, avg.time = '1 hour')
scatterPlot(data.merged,x = "PM2.5.mote",y = "PM2.5.BAM",linear = TRUE, avg.time = '1 hour')

scatterPlot(data.merged,x = "PM2.5.odin",y = "PM2.5.BAM",linear = TRUE, avg.time = '1 hour')
scatterPlot(selectByDate(data.merged,start=date1,end=date2),x = "PM2.5.odin",y = "PM2.5.mote",linear = TRUE, avg.time = '1 hour')

for ( i in c(3,4,7,8)){
  scatterPlot(data.merged,x = names(data.merged)[i], y = "PM2.5.BAM" ,linear = TRUE)
  invisible(readline(prompt="Press [enter] to continue"))
}


# Indoor ODINs  52, 53, 74, 75
# Indices       3   4   7   8
# Outdoor ODNIs 59, 72, 76
# Indices       5   6   9

scatterPlot(data.merged,x = "PM2.5.odin", y = "PM2.5.BAM" ,linear = TRUE, type = 'hour')


scatterPlot(data.merged,x = "PM2.5.BAM",y = "PM2.5.odin",
            linear = TRUE, avg.time = '1 hour',
            ylim=c(-5,60),xlim=c(-5,110),
            cols='blue',pch=19)
scatterPlot(data.merged,x = "PM2.5.BAM",y = "PM2.5.mote",
            linear = TRUE, avg.time = '1 hour',
            ylim=c(-5,60),xlim=c(-5,110),
            cols='blue',pch=19)
scatterPlot(data.merged,x = "PM2.5.BAM",y = "PM2.5.grimm",
            linear = TRUE, avg.time = '1 hour',
            ylim=c(-5,60),xlim=c(-5,110),
            cols='blue',pch=19)

timeVariation(data.merged,pollutant = "PM2.5.BAM")


## Scatter plot of ALL PM2.5 estimates against BAM
data.merged.1hr <- timeAverage(data.merged, avg.time = '1 hour')
sp <- ggplot(data = data.merged, aes(x=PM2.5.BAM)) +
  geom_point(aes(y=PM2.5.mote),colour = 'green') +
  geom_point(aes(y=PM2.5.odin),colour = 'red') +
  geom_point(aes(y=PM2.5.grimm),colour = 'black') +
  xlim(c(0,150)) +
  ylim(c(0,50)) +
  scale_color_manual(values=c("green","red","black"))
sp

plot_data <- reshape(data.merged,varying = c('PM2.5.mote','PM2.5.odin','PM2.5.grimm'),
                     timevar = 'PM2.5.BAM',
                     direction = 'long')
