#' ---
#' title: "PM2.5 in Waterview tunnel"
#' author: Gustavo Olivares
#' output: pdf_document
#' ---

#' ## Prepare the environment

# Load relevant packages #####
library(librarian) # To more flexibly manage packages
shelf(readr,
      reshape2,
      ggplot2,
      scales,
      gstat,
      RJSONIO,
      curl,
      base64enc,
      zoo,
      openair,
      stringi,
      viridis,
      dplyr,
      RColorBrewer,
      purrr,
      magick)

# Paths
data_path <- path.expand("~/data/Waterview2018/")

#' ## BAM

# Load data
rawdata.BAM <- read_csv("~/data/Waterview2018/BAM/bam_data.txt",
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

# Timeseries of raw data
timePlot(data.BAM,pollutant = 'pm2.5',
         ylim = c(-100,200),
         main = 'RAW PM2.5 1min data')
timePlot(data.BAM,pollutant = 'pm2.5',
         ylim = c(-100,200),
         avg.time = '1 hour',
         main = 'RAW PM2.5 1 hour average')
timeVariation(data.BAM,pollutant = 'pm2.5')

# Working dataset
data.BAM.10min <- timeAverage(data.BAM,avg.time = '10 min')[,c('date','pm2.5')]
names(data.BAM.10min) <- c('date','PM2.5.BAM')
#' ## ODIN

# Read the secrets for ODIN data
secret_hologram <- read_delim("./secret_hologram.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
# Get the devices ID #####
base_url <- "https://dashboard.hologram.io/api/1/devices?"
tag <- "waterview_indoor"
built_url <- paste0(base_url,
                    "orgid=",secret_hologram$orgid,"&",
                    "tagname=",tag,"&",
                    "apikey=",secret_hologram$apikey)
req1 <- curl_fetch_memory(built_url)
jreq1 <- fromJSON(rawToChar(req1$content))$data
nsites <- length(jreq1)
curr_data <- data.frame(deviceid = (1:nsites),ODIN = NA)
for (i in (1:nsites)){
  curr_data$deviceid[i] <- jreq1[[i]]$id
  curr_data$ODIN[i] <- jreq1[[i]]$name
}

x_dev <- curr_data$ODIN

## Get the timeseries data #####
# UTC time start
x_now <- Sys.time()
print(x_now)
# UTC time start
t_start <- as.numeric(as.POSIXct("2018/10/29 12:00:00",tz = "GMT-12"))
# UTC time end ... now
t_end <- floor(as.numeric(x_now))
# Set the averaging interval
time_avg <- '10 min'

ndata <- 1
nstep <- 1
print("Getting data")
base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"
for (i_dev in (1:nsites)){
  print(curr_data$ODIN[i_dev])
  nstep <- 1
  while (ndata >= 1){
    if (nstep == 1){
      built_url <- paste0(base_url,
                          "deviceid=",curr_data$deviceid[i_dev],"&",
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
                          "deviceid=",curr_data$deviceid[i_dev],"&",
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
    if (ndata < 1){
      break
    }
    startat <- jreq2_tmp[[ndata]]$id
    nstep <- nstep + 1
    print(jreq2_tmp[[ndata]]$logged)
  }
  
  ndata <- length(jreq2)
  if (ndata < 1){
    # This device didn't have data for this period
    next
  }
  c_data <- data.frame(id = (1:ndata))
  # c_data$PM1 <- NA
  c_data$PM2.5 <- NA
  # c_data$PM10 <- NA
  # c_data$PMc <- NA
  # c_data$GAS1 <- NA
  # c_data$Tgas1 <- NA
  # c_data$GAS2 <- NA
  # c_data$Temperature <- NA
  # c_data$RH <- NA
  c_data$date <- as.POSIXct(jreq2[[ndata]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
  c_data$timestamp <- c_data$date
  c_data$serialn <- curr_data$ODIN[i_dev]
  
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
    # c_data$PM1[i] <- as.numeric(payload[1])
    c_data$PM2.5[i] <- as.numeric(payload[2])
    # c_data$PM10[i] <- as.numeric(payload[3])
    # c_data$PMc[i] <- as.numeric(payload[3]) - as.numeric(payload[2])
    # c_data$GAS1[i] <- as.numeric(payload[4])
    # c_data$Tgas1[i] <- as.numeric(payload[5])
    # c_data$GAS2[i] <- as.numeric(payload[6])
    # c_data$Temperature[i] <- as.numeric(payload[7])
    # c_data$RH[i] <- as.numeric(payload[8])
    c_data$timestamp[i] <- as.POSIXct(as.character(payload[9]),format = "%Y/%m/%d;%H:%M:%S",tz="UTC")
    c_data$date[i] <- as.POSIXct(jreq2[[i]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
  }
  
  has_data <- try(length(c_data$date),silent = TRUE)
  if (inherits(has_data,"try-error")) {
    next
  }

  wrong_dates <- which(is.na(c_data$date) | (c_data$date <= as.POSIXct("2018/01/01")) | c_data$date > as.POSIXct(Sys.time()))
  tmp_error_catching <- try(c_data$date[wrong_dates] <- c_data$timestamp[wrong_dates],
                            silent = TRUE)
  wrong_dates <- which(c_data$date <= as.POSIXct("2010/01/01"))
  tmp_error_catching <- try(c_data$date[wrong_dates] <- NA,
                            silent = TRUE)
  if (i_dev==1){
    all_data <- c_data
    all_data.tavg <- timeAverage(c_data,avg.time = time_avg)
    all_data.tavg$serialn <- curr_data$ODIN[i_dev]
  } else {
    all_data <- rbind(all_data,c_data)
    c_data2 <- timeAverage(c_data,avg.time = time_avg)
    c_data2$serialn <- curr_data$ODIN[i_dev]
    all_data.tavg <- rbind(all_data.tavg,c_data2)
    rm(c_data2)
  }
  rm(c_data)
}
# wide data
data.odin.10min <- merge(subset(all_data.tavg,serialn == curr_data$ODIN[1]),
                   subset(all_data.tavg,serialn == curr_data$ODIN[2]),
                   by = "date",
                   all=TRUE)[,c(1,2,3,4,6,7,8)]
names(data.odin.10min) <- c('date',
                      paste0(names(all_data.tavg)[c(2,3,4)],'.',substr(curr_data$ODIN[1],6,9)),
                      paste0(names(all_data.tavg)[c(2,3,4)],'.',substr(curr_data$ODIN[2],6,9)))
for (i_dev in (3:nsites)) {
  print(i_dev)
  c_names <- names(data.odin.10min)
  data.odin.10min <- merge(data.odin.10min,
                     subset(all_data.tavg,serialn == curr_data$ODIN[i_dev])[,c(1,2,3,4)],
                     by = "date",
                     all=TRUE)
  names(data.odin.10min) <- c(c_names,
                        paste0(names(all_data.tavg)[c(2,3,4)],'.',substr(curr_data$ODIN[i_dev],6,9)))
}

#' ## Merging data

data.merged <- merge(data.BAM.10min,
                     data.odin.10min,
                     by = 'date',
                     all = TRUE)

#' # Results

timePlot(data.merged,pollutant = c('PM2.5.BAM','PM2.5.0074'),avg.time = '1 hour',group = TRUE)
