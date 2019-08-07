# Download ODIN data from Hologram Cloud and save ODIN_full.RData

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
              #"0052",
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
x_start <- as.POSIXct("2018/10/29 12:00:00",tz = "GMT-12")
t_start <- as.numeric(x_start)
# UTC time end ... now
x_end <- as.POSIXct("2018/12/23 12:00:00",tz = "GMT-12")
t_end <- floor(as.numeric(x_end))
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

print("Data fetched")
print(ndata <- length(jreq2))

# We'll do this in parallel because it takes A LONG time with a few 100k records
#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(4) #not to overload your computer
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
                      x_payload <- fromJSON(xxx)
                      # x_payload <- try(fromJSON(xxx),silent=TRUE)
                      # if (inherits(x_payload,"try-error")) {
                      #   c_data
                      #   next
                      # }
                      payload <- unlist(x_payload)
                      # if (length(payload)<5){
                      #   c_data
                      #   next
                      # }
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
# Remove index
all_data$id <- NULL
print("Data parsed")
print(length(all_data$date))
#Add ODIN-0052 from offline file
# Read data file

odin.52.data <- read.delim("/home/gustavo/data/Waterview2018/OFFLINE DOWNLOAD/waterview_data/ODIN/DATA_SD0052.TXT",
                           sep = ';',
                           header=FALSE)


names(odin.52.data) <- c('framelength',
                      'PM1',
                      'PM2.5',
                      'PM10',
                      'PM1x',
                      'PM2.5x',
                      'PM10x',
                      'GAS1sn',
                      'GAS1',
                      'Tgas1',
                      'GAS2',
                      'Temperature',
                      'RH',
                      'deviceid',
                      'Serialn',
                      'Day',
                      'Time',
                      'v1','v2','v3','v4','v5')

odin.52.data$PMc <- odin.52.data$PM10 - odin.52.data$PM2.5
odin.52.data$tags <- "offline"

odin.52.data$date <- as.POSIXct(paste(odin.52.data$Day,
                                      odin.52.data$Time),
                                format = '%Y/%m/%d %H:%M:%S',
                                tz='UTC')
odin.52.data$timestamp <- as.POSIXct(paste(odin.52.data$v1,
                                           odin.52.data$v2),
                                     format = '%Y/%m/%d %H:%M:%S',
                                     tz='UTC')

odin.52.data <- odin.52.data[,c("PM1",
                                "PM2.5",
                                "PM10",
                                "PMc",
                                "GAS1",
                                "Tgas1",
                                "GAS2",
                                "Temperature",
                                "RH",
                                "date",
                                "timestamp",
                                "deviceid",
                                "tags")]
odin.52.data$deviceid <- 165088
print("ODIN 52 done")
all_data <- rbind(all_data,
                  odin.52.data)
all_data$serialn <- NA
device_ids <- unique(all_data$deviceid)
for (i in device_ids){
  all_data$serialn[all_data$deviceid==i] <- subset(curr_data,deviceid==i)$ODIN
}

print(min(all_data$timestamp))
print(max(all_data$timestamp))
names(all_data)

# Fix wrong dates
# Clock not setup ... wrong date ... replace with server logging date
wrong_dates <- which(is.na(all_data$date) | (all_data$date <= as.POSIXct("2018/01/01")) | all_data$date > as.POSIXct(Sys.time()))
tmp_error_catching <- try(all_data$date[wrong_dates] <- all_data$timestamp[wrong_dates],
                          silent = TRUE)
# Clock in device ahead of server logging time ... wrong date ... replace with server logging date
wrong_dates <- which((all_data$date - all_data$timestamp) > 0)
tmp_error_catching <- try(all_data$date[wrong_dates] <- all_data$timestamp[wrong_dates],
                          silent = TRUE)
# No timestamp and no clock ... wrong date ... catchall step, replace with NA
wrong_dates <- which(all_data$date <= as.POSIXct("2018/01/01"))
tmp_error_catching <- try(all_data$date[wrong_dates] <- NA,
                          silent = TRUE)

# Remove data where PM2.5 > PM10 (logging problem)
remove_these <- which(all_data$PMc<0)
all_data[remove_these,c(1,2,3,4,5,6)] <- NA

ggplot(data = all_data, aes(x=date)) +
  geom_point(aes(y=PM2.5,colour = serialn))




# Calculate averaged time series
cl <- makeCluster(4) #not to overload your computer
registerDoParallel(cl)

all_data.tavg <- foreach(i=1:length(device_ids),
                         .packages=c("openair"),
                         .combine=rbind,
                         .errorhandling = 'remove') %dopar%
                         {
                           device_now <- subset(curr_data,deviceid==device_ids[i])
                           some_data <- subset(all_data, serialn == device_now$ODIN)
                           avg_data <- timeAverage(some_data,
                                                   avg.time = time_avg,
                                                   start.date = strftime(x_start, format = "%Y-%m-%d %H:00:00"))
                           avg_data$serialn <- device_now$ODIN
                           avg_data
                         }

stopCluster(cl)

ggplot(data = all_data.tavg, aes(x=date)) +
  geom_point(aes(y=PM2.5,colour = serialn))

# Save data for later use
save(all_data,all_data.tavg,file = 'ODIN_full.RData')
