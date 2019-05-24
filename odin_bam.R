#' ---
#' title: "In-Tunnel ODIN + BAM + Grimm"
#' author: Gustavo Olivares
#' output: html_document
#' ---


#'# Purpose
#'The goal of this report is to document the comparison between the ODIN, 
#'the Grimm and the BAM when deployed in the Waterview tunnel during 2018.

##### Load relevant packages #####
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
##### Define paths and strings ####
data_path <- path.expand("~/data/Waterview2018/")
##### Load Data ####
# BAM
bam_data <- read_csv(paste0(data_path,"BAM/tunnel/bam_data.txt"), 
                     col_types = cols(date = col_datetime(format = "%Y/%m/%d %H:%M:%S GMT")))
## Remove date after 2018-11-21 21:00:38 UTC
bam_data <- subset(bam_data, date < '2018-11-22 10:00:00')

# Grimm
grimm_data <- read_delim(paste0(data_path,"Grimm/tunnel/parsed_grimm_data.txt"),
                         delim = '\t')
grimm_data$date <- with(grimm_data,as.POSIXct(paste0("20",Year,"/",
                                                     Month,"/",
                                                     Day," ",
                                                     Hour,":",
                                                     Minute,":",
                                                     Second))) - 12*3600
## Remove date before 2018-10-29 00:00:00
grimm_data <- subset(grimm_data, date > '2018-10-29 00:00:00')

# ODIN

# Read the secrets
secret_hologram <- read_delim("./secret_hologram.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
# Get the tag list
base_url <- "https://dashboard.hologram.io/api/1/devices/tags?"
built_url <- paste0(base_url,
                    "orgid=",secret_hologram$orgid,"&",
                    "apikey=",secret_hologram$apikey)
req1 <- curl_fetch_memory(built_url)
jreq1 <- fromJSON(rawToChar(req1$content))$data$tags
ntags <- length(jreq1)
all_tags <- data.frame(id = (1:ntags),name = NA,topic = NA)

for (i in (1:ntags)){
  all_tags$id[i] <- jreq1[[i]]$id
  all_tags$name[i] <- jreq1[[i]]$name
  all_tags$topic[i] <- paste0("_TAG_",jreq1[[i]]$id,"_")
}
wanted_tags_human <- c("waterview_outdoor","waterview","waterview_indoor")
tags <- subset(all_tags,name %in% wanted_tags_human)
wanted_tags <-tags$topic[1]
for (i in (2:length(tags))){
  wanted_tags <- paste0(wanted_tags,",",tags$topic[i])
}


## Get the timeseries data #####

# UTC time start
t_start <- as.numeric(as.POSIXct("2018/11/01 00:00:00",tz = "GMT-12"))
# UTC time end ... now
t_end <- as.numeric(as.POSIXct("2018/11/23 00:00:00",tz = "GMT-12"))
# Set the averaging interval

ndata <- 1
nstep <- 1
print("Getting data")
base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"

while (ndata >= 1){
  if (nstep == 1){
    built_url <- paste0(base_url,
                        "topicnames=",wanted_tags,"&",
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
                        "topicnames=",wanted_tags,"&",
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
  startat <- fromJSON(rawToChar(req2$content))$lastid
  nstep <- nstep + 1
  print(jreq2_tmp[[ndata]]$logged)
}

ndata <- length(jreq2)
print("Got data")

c_data <- data.frame(id = (1:ndata))
c_data$PM1 <- NA
c_data$PM2.5 <- NA
c_data$PM10 <- NA
c_data$PMc <- NA
c_data$GAS1 <- NA
c_data$Tgas1 <- NA
c_data$GAS2 <- NA
c_data$Temperature <- NA
c_data$RH <- NA
c_data$date <- as.POSIXct(jreq2[[ndata]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
c_data$timestamp <- c_data$date
c_data$serialn <- NA

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
  c_data$date[i] <- as.POSIXct(as.character(payload[9]),format = "%Y/%m/%d;%H:%M:%S",tz="UTC")
  c_data$timestamp[i] <- as.POSIXct(jreq2[[i]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
  c_data$serialn <- fromJSON(jreq2[[i]]$data)$device_id
}

wrong_dates <- which(is.na(c_data$date) | (c_data$date <= as.POSIXct("2018/01/01")) | c_data$date > as.POSIXct(Sys.time()))
tmp_error_catching <- try(c_data$date[wrong_dates] <- c_data$timestamp[wrong_dates],
                          silent = TRUE)
wrong_dates <- which(c_data$date <= as.POSIXct("2010/01/01"))
tmp_error_catching <- try(c_data$date[wrong_dates] <- NA,
                          silent = TRUE)
# Merge ODIN and BAM data
merged.data <- merge(c_data,bam_data,by = 'date', all = TRUE)
# Add Grimm data
merged.data <- merge(merged.data,grimm_data,by = 'date', all = TRUE)
merged.data <- merged.data[,c(1,3,4,5,6,10,11,13,14,29,30,31,32,33)]
names(merged.data) <- c('date',
                        'PM1.ODIN',
                        'PM2.5.ODIN',
                        'PM10.ODIN',
                        'PMc.ODIN',
                        'Temperature',
                        'RH',
                        'SerialN',
                        'PM2.5.BAM',
                        'TSP.Grimm',
                        'PM10.Grimm',
                        'PM2.5.Grimm',
                        'PM1.Grimm',
                        'N300.Grimm')
timePlot(merged.data,pollutant = c('PM2.5.ODIN','PM2.5.BAM','PM2.5.Grimm'),avg.time = '1 hour', group = TRUE)
