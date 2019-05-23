#' ---
#' title: "In-Tunnel ODIN"
#' author: Gustavo Olivares
#' output: pdf_document
#' ---

#' ## Prepare the environment

# Fetch data from several units

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
      magick,
      foreach,
      doParallel)

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
wanted_tags <-paste(tags$topic,collapse = ",")
print(wanted_tags)

# Fetch the ODIN names
base_url <- "https://dashboard.hologram.io/api/1/devices?"
built_url <- paste0(base_url,
                    "limit=500&",
                    "orgid=",secret_hologram$orgid,"&",
                    "apikey=",secret_hologram$apikey)
req1 <- curl_fetch_memory(built_url)
jreq1 <- fromJSON(rawToChar(req1$content))$data
ndevices <- length(jreq1)
all_devices <- data.frame(id = (1:ndevices),name = NA)

for (i in (1:ndevices)){
  all_devices$id[i] <- jreq1[[i]]$id
  all_devices$name[i] <- jreq1[[i]]$name
}

## Get the timeseries data #####
# UTC time start
x_now <- Sys.time()
print(x_now)
# UTC time start
t_start <- as.numeric(as.POSIXct("2018/10/29 12:00:00",tz = "GMT-12"))
# UTC time end ... now
t_end <- as.numeric(as.POSIXct("2019/03/01 12:00:00",tz = "GMT-12"))
# Set the averaging interval
time_avg <- '15 min'

ndata <- 1
nstep <- 1
print("Getting data")
base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"

while (ndata >= 1){
  if (nstep == 1){
    print("First 1000 fetch")
    print(nstep)
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
    print("Next 1000 fetch")
    print(nstep)
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

# We'll do this in parallel because it takes A LONG time with a few 100k records
#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-3) #not to overload your computer
registerDoParallel(cl)

all_data <- foreach(i=1:ndata,.packages=c("base64enc","RJSONIO"),.combine=rbind) %dopar%
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

all_data$serialn <- NA
device_ids <- unique(all_data$deviceid)
for (i in device_ids){
  all_data$serialn[all_data$deviceid==i] <- subset(all_devices,id==i)$name
}

# Remove index
all_data$id <- NULL
print(min(all_data$timestamp))
print(max(all_data$timestamp))
names(all_data)

wrong_dates <- which(is.na(all_data$date) | (all_data$date <= as.POSIXct("2018/01/01")) | all_data$date > as.POSIXct(Sys.time()))
tmp_error_catching <- try(all_data$date[wrong_dates] <- all_data$timestamp[wrong_dates],
                          silent = TRUE)
wrong_dates <- which(all_data$date <= as.POSIXct("2010/01/01"))
tmp_error_catching <- try(all_data$date[wrong_dates] <- NA,
                          silent = TRUE)

ggplot(data = all_data,aes(x=date)) +
  geom_line(aes(y = PM10,colour = serialn)) +
  ylim(0,120)

odin_names <- unique(all_data$serialn)
nodins <- length(odin_names)
# wide data
wide_data <- merge(subset(all_data,serialn == odin_names[1]),
                   subset(all_data,serialn == odin_names[2]),
                   by = "date",
                   all=TRUE)
names(wide_data) <- c('date',
                     paste0(names(all_data)[c(1:9,11:14)],'.',substr(odin_names[1],6,9)),
                     paste0(names(all_data)[c(1:9,11:14)],'.',substr(odin_names[2],6,9)))
for (i_dev in (3:nodins)) {
  c_names <- names(wide_data)
  wide_data <- merge(wide_data,
                     subset(all_data,serialn == odin_names[i_dev]),
                     by = "date",
                     all=TRUE)
  names(wide_data) <- c(c_names,
                        paste0(names(all_data)[c(1:9,11:14)],'.',substr(odin_names[i_dev],6,9)))
}

save.image('./to_plot_odin.RData',compress = 'xz')
save(all_data,all_devices,all_tags,wide_data,file = './odin_hologram.RData',compress = 'xz')
