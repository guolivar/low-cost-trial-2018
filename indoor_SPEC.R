#' ---
#' title: "Indoor SPEC sensors online"
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
      magick)

# Read key
read_address <- "http://penap-data.dyndns.org:8080/output/4mWeal6PZVSlE1kyNkQdCvVadDE.json"

# Load API data
data.api <- read_delim("~/data/Waterview2018/APINOx/tunnel/LOF01.txt", 
                    "\t", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
start_date <- as.POSIXct("2018/11/21 22:15:00",tz = 'UTC')
idx_vec <- (1:length(data.api$X1)) - 1
data.api$date <- start_date + idx_vec

data.api.1min <- timeAverage(data.api,avg.time = '1 min')

## Get the timeseries data #####
# Get the data #####
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

#### Merge data ####
names(data.spec) <- c('date','COspec','T_CO','NO2spec','T_NO2','SN_CO','SN_NO2','CO.NO2')
names(data.api.1min) <- c('date','NOx','NO2raw','NO')

all.data <- merge(data.api.1min,data.spec,by = 'date',all = TRUE)
all.data$NO2 <- all.data$NO2raw * 0.63 - 58.8

#### Plot data ####

timeVariation(all.data,pollutant = c('NO2','NO2spec'))


