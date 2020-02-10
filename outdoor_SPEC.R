#' ---
#' title: "Outdoor SPEC sensors online"
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
      lubridate,lib = '/tmp')

# Read key
read_address <- "http://penap-data.dyndns.org:8080/output/4mWeal6PZVSlE1kyNkQdCvVadDE.json"

## Get the timeseries data #####
# Get the Ecotech data #####
data_path <- path.expand("~/data/Waterview2018/WTC_data/Outdoor/201812_north_portal.txt")
all.data.portal <- read_delim(data_path, 
                          "\t", 
                          escape_double = FALSE,
                          col_types = cols(DateTime = col_character(),
                                           `PM₁₀` = col_integer(),
                                           `PM₁₀ 24Hr Rolling` = col_integer()),
                          trim_ws = TRUE)
names(all.data.portal) <- c('date','NO','NO2','NOx','PM2.5','PM10','PM2.5.24hr','PM10.24hr','ws','wd')
all.data.portal$date <- with_tz(as.POSIXct(all.data.portal$date, format = '%d/%m/%Y %H:%M',tz='Pacific/Auckland'),"UTC")
data.portal <- subset(all.data.portal,(date > "2018-12-13")&(date<"2018-12-22"))

# Get the SPEC data #####
req1 <- curl_fetch_memory(read_address)
jreq1 <- fromJSON(rawToChar(req1$content))

# UTC time start
x_now <- Sys.time()
print(x_now)
# UTC time start
t_start <- as.numeric(as.POSIXct("2018/12/13 20:00:00",tz = "GMT-12"))
# UTC time end ... now
t_end <- floor(as.numeric(x_now))
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

data.spec <- subset(data.spec,date>as.POSIXct(t_start,origin = '1970-01-01 00:00'))

# Merge data
names(data.portal) <- c('date','NO','NO2','NOx','PM2.5','PM10','PM2.5.24hr','PM10.24hr','ws','wd')
names(data.spec) <- c('date','COspec','T_CO','NO2spec','T_NO2','SN_CO','SN_NO2','CO.NO2')

all.data <- merge(data.portal,data.spec,by = 'date',all = TRUE)
all.data.1hr <- timeAverage(all.data,avg.time = '1 hour')

# Plot data ####

timePlot(all.data.1hr,
         pollutant = c('NO2','NO2spec'),
         main = "NO2 sensors outdoor",
         y.relation = "free",
         group = TRUE)


timePlot(data.spec,
         pollutant = c('T_CO','T_NO2'),
         name.pol = c('Temp_CO','Temp_NO2'),
         main = "SPEC sensors in Tunnel",
         group = TRUE,
         avg.time = avg_plot)


timeVariation(all.data.1hr,
              pollutant = c('NO2','NO2spec','T_NO2'))

timeVariation(data.spec,
              pollutant = c('CO'))

timeVariation(data.spec,
              pollutant = c('CO.NO2'))

timePlot(data.spec,
         pollutant = c('CO','NO2'),
         main = "SPEC sensors In Portal",
         y.relation = "free",
         avg.time = avg_plot)

timePlot(data.spec,
         pollutant = c('CO','NO2'),
         main = "SPEC sensors In Portal",
         y.relation = "free",
         avg.time = avg_plot)

timePlot(data.spec,
         pollutant = c('NO2'),
         main = "SPEC sensors In Portal",
         y.relation = "free",
         avg.time = avg_plot)
