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
      magick)

# Read key
read_address <- "http://penap-data.dyndns.org:8080/output/4mWeal6PZVSlE1kyNkQdCvVadDE.json"

## Get the timeseries data #####
# Get the data #####
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
# Plot data ####
avg_plot <- '5 min'
data.spec <- subset(data.spec,date>as.POSIXct(t_start,origin = '1970-01-01 00:00'))
timePlot(data.spec,
         pollutant = c('CO','NO2','CO.NO2'),
         main = "SPEC sensors In Tunnel",
         y.relation = "free",
         avg.time = avg_plot)

timePlot(data.spec,
         pollutant = c('NO2'),
         main = "SPEC sensors In Tunnel",
         ylim = c(-10,50),
         avg.time = avg_plot)

timePlot(data.spec,
         pollutant = c('T_CO','T_NO2'),
         name.pol = c('Temp_CO','Temp_NO2'),
         main = "SPEC sensors in Tunnel",
         group = TRUE,
         avg.time = avg_plot)


timeVariation(data.spec,
              pollutant = c('NO2'))

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
