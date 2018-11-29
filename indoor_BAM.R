#' ---
#' title: "Indoor BAM online"
#' author: Gustavo Olivares
#' output: pdf_document
#' ---

#' ## Prepare the environment

# Fetch data from several units

##### Load relevant packages #####
library(librarian) # To more flexibly manage packages
shelf(readr,
      reshape2,
      automap,
      raster,
      gstat,
      sp,
      rgdal,
      ggmap,
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
read_address <- "http://penap-data.dyndns.org:8080/output/34Gk0X92B9IKwZ1qN1Laivwr8PQ.json"

## Get the timeseries data #####
# Get the data #####
req1 <- curl_fetch_memory(read_address)
jreq1 <- fromJSON(rawToChar(req1$content))

# UTC time start
x_now <- Sys.time()
print(x_now)
# UTC time start
t_start <- as.numeric(as.POSIXct("2018/10/25 12:00:00",tz = "GMT-12"))
# UTC time end ... now
t_end <- floor(as.numeric(x_now))
# Set the averaging interval
time_avg <- '15 min'

xx2 <- unlist(jreq1)
ndata <- length(xx2)
data.bam <- data.frame(date = as.POSIXct(xx2[seq(2,ndata,2)],format="%Y-%m-%dT%H:%M:%OS", tz='UTC'))
data.bam$PM2.5 <- as.numeric(xx2[seq(1,ndata,2)])

# Plot data ####
avg_plot <- '5 min'

timePlot(data.bam,
         pollutant = c('PM2.5'),
         main = "BAM In Tunnel",
         ylim = c(0,150),
         avg.time = avg_plot)

timeVariation(data.bam,
              pollutant = c('PM2.5'))

