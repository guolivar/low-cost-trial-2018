library(librarian) # To more flexibly manage packages
shelf(readr,
      reshape2,
      ggplot2,
      scales,
      gstat,
      zoo,
      openair,
      stringi,
      viridis,
      dplyr,
      RColorBrewer,
      purrr,
      magick)
# Load Data ####
load('./to_plot_odin.RData')
# Plot data ####
avg_plot <- '10 min'
i_dev <- 1
for (i_dev in (1:nodins)) {
  plot_data <- subset(all_data,(serialn == odin_names[i_dev] & date < as.POSIXct("2018-12-25 21:00:00",tz = 'GMT')))
  if (length(plot_data$date) < 2){
    next
  }
  timePlot(plot_data,
           pollutant = c('PM1','PM2.5','PM10'),
           main = paste(odin_names[i_dev],"\nIn Tunnel"),
           avg.time = avg_plot,
           group = TRUE,
           ylim=c(0,30))
}

for (i_dev in (1:nodins)) {
  plot_data <- subset(all_data,(serialn == odin_names[i_dev] & date > as.POSIXct("2018-12-25 21:00:00",tz = 'GMT')))
  if (length(plot_data$date) < 2){
    next
  }
  timePlot(plot_data,
           pollutant = c('PM1','PM2.5','PM10'),
           main = paste(odin_names[i_dev],"\nOutdoor"),
           avg.time = avg_plot,
           group = TRUE,
           ylim=c(0,30))
}

i_dev <- 1
for (i_dev in (1:nodins)) {
  plot_data <- subset(all_data,(serialn == odin_names[i_dev] & date < as.POSIXct("2018-12-25 21:00:00",tz = 'GMT')))
  if (length(plot_data$date) < 2){
    next
  }
  timeVariation(plot_data,
                pollutant = c('PM1','PM2.5','PM10'),
                main = paste(odin_names[i_dev],"\nIn Tunnel"))
}

for (i_dev in (1:nodins)) {
  plot_data <- subset(all_data,(serialn == odin_names[i_dev] & date > as.POSIXct("2018-12-25 21:00:00",tz = 'GMT')))
  if (length(plot_data$date) < 2){
    next
  }
  timeVariation(plot_data,
                pollutant = c('PM1','PM2.5','PM10'),
                main = paste(odin_names[i_dev],"\nOutdoor"))
}


