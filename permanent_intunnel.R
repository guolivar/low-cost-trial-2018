#' ---
#' title: "In-Tunnel data from permanent sensors"
#' author: Gustavo Olivares
#' output: html_document
#' ---

## Libraries
library(readr)
library(readxl)
library(openair)

## Paths
data.path <- path.expand('~/data/Waterview2018/WTJO_data/Indoor/')
data.files <- dir(data.path,pattern = 'xlsx')

## Read data
t_base <- '10 min'
## First file
## CO
co.data <- read_excel(paste0(data.path,data.files[1]),
                      'SAQ801_CO',
                      skip = 2)
## NO
no.data <- read_excel(paste0(data.path,data.files[1]),
                      'SAQ801_NO',
                      skip = 2)
## NO2
no2.data <- read_excel(paste0(data.path,data.files[1]),
                       'SAQ801_NO2',
                       skip = 2)
## Vis
vis.data <- read_excel(paste0(data.path,data.files[1]),
                       'SAQ801_VIS',
                       skip = 2)
# The rest
for (i in (2:length(data.files))){
  ## CO
  co.tmp <- read_excel(paste0(data.path,data.files[i]),
                        'SAQ801_CO',
                        skip = 2)
  co.data <- rbind(co.data,co.tmp)
  ## NO
  no.tmp <- read_excel(paste0(data.path,data.files[i]),
                        'SAQ801_NO',
                        skip = 2)
  no.data <- rbind(no.data,no.tmp)
  ## NO2
  no2.tmp <- read_excel(paste0(data.path,data.files[i]),
                         'SAQ801_NO2',
                         skip = 2)
  no2.data <- rbind(no2.data,no2.tmp)
  ## Vis
  vis.tmp <- read_excel(paste0(data.path,data.files[i]),
                         'SAQ801_VIS',
                         skip = 2)
  vis.data <- rbind(vis.data,vis.tmp)
}
names(co.data) <- c('date','CO')
names(no.data) <- c('date','NO')
names(no2.data) <- c('date','NO2')
names(vis.data) <- c('date','VIS')
co.avg <- timeAverage(co.data,avg.time = t_base)
no.avg <- timeAverage(no.data,avg.time = t_base)
no2.avg <- timeAverage(no2.data,avg.time = t_base)
vis.avg <- timeAverage(vis.data,avg.time = t_base)
# Merge data
data.intunnel <- merge(co.avg,no.avg,by = 'date', all = TRUE)
data.intunnel <- merge(data.intunnel,no2.avg,by = 'date', all = TRUE)
data.intunnel <- merge(data.intunnel,vis.avg,by = 'date', all = TRUE)
