#' ---
#' title: "Risk assessment of tunnel air"
#' author: Gustavo Olivares
#' output: pdf_document
#' ---

#' ## Prepare the environment

library('librarian')
shelf(openair,
      readr,
      ggplot2,
      readxl)

data_path <- path.expand('~/data/Waterview2018/AIR QUALITY SENSORS-501-502-503/')

#' ## Read the data
#' 
#' ### Site 501

input1 <- read_excel(paste0(data_path,'SAQ501_May 2018.xlsx'),
                     sheet = 1,
                     skip = 2)
names(input1) <- c('date','CO.501')
data.501 <- input1

input1 <- read_excel(paste0(data_path,'SAQ501_May 2018.xlsx'),
                     sheet = 2,
                     skip = 2)
names(input1) <- c('date','NO.501')
data.501 <- merge(data.501,input1,
                  by = 'date',
                  all = TRUE)

input1 <- read_excel(paste0(data_path,'SAQ501_May 2018.xlsx'),
                     sheet = 3,
                     skip = 2)
names(input1) <- c('date','NO2.501')
data.501 <- merge(data.501,input1,
                  by = 'date',
                  all = TRUE)

input1 <- read_excel(paste0(data_path,'SAQ501_May 2018.xlsx'),
                     sheet = 4,
                     skip = 2)
names(input1) <- c('date','VIS.501')
data.501 <- merge(data.501,input1,
                  by = 'date',
                  all = TRUE)


#' ### Site 502

input1 <- read_excel(paste0(data_path,'SAQ502_May 2018.xlsx'),
                     sheet = 1,
                     skip = 2)
names(input1) <- c('date','CO.502')
data.502 <- input1

input1 <- read_excel(paste0(data_path,'SAQ502_May 2018.xlsx'),
                     sheet = 2,
                     skip = 2)
names(input1) <- c('date','NO.502')
data.502 <- merge(data.502,input1,
                  by = 'date',
                  all = TRUE)

input1 <- read_excel(paste0(data_path,'SAQ502_May 2018.xlsx'),
                     sheet = 3,
                     skip = 2)
names(input1) <- c('date','NO2.502')
data.502 <- merge(data.502,input1,
                  by = 'date',
                  all = TRUE)

input1 <- read_excel(paste0(data_path,'SAQ502_May 2018.xlsx'),
                     sheet = 4,
                     skip = 2)
names(input1) <- c('date','VIS.502')
data.502 <- merge(data.502,input1,
                  by = 'date',
                  all = TRUE)

#' ### Site 503

input1 <- read_excel(paste0(data_path,'SAQ503_May 2018.xlsx'),
                     sheet = 1,
                     skip = 2)
names(input1) <- c('date','CO.503')
data.503 <- input1

input1 <- read_excel(paste0(data_path,'SAQ503_May 2018.xlsx'),
                     sheet = 2,
                     skip = 2)
names(input1) <- c('date','NO.503')
data.503 <- merge(data.503,input1,
                  by = 'date',
                  all = TRUE)

input1 <- read_excel(paste0(data_path,'SAQ503_May 2018.xlsx'),
                     sheet = 3,
                     skip = 2)
names(input1) <- c('date','NO2.503')
data.503 <- merge(data.503,input1,
                  by = 'date',
                  all = TRUE)

input1 <- read_excel(paste0(data_path,'SAQ503_May 2018.xlsx'),
                     sheet = 4,
                     skip = 2)
names(input1) <- c('date','VIS.503')
data.503 <- merge(data.503,input1,
                  by = 'date',
                  all = TRUE)

all.data <- merge(data.501,data.502,
                  by = 'date',
                  all = TRUE)
all.data <- merge(all.data,data.503,
                  by = 'date',
                  all = TRUE)

#' ## Carbon Monoxide [ppm]
#' 
#' ### 15 Minutes
summary(timeAverage(all.data,avg.time = '15 min')[,c('CO.501','CO.502','CO.503')], digits = 2)
#' ### 30 Minutes
summary(timeAverage(all.data,avg.time = '30 min')[,c('CO.501','CO.502','CO.503')], digits = 2)
#' ### 1 Hour
summary(timeAverage(all.data,avg.time = '1 hour')[,c('CO.501','CO.502','CO.503')], digits = 2)
#' ### 8 Hour
summary(timeAverage(all.data,avg.time = '8 hour')[,c('CO.501','CO.502','CO.503')], digits = 2)

#' ## Nitric Oxide [ppm]
#' 
#' ### 15 Minutes
summary(timeAverage(all.data,avg.time = '15 min')[,c('NO.501','NO.502','NO.503')], digits = 2)
#' ### 1 Hour
summary(timeAverage(all.data,avg.time = '1 hour')[,c('NO.501','NO.502','NO.503')], digits = 2)
#' ### 8 Hour
summary(timeAverage(all.data,avg.time = '8 hour')[,c('NO.501','NO.502','NO.503')], digits = 2)

#' ## Nitrogen Dioxide [ppm]
#' 
#' ### 15 Minutes
summary(timeAverage(all.data,avg.time = '15 min')[,c('NO2.501','NO2.502','NO2.503')], digits = 2)
#' ### 1 hour
summary(timeAverage(all.data,avg.time = '1 hour')[,c('NO2.501','NO2.502','NO2.503')], digits = 2)
#' ### 8 hour
summary(timeAverage(all.data,avg.time = '8 hour')[,c('NO2.501','NO2.502','NO2.503')], digits = 2)
