# Random plots for report

# Sample scatterplot
library(ggplot2)
set.seed(1)
nobs <- 700
plot_data <- data.frame(id = (1:nobs))
plot_data$reference <- round(rweibull(700,1.1,20))
plot_data$readings <- plot_data$reference +
  + sample(rnorm(nobs,sd = 5),n_ref,replace = TRUE) 
  + 0.1/sample(rnorm(nobs,sd = 5),n_ref,replace = TRUE)^2

ggplot(plot_data,aes(x=reference, y=readings)) +
  geom_point() +
  xlab("Reference readings") +
  ylab("Sensor readings")

# Binning data for repeatability
cut_levels <- c(-1,0,5,10,20,50,200)
plot_data$ref.cat <- cut(plot_data$reference,
                         breaks = cut_levels)
cut_labels <- format(tapply(plot_data$reference, plot_data$ref.cat, mean),digits = 2)
plot_data$ref.cat <- cut(plot_data$reference,
                         breaks = cut_levels,
                         labels = cut_labels)

ggplot(plot_data,aes(x=ref.cat, y=readings)) +
  geom_boxplot() +
  xlab("Reference readings") +
  ylab("Sensor readings")

