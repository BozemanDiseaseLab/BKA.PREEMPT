#' A Function for auto-generating control ratio plots.
#' Designed for data outputed from soft.max.clean function
#'
#' @param data Data formatted per soft.max.clean output.
#' @param positive.control Name of positive control, as string.
#' @export
#' @examples
#' control.ratio.plot()

control.ratio.data <- function(data, positive.control)
{
  library(tidyverse)
  #data <- bka.merge
  
  data_pos <- data[data$sample == positive.control, ]
  data_sample <- data[data$sample != positive.control, ]
  
  
  
  data_0 <- na.omit(data[data$time == 0, ])
  data_n0 <- na.omit(data[data$time != 0, ])

  data.join <- full_join(data_0, data_n0, by =c('which_row', 'column', 'sample', 'bacteria', 'experiment_id'))
  
  # nrow(data_0) + nrow(data_n0)
  # nrow(data.join)
  
  data.join$deltameasure = as.numeric(data.join$measure.y) - as.numeric(data.join$measure.x)
  
  #positive.control = 'bacteriaonly'
  data_pos <- data.join[data.join$sample == positive.control, ]
  data_sample <- data.join[data.join$sample != positive.control, ]
  
  data_pos <- data_pos[,-which(names(data_pos) %in% c('measure.y', 'measure.x'))]
  data_sample <- data_sample[,-which(names(data_sample) %in% c('measure.y', 'measure.x'))]
  
  data_pos <- data_pos %>%
    group_by(sample, bacteria, experiment_id, time.y) %>%
    summarise(mean = mean(deltameasure), variance = var(deltameasure))
  
  data_sample <- data_sample %>%
    group_by(sample, bacteria, experiment_id, time.y) %>%
    summarise(mean = mean(deltameasure), variance = var(deltameasure))
  
  data.join <- full_join(data_pos,data_sample,by = c( "bacteria", "experiment_id", "time.y"))
  nrow(data_pos)  + nrow(data_sample)
  nrow(data.join)
  
  data.join$deltaratio <-   data.join$mean.x / data.join$mean.y
  #hist(data.join$deltaratio)
  
  return(data.join)
  
}
