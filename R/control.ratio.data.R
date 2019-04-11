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
  data_0 <- na.omit(data[data$time == 0, ])
  #data_n0 <- na.omit(data[data$time != 0, ])

  data.join <- full_join(data_0, data, by =c('which_row', 'column', 'sample', 'bacteria', 'experiment_id'), suffix = c('.t0', '.t1_12'))
  
  # nrow(data_0) + nrow(data_n0)
  # nrow(data.join)
  
  data.join$deltameasure = as.numeric(data.join$measure.t1_12) - as.numeric(data.join$measure.t0)
  
  #positive.control = 'bacteriaonly'
  data_pos <- data.join[data.join$sample == positive.control, ]
  #data_sample <- data.join[data.join$sample != positive.control, ]
  
  data_pos <- data_pos[,-which(names(data_pos) %in% c('measure.t0', 'measure.t1_12'))]
  data_pos <- data_pos %>%
    rename(control = sample)
  #data_sample <- data_sample[,-which(names(data_sample) %in% c('measure.y', 'measure.x'))]
  data.join <- data.join[,-which(names(data.join) %in% c('measure.t0', 'measure.t1_12'))]
  
  data_pos <- data_pos %>%
    group_by(control, bacteria, experiment_id, time.t1_12) %>%
    summarise(mean = mean(deltameasure), sd = sd(deltameasure))
  
  #dont summarize the final value 
  #data.join <- data.join %>%
    #group_by(sample, bacteria, experiment_id, time.t1_12) %>%
    #summarise(mean = mean(deltameasure), sd = sd(deltameasure))
  
  data.join <- full_join(data.join,data_pos,by = c( "bacteria", "experiment_id", "time.t1_12"), suffix = c('.sample', '.control'))
  
  #data.join$deltaratio <- 1- (data.join$mean.sample  / data.join$mean.control)
  
  data.join$deltaratio <- 1- (data.join$deltameasure  / data.join$mean)
  
  #hist(data.join$deltaratio)
  
  return(data.join)
  
}
