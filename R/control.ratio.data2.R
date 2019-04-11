#' A Function for auto-generating control ratio plots.
#' Designed for data outputed from soft.max.clean function
#'
#' @param data Data formatted per soft.max.clean output.
#' @param positive.control Name of positive control, as string.
#' @export
#' @examples
#' control.ratio.plot()

control.ratio.data2 <- function(data, positive.control)
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
  
  data_pos <- data_pos[,-which(names(data_pos) %in% c('measure.t0', 'measure.t1_12', 'which_row', 'column', 'time.t0'))]
  colnames(data_pos)[1] <- "control_sample"
  
  #data_sample <- data_sample[,-which(names(data_sample) %in% c('measure.y', 'measure.x'))]
  data.join <- data.join[,-which(names(data.join) %in% c('measure.t0', 'measure.t1_12','which_row', 'column', 'time.t0'))]
  
  data.join1 <- left_join(data.join,data_pos,by = c(#"time.t0",
                                                    #"which_row",
                                                    #"column", 
                                                    "bacteria", 
                                                    "experiment_id", 
                                                    "time.t1_12"
                                                    ), suffix = c('.sample', '.control'))
  
  data.join1 <- data.join1 %>%
    group_by(sample, bacteria, experiment_id, time.t1_12) %>%
    summarise(deltameasure.control.mean = mean(deltameasure.control), 
              deltameasure.control.var = var(deltameasure.control),
              deltameasure.sample.mean = mean(deltameasure.sample), 
              deltameasure.sample.var = var(deltameasure.sample),
              delta.measure.cov = cov(deltameasure.control, deltameasure.sample))
              
# data_pos1 <- data_pos %>%
#     group_by(control_sample, bacteria, experiment_id, time.t1_12) %>%
#     summarise(mean = mean(deltameasure), sd = sd(deltameasure))
#   
#   data.join <- data.join %>%
#     group_by(sample, bacteria, experiment_id, time.t1_12) %>%
#     summarise(mean = mean(deltameasure), sd = sd(deltameasure))

data.join1$deltaratio <-  1 - (data.join1$deltameasure.sample.mean  / data.join1$deltameasure.control.mean)

#https://statmd.wordpress.com/2013/08/04/the-expectation-of-the-ratio-of-two-random-variables/

# E(X)^2  / E(Y)^2  * (Var(X)/E(X)^2) - 2 * Cov(X/Y)/ (E(X)E(Y) + Var(Y)/E(Y)^2)

data.join1$deltaratio.var <- ((data.join1$deltameasure.sample.mean)^2/(data.join1$deltameasure.control.mean)^2) *
                              ((data.join1$deltameasure.sample.var / (data.join1$deltameasure.sample.mean)^2) - 
                              2*data.join1$delta.measure.cov/((data.join1$deltameasure.sample.mean)*(data.join1$deltameasure.control.mean))
                              +  (data.join1$deltameasure.control.var / (data.join1$deltameasure.control.mean)^2))
  
  return(data.join1)
}
