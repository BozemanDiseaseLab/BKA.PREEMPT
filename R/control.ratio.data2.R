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
  
  #okay, obtain all data recorded at time points 0. this is how we establish the baseline readings for our 'delta'
  data_0 <- na.omit(data[data$time == 0, ])
  
  #join the raw data (data) with data_0, all we are  doing is just pasting on a column of the OD readings for each well at time 0. 
  data.join <- full_join(data_0, data, by =c('which_row', 'column', 'sample', 'bacteria', 'experiment_id'), suffix = c('.t0', '.t1_12'))
  #now obtain the delta measure. for each subsequence time point (t1 through t12) and the reading  at time point 0
  data.join$deltameasure = as.numeric(data.join$measure.t1_12) - as.numeric(data.join$measure.t0)
  
  #okay,  now obtain the readings for the control. this how we take our ratio
  data_pos <- data.join[data.join$sample == positive.control, ]
  
  #get rid of all those dumb extra columns. we  hate them
  data_pos <- data_pos[,-which(names(data_pos) %in% c('measure.t0', 'measure.t1_12', 'which_row', 'column', 'time.t0'))]
  #rename  the  sample column as the  'control_sample', this will  just  make it  easier to understand
  #what the f  is going on when we merge them...mkay? 
  colnames(data_pos)[1] <- "control_sample"
  
  #join da  thangs, but first get  rid   of some silly columns we dont care about anymore
  data.join <- data.join[,-which(names(data.join) %in% c('measure.t0', 'measure.t1_12','which_row', 'column', 'time.t0'))]
  
  #okay now lets do a left join. bring in the OD  readings  from the control and match them with the other columns. RATIOS
  data.join1 <- left_join(data.join,data_pos,by = c(#"time.t0",
                                                    #"which_row",
                                                    #"column", 
                                                    "bacteria", 
                                                    "experiment_id", 
                                                    "time.t1_12"
                                                    ), suffix = c('.sample', '.control'))
  #okay,  lets take some  summary stats (variance, mean, covariance, for these measures.) 
  #we  will need these to get  the variance of the ratio. mmmkay?
  data.join1 <- data.join1 %>%
    group_by(sample, bacteria, experiment_id, time.t1_12) %>%
    summarise(deltameasure.control.mean = mean(deltameasure.control), 
              deltameasure.control.var = var(deltameasure.control),
              deltameasure.sample.mean = mean(deltameasure.sample), 
              deltameasure.sample.var = var(deltameasure.sample),
              delta.measure.cov = cov(deltameasure.control, deltameasure.sample))
              
#take 1- ratio to get the %killing compared to the control 
data.join1$deltaratio <-  1 - (data.join1$deltameasure.sample.mean  / data.join1$deltameasure.control.mean)

#okay now we need to get the variance around each ratio.
#i am using the following formula (a source explaining my thoughts below also included)
#https://statmd.wordpress.com/2013/08/04/the-expectation-of-the-ratio-of-two-random-variables/
# E(X)^2  / E(Y)^2  * (Var(X)/E(X)^2) - 2 * Cov(X/Y)/ (E(X)E(Y) + Var(Y)/E(Y)^2)

data.join1$deltaratio.var <- ((data.join1$deltameasure.sample.mean)^2/(data.join1$deltameasure.control.mean)^2) *
                              ((data.join1$deltameasure.sample.var / (data.join1$deltameasure.sample.mean)^2) - 
                              2*data.join1$delta.measure.cov/((data.join1$deltameasure.sample.mean)*(data.join1$deltameasure.control.mean))
                              +  (data.join1$deltameasure.control.var / (data.join1$deltameasure.control.mean)^2))
  
#return da data yo
  return(data.join1)
}
