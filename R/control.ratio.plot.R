#' A Function for auto-generating control ratio plots.
#' Designed for data outputed from soft.max.clean function
#'
#' @param data Data formatted per soft.max.clean output.
#' @param positive.control Name of positive control, as string.
#' @export
#' @examples
#' control.ratio.plot()

control.ratio.plot <- function(data, positive.control)
{
  library(tidyverse)
  data_0 <- data[data$time == 0, ]
  data_n0 <- data[data$time != 0, ]

  data.join <- full_join(data_0, data_n0, by =c('which_row', 'column', 'sample'))

  data_pos <- data.join[data.join$sample == positive.control, ]
  data_sample <- data.join[data.join$sample != positive.control, ]

  data.join$neg_control <- data_pos$measure.y

  plot<- data.join %>%
    unite(well, c(which_row, column)) %>%
    mutate(rate_ratio = as.numeric(measure.y)/as.numeric(neg_control)) %>%
    filter(sample != 'Blank') %>%
    ggplot() +
    geom_line(aes(x=time.y, y= as.numeric(rate_ratio), group = well, col = sample)) +
    #scale_y_continuous(0,3)+
    facet_wrap(~sample)

  return(plot)

}
