#' A Function for formatting BKA excel data
#'
#' @param row_start_of_data Row number of start of data in Excel.
#' @param file_path Where does the data live on your computer?
#' @param num_of_time_points Number of time points.
#' @export
#' @examples
#' soft.max.clean

soft.max.clean <- function(file_path, num_of_time_points)
{
  library(tidyverse)
  library(readxl)
  data <- readxl::read_excel(file_path, col_names = FALSE)
  index <- which(grepl(pattern = "Time", unlist(data[,1]))== TRUE)
  data <- data[-c(0:index), ]
  data <- data[,1:14]
  
  wells <- readxl::read_excel(file_path, col_names = FALSE, sheet = 2)
  #load in experimental 'metadata'
  metadata <- readxl::read_excel(file_path, col_names = FALSE, sheet = 3)
  
  colnames(data) <- c("time", "temp_c", "col_1", "col_2", "col_3","col_4","col_5","col_6","col_7","col_8","col_9","col_10","col_11","col_12")

  if(is.na(num_of_time_points))
  {
    num_of_time_points <- nrow(data[!is.na(data$time), ])

  }

  end_of_data <- 9*num_of_time_points
  data <- data[c(0:end_of_data),]

  #remove blanks
  blanks <- seq(9,end_of_data,by=9)
  data <- data[-c(blanks),]

  #fix time variables
  times <-  seq(1,end_of_data-9,by=8)
  index <- 0

  data$time_2 <- NA
  for(i in (times))
  {
    index <- index+1
    data[c(i:(i+7)),'time_2'] <- as.numeric(data[times[index],'time'])
  }

  data$time <- data$time_2
  data$time_2 <- NULL
  data$which_row <- LETTERS[1:8]

  data.tidy <- data %>%
    select(-c(temp_c)) %>%
    group_by(time, which_row) %>%
    gather(key=column, value = measure, 2:13)

  colnames(wells) <- c("col_1", "col_2", "col_3","col_4","col_5","col_6","col_7","col_8","col_9","col_10","col_11","col_12")
  wells$which_row <- LETTERS[1:8]

  wells.tidy <- wells %>%
    group_by(which_row) %>%
    gather(key=column, value = sample, 1:12)

  data.tidy.join <- full_join(data.tidy, wells.tidy)
  #add in experimental metadata
  data.tidy.join$bacteria = rep(as.character(metadata[3,3]),nrow(data.tidy.join)) 
  data.tidy.join$experiment_id <- rep(as.character(paste(metadata[1,3], metadata[2,3], sep = "_")), nrow(data.tidy.join))
  
  return(data.tidy.join)
}
