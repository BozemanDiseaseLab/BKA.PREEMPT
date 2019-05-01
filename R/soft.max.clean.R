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
  
  #first, load in data using read_excel function, dont obtain col_names. this is reading in the raw softmax data
  #it is a bizarre  format so we need to clean it up
  data <- readxl::read_excel(file_path, col_names = FALSE)
  #the  softmax data often includes many rows of metadata about the run, however, the number of rows is for some
  #reason really inconsisten, so we can't just use 'skip rows' argument in the  read_excel function
  #instead, look for the word 'time', that is where the data starts. so let's find that row, and call it 'index' 
  index <- which(grepl(pattern = "Time", unlist(data[,1]))== TRUE)
  #every row before  we first see the word "time', delete 
  data <- data[-c(0:index), ]
  #now we need to grab the end
  index2 <- which(grepl(pattern = "~End", unlist(data[,1]))== TRUE)[[1]]
  data <- data[-c(index2:nrow(data)), ]
  
  #only grabb the first 14 columns, everything else is GAAHHBAGE, to quote marky mark 
  data <- data[,1:14]
  
  #okay now we need to read in the 'well's sheet. This how we know which sample  is in each well.
  #without this data we are no better than the animals 
  wells <- readxl::read_excel(file_path, col_names = FALSE, sheet = 2)
  #load in experimental 'metadata'
  metadata <- readxl::read_excel(file_path, col_names = FALSE, sheet = 3)
  
  #okay, labels all  the columns 
  colnames(data) <- c("time", "temp_c", "col_1", "col_2", "col_3","col_4","col_5","col_6","col_7","col_8","col_9","col_10","col_11","col_12")

  #4/26/19:  we can get rid of these functions we think

  #okay betches, lifes about to get h@rD 
  #find the indexes where the values are  NA. this includes those dumb gaps AND the end of the data
  #the end of the data, those indexes should be 1 apart (thus the 'diff'  function)
  #the gaps should be 9 apart. 
  #okay wait this was so  stupid. we could have just got rid of the fucking blank  rows. fuck me
  rm(index)
  
  is.na.data <-which(is.na(data$col_1))
  if(TRUE %in% diff(is.na.data) == 1)
  {
  index <- which(diff(is.na.data) == 1)[[1]]
  end_of_data <- is.na.data[[index]]
  data <- data[c(0:(end_of_data-1)),]
  }
  
  #remove blanks, which exist between every time point in the softmax data. stupid goddamn softmax
  blanks <- seq(9,nrow(data),by=9)
  data <- data[-c(blanks),]

  #figure  out how  many time points are  in the data
  if(is.na(num_of_time_points))
  {
    num_of_time_points <- nrow(data[!is.na(data$time), ])
  }
  
  #fix time variables

  times <-  seq(1,nrow(data),by=8)
  index <- 0
  #i  honestly do not remember what im doing here
  data$time_2 <- NA
  for(i in (times))
  {
    index <- index+1
    data[c(i:(i+7)),'time_2'] <- as.numeric(data[times[index],'time'])
  }

  data$time <- data$time_2
  data$time_2 <- NULL
  data$which_row <- LETTERS[1:8]

  #tidy up the data, putting it into a 'tidy' format. this is the format  we  should use for  the analyses 
  data.tidy <- data %>%
    select(-c(temp_c)) %>%
    group_by(time, which_row) %>%
    gather(key=column, value = measure, 2:13)

  #label the wells in the 'well' datasheet, so we can join it with the actual raw softmax data
  colnames(wells) <- c("col_1", "col_2", "col_3","col_4","col_5","col_6","col_7","col_8","col_9","col_10","col_11","col_12")
  wells$which_row <- LETTERS[1:8]
  
  wells.tidy <- wells %>%
    group_by(which_row) %>%
    gather(key=column, value = sample, 1:12)

  #we have  now joined the raw data with the wells datasheet, which maps up  96 wells plate 
  data.tidy.join <- full_join(data.tidy, wells.tidy, by = c("which_row", "column"))
  #add in experimental metadata, like who ran the assay, the bacteria,  etc. 
  data.tidy.join$bacteria = rep(as.character(metadata[3,3]),nrow(data.tidy.join)) 
  data.tidy.join$experiment_id <- rep(as.character(paste(metadata[1,3], metadata[2,3], sep = "_")), nrow(data.tidy.join))
  #"okay Caylee I hope you are happy with all this annotation" - Dan
  #"oh wow this  sure  is great, Dan you are  da real  MPV" - Caylee
  #" oh wow love this  gr8 communication, this project sure  is swell' -Dan
  #"I am scretly Jared Kushner, please dont tell Raina' -Wyatt
  return(data.tidy.join)
}
