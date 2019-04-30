source('/Users/dancrowley/BKA.PREEMPT/R/soft.max.clean.R')
file_path = '~/Box Sync/Research NSF Bat1Health Collaboration/bat_immunoassays/Evelyn_Data/'
file.list <- list.files(file_path, pattern = '.xlsx')
file.list

file <- paste(file_path, 'EB023_BKA_S.aureus_ATCC6538.xlsx', sep = "")
data <- soft.max.clean(file_path = file, num_of_time_points = 7)
