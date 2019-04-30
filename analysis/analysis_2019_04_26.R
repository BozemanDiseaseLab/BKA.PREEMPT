# source('/Users/dancrowley/BKA.PREEMPT/R/soft.max.clean.R')
source('/Users/aubrygrad/Desktop/git/BKA.PREEMPT/R/soft.max.clean.R')
file_path = '~/Box Sync/Research NSF Bat1Health Collaboration/bat_immunoassays/Evelyn_Data/'
file.list <- list.files(file_path, pattern = '.xlsx')
file.list

file <- paste(file_path, 'EB023_BKA_S.aureus_ATCC6538.xlsx', sep = "")
data <- soft.max.clean(file_path = file, num_of_time_points = 7)

head(data)
summary(data)
names(data)

data %>%
   unite(well, c(which_row, column)) %>%
  ggplot(aes(x=time, y= (as.numeric(measure)))) +
   geom_point(aes(group = well, col = sample)) +
  #geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
  geom_line(aes(group = well, col = sample)) +
  facet_grid(~sample)

# calculate dist from edges
data2 <- data %>%
  mutate(newcol2 = str_replace(column, "col_", "")) %>%
   mutate(newcol = str_replace_all(which_row, 
                 c("A" = "1", "B" = "2", "C" = "3", "D" = "4", "H" = "1", "G" = "2", "F" = "3", "E" = "4")) )

data2$newcol2[data2$newcol2 == 7] =  6
data2$newcol2[data2$newcol2 == 8] =  5
data2$newcol2[data2$newcol2 == 9] =  4
data2$newcol2[data2$newcol2 == 10] = 3
data2$newcol2[data2$newcol2 == 11] = 2
data2$newcol2[data2$newcol2 == 12] = 1

data2$edge = ( as.numeric (data2$newcol) )* (as.numeric (data2$newcol2))

# plot bacteria conc and PBS
data2 %>%
  unite(well, c(which_row, column)) %>%
  ggplot(aes(x=time, y= (as.numeric(measure)))) +
  geom_point(aes(group = well, col = edge)) +
  geom_line(aes(group = well, col = edge)) +
  facet_grid(~sample)

# PBS looked like there was separation based on edge?
data2 %>%
  filter(sample == 'PBS') %>%
  unite(well, c(which_row, column)) %>%
  ggplot(aes(x=time, y= (as.numeric(measure)) )) +
  geom_point(aes(group = well, col =edge)) +
  geom_line(aes(group = well, col = edge)) +
  ggtitle("PBS only controls")


# just bacteria concentrations to compare 
data2 %>%
  filter(sample == c( '10^5/mL', "10^4/mL")) %>%
  unite(well, c(which_row, column)) %>%
  ggplot(aes(x=time, y= (as.numeric(measure)) )) +
  geom_point(aes(group = well, col = edge)) +
  geom_line(aes(group = well, col = edge)) +
  facet_grid(~sample)



