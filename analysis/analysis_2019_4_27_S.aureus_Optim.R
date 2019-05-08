# source('/Users/dancrowley/BKA.PREEMPT/R/soft.max.clean.R')

wd <- getwd()

source(paste(wd, '/R/soft.max.clean.R', sep = ''))

file_path = '~/Box Sync/Research NSF Bat1Health Collaboration/bat_immunoassays/Evelyn_Data/'
file.list <- list.files(file_path, pattern = '.xlsx')
file.list

# Evelyns BKA data for optimizing S aureus concentration and looking at edge effects
#(SECOND plate layout: blocks to look at edge effects/dilution)
file <- paste(file_path, 'EB024_BKA_S.aureus_ATCC6538.xlsx', sep = "")
data <- soft.max.clean(file_path = file, num_of_time_points = NA)

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
  mutate(newcol_X = str_replace(column, "col_", "")) %>%
  mutate(newcol = str_replace_all(which_row, c("A" = "1", "B" = "2", "C" = "3", "D" = "4", 
                                               "H" = "1", "G" = "2", "F" = "3", "E" = "4")) )

data2$newcol2 <- data2$newcol_X
data2$newcol2[data2$newcol_X == 7] =  6
data2$newcol2[data2$newcol_X == 8] =  5
data2$newcol2[data2$newcol_X == 9] =  4
data2$newcol2[data2$newcol_X == 10] = 3
data2$newcol2[data2$newcol_X == 11] = 2
data2$newcol2[data2$newcol_X == 12] = 1

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

# also want to look at blocks directly?
# 10^5 compare A1 - A6 and B1,C1,D1 to G2 - G6 and E2 F2 
head(data2)

data2$well <- paste(data2$which_row, data2$newcol_X, sep = "") # use well to specify
data2$block = "C"

data2$block[data2$sample == "10^5/mL"] = "I_10.5"
str(data2$well)
data2$block[data2$well == "A1"] = "EE_10.5" # edge exterior
data2$block[data2$well == "A2"] = "EE_10.5"
data2$block[data2$well == "A3"] = "EE_10.5"
data2$block[data2$well == "A4"] = "EE_10.5"
data2$block[data2$well == "A5"] = "EE_10.5"
data2$block[data2$well == "A6"] = "EE_10.5"
data2$block[data2$well == "B1"] = "EE_10.5"
data2$block[data2$well == "C1"] = "EE_10.5"
data2$block[data2$well == "D1"] = "EE_10.5"

data2$block[data2$well == "G2"] = "EB_10.5" # edge buffer
data2$block[data2$well == "G3"] = "EB_10.5"
data2$block[data2$well == "G4"] = "EB_10.5"
data2$block[data2$well == "G5"] = "EB_10.5"
data2$block[data2$well == "G6"] = "EB_10.5"
data2$block[data2$well == "E2"] = "EB_10.5"
data2$block[data2$well == "F2"] = "EB_10.5"


data2 %>%
  filter(sample ==  '10^5/mL') %>%
  # unite(well, c(which_row, column)) %>%
  ggplot(aes(x=time, y= (as.numeric(measure)) )) +
  geom_point(aes(group = well, col = block)) +
  geom_line(aes(group = well, col = block)) +
  facet_grid(~block)

