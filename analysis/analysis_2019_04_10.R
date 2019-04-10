
file_path = '~/Box Sync/Research NSF Bat1Health Collaboration/bat_immunoassays/'
file.list <- list.files(file_path, pattern = '.xlsx')
file.list

for (i in 1:length(file.list))
{
  file <- paste(file_path, file.list[i], sep = "")
  assign(file.list[i], soft.max.clean(file_path = file, num_of_time_points = 7))
}

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
bka.merge <- do.call(rbind, dfs)
rm(list=(ls()[ls()!="bka.merge"]))

x <- bka.merge %>%
 filter(measure == 'Sample#')

bka.merge$sample <- tolower(bka.merge$sample)
bka.merge$sample <- gsub(" ", "", bka.merge$sample)


bka.merge %>%
  dplyr::filter(grepl(pattern='edta', x=sample)) %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot() +
  geom_line(aes(x=time, y= as.numeric(measure), group = well, col = experiment_id)) +
  ylim(.5,2)+
  facet_grid(experiment_id~sample) 

bka.merge %>%
  filter(experiment_id == 'pbsonly') %>%
  mutate(sample = ifelse(grepl('bat',sample),'bat serum', sample)) %>%
  #dplyr::filter(grepl(pattern='edta|bacteriaonly', x=sample)) %>%
  #dplyr::filter(experiment_id == '1.30.19_Evelyn Benson') %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot(aes(x=time, y= as.numeric(measure))) +
  geom_point(aes(group = well, col = experiment_id)) +
  geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
  ylim(.5,2)+
  xlim(0,.8)+
  facet_grid(bacteria~sample)

bka.merge %>%
  filter(experiment_id == '1.29.19_Evelyn Benson') %>%
  mutate(sample = ifelse(grepl('bat',sample),'bat serum', sample)) %>%
  #dplyr::filter(grepl(pattern='edta|bacteriaonly', x=sample)) %>%
  #dplyr::filter(experiment_id == '1.30.19_Evelyn Benson') %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot(aes(x=time, y= as.numeric(measure))) +
  geom_point(aes(group = well, col = experiment_id)) +
  geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
  ylim(.5,2.5)+
  xlim(0,.8)+
  facet_wrap(bacteria~sample)

bka.merge %>%
  #filter(experiment_id == '1.29.19_Evelyn Benson') %>%
  #mutate(sample = ifelse(grepl('bat',sample),'bat serum', sample)) %>%
  #dplyr::filter(grepl(pattern='edta|bacteriaonly', x=sample)) %>%
  #dplyr::filter(experiment_id == '1.30.19_Evelyn Benson') %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot(aes(x=time, y= as.numeric(measure))) +
  geom_point(aes(group = well, col = experiment_id)) +
  geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
  ylim(.5,2.5)+
  xlim(0,.8)+
  facet_grid(bacteria~sample)

#first lets look only at inter and intra experiment variability 

bka.merge %>%
  #filter(experiment_id == '1.29.19_Evelyn Benson') %>%
  dplyr::filter(grepl(pattern='bacteriaonly', x=sample)) %>%
  #dplyr::filter(experiment_id == '1.30.19_Evelyn Benson') %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot(aes(x=time, y= as.numeric(measure))) +
  geom_point(aes(group = well, col = experiment_id)) +
  geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
  ylim(.5,2.5)+
  xlim(0,.8)+
  facet_grid(bacteria~experiment_id)

bka.merge <- bka.merge %>%
  mutate(sample = ifelse(grepl('bat',sample),'bat serum', sample)) %>%
  filter(sample != 'blank')

list <- unique(bka.merge$sample)
for(i in 1:length(unique(bka.merge$sample)))
{
  tp12 <- bka.merge[bka.merge$time == 0.5, ]
  tp12 <- tp12[tp12$bacteria == 'E.coli', ]
  tp12 <- tp12[tp12$sample == list[i], ]

  tp12 <- tp12[!is.na(tp12$sample), ]
  
  print(list[i])
  lm1 <- lm(as.numeric(measure) ~ 1, data =tp12 )
  tryCatch({lm2 <- lm(as.numeric(measure) ~ experiment_id, data =tp12 )}, error = function(e){})
  tryCatch({print(anova(lm2))}, error = function(e){})
}


tp12 <- bka.merge[bka.merge$time == 0.5, ]
tp12 <- tp12[tp12$bacteria == 'E.coli', ]

tp12 %>% 
  ggplot(aes( experiment_id, as.numeric(measure))) +
  geom_boxplot() +
  geom_jitter(alpha =.5) +
  facet_wrap(~sample)

#..................................
rm(list=(ls()[ls()!="bka.merge"]))
source('R/control.ratio.data.R')
ratio <- control.ratio.data(bka.merge, positive.control = 'bacteriaonly')

ratio %>%
  filter(bacteria == 'E.coli') %>%
  ggplot(aes(x=time.y, y = log(deltaratio), group  = experiment_id, col = experiment_id)) +
  geom_line() +
  facet_wrap(~sample.y)

ratio <- control.ratio.data(bka.merge, positive.control = 'pbsonly')

ratio %>%
  ggplot(aes(x=time.y, y = log(deltaratio), group  = experiment_id, col = experiment_id)) +
  geom_line() +
  facet_grid(sample.y ~bacteria)



