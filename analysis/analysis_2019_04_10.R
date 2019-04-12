source('R/soft.max.clean.R')
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

bka.merge$sample <- tolower(bka.merge$sample)
bka.merge$sample <- gsub(" ", "", bka.merge$sample)

bka.merge %>%
  dplyr::filter(grepl(pattern='edta', x=sample)) %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot() +
  geom_line(aes(x=time, y= as.numeric(measure), group = well, col = experiment_id)) +
  ylim(.5,2.5)+
  facet_grid(experiment_id~sample) 

bka.merge %>%
  filter(sample != 'blank') %>%
  mutate(sample = ifelse(grepl('bat',sample),'bat serum', sample)) %>%
  #dplyr::filter(grepl(pattern='edta|bacteriaonly', x=sample)) %>%
  #dplyr::filter(experiment_id == '1.30.19_Evelyn Benson') %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot(aes(x=time, y= as.numeric(measure))) +
  geom_point(aes(group = well, col = experiment_id)) +
  geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
  ylim(.9,2)+
  xlim(0,.6)+
  facet_grid(bacteria~sample)

# bka.merge %>%
#   filter(experiment_id == '1.29.19_Evelyn Benson') %>%
#   mutate(sample = ifelse(grepl('bat',sample),'bat serum', sample)) %>%
#   #dplyr::filter(grepl(pattern='edta|bacteriaonly', x=sample)) %>%
#   #dplyr::filter(experiment_id == '1.30.19_Evelyn Benson') %>%
#   unite(well, c(which_row, column)) %>%
#   filter(sample != 'Blank') %>%
#   ggplot(aes(x=time, y= as.numeric(measure))) +
#   geom_point(aes(group = well, col = experiment_id)) +
#   geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
#   ylim(.5,2.5)+
#   xlim(0,.8)+
#   facet_wrap(bacteria~sample)

#first lets look only at inter and intra experiment variability 

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
  geom_hline(yintercept = 0 , color = 'red', size = 1, alpha = .5) +
  geom_boxplot() +
  geom_jitter(alpha =.5) +
  facet_wrap(~sample)

#..................................
rm(list=(ls()[ls()!="bka.merge"]))
source('R/control.ratio.data.R')
source('R/control.ratio.data2.R')

ratio <- control.ratio.data(bka.merge, positive.control = 'bacteriaonly')
ratio2 <- control.ratio.data2(bka.merge, positive.control = 'bacteriaonly')
  
ratio <- left_join(ratio, ratio2[,c('sample', "experiment_id", "time.t1_12", "bacteria", "deltaratio","deltaratio.var")], by = c('sample', "experiment_id", "time.t1_12", "bacteria"))

#https://statmd.wordpress.com/2013/08/04/the-expectation-of-the-ratio-of-two-random-variables/

# E(X)^2  / E(Y)^2  * (Var(X)/E(X)^2) - 2 * Cov(X/Y)/ (E(X)E(Y) + Var(Y)/E(Y)^2)

tp12 <- ratio[ratio$time.t1_12 == 0.5, ]
#tp12 <- tp12[tp12$bacteria == 'S.aureus', ]

tp12 %>% 
  filter(grepl('bat', sample)) %>%
  ggplot(aes(bacteria, as.numeric(deltaratio.x, col = bacteria))) +
  geom_hline(yintercept = 0 , color = 'purple', size = 1, alpha =.5) +
  geom_boxplot(aes(col = bacteria), coef = 0) + 
  geom_jitter(aes(col = bacteria)) +
  facet_wrap(~sample) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_errorbar(aes(col = bacteria, ymin=deltaratio.y-sqrt(deltaratio.var), ymax=deltaratio.y+sqrt(deltaratio.var)), width=.2,
                position=position_dodge(.9))  +
  facet_grid(~sample) +
  ylab("Percent Killing Compared to Positive Control") 

tp12 %>% 
  filter(grepl('mouse', sample)) %>%
  mutate(experiment = paste(experiment_id, bacteria)) %>%
  ggplot(aes(experiment, as.numeric(deltaratio.x, col = bacteria))) +
  geom_hline(yintercept = 0 , color = 'purple', size = 1, alpha =.5) +
  geom_boxplot(aes(col = bacteria), coef = 0) + 
  geom_jitter(aes(col = bacteria)) +
  facet_wrap(~sample) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_errorbar(aes(col = bacteria, ymin=deltaratio.y-sqrt(deltaratio.var), ymax=deltaratio.y+sqrt(deltaratio.var)), width=.2,
                position=position_dodge(.9))  +
  #facet_grid(sample~bacteria) +
  ylab("Percent Killing Compared to Positive Control") 

tp12 %>% 
  filter( sample == 'edta' | sample == 'bacteriaonly') %>%
  ggplot(aes(experiment_id, as.numeric(deltaratio.x, col = bacteria))) +
  geom_hline(yintercept = 0 , color = 'purple', size = 1, alpha =.5) +
  geom_boxplot(aes(col = bacteria), coef = 0) + 
  geom_jitter(aes(col = bacteria)) +
  facet_wrap(~sample) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_errorbar(aes(col = bacteria, ymin=deltaratio.y-sqrt(deltaratio.var), ymax=deltaratio.y+sqrt(deltaratio.var)), width=.2,
                position=position_dodge(.9))  +
  facet_grid(~sample) +
  ylab("Percent Killing Compared to Positive Control") 






x <- tp12 %>%
  filter(experiment_id == '4.3.19_Evelyn Benson') %>%
  filter(sample.sample == 'bacteriaonly')
  
control.ratio.data2(bka.merge, positive.control = 'pbsonly') %>%
  filter(bacteria == 'E.coli') %>%
  mutate(sample = ifelse(grepl('bat',sample.sample),'bat serum', sample.sample)) %>%
  mutate(group = paste(sample.sample,experiment_id)) %>%
  ggplot(aes(x=time.t1_12, y = mean.sample, group  = group, col = experiment_id)) +
  geom_ribbon(aes(x=time.t1_12, ymax = mean.sample + sd.sample, ymin = mean.sample - sd.sample), alpha =.4) +
  geom_line() +
  facet_wrap(~sample)

control.ratio.data2(bka.merge, positive.control = 'bacteriaonly')%>%
  filter(bacteria == 'E.coli') %>%
  mutate(sample = ifelse(grepl('bat',sample.sample),'bat serum', sample.sample)) %>%
  mutate(group = paste(sample.sample,experiment_id)) %>%
  #filter(time.t1_12 == 0 |time.t1_12 ==0.50000000) %>%
  ggplot(aes(x=time.t1_12, y = deltaratio, group  = group, col = experiment_id)) +
  geom_line() +
  facet_wrap(~sample)

control.ratio.data2(bka.merge, positive.control = 'pbsonly')%>%
filter(bacteria == 'E.coli') %>%
  mutate(sample = ifelse(grepl('bat',sample.sample),'bat serum', sample.sample)) %>%
  mutate(group = paste(sample.sample,experiment_id)) %>%
  ggplot(aes(x=time.t1_12, y = (deltaratio), group  = group, col = experiment_id)) +
  geom_line() +
  facet_wrap(~sample)


