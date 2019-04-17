bka.merge %>%
  filter(sample != 'blank') %>%
  filter(sample == 'bacteriaonly') %>%
  #filter(time != 0.00000000) %>%
  mutate(sample = ifelse(grepl('bat',sample),'bat serum', sample)) %>%
  #dplyr::filter(grepl(pattern='edta|bacteriaonly', x=sample)) %>%
  #dplyr::filter(experiment_id == '1.30.19_Evelyn Benson') %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot(aes(x=time, y= (as.numeric(measure)))) +
  #ggplot(aes(x=time, y= log(as.numeric(measure)))) +
  #ggplot(aes(x=time, y= boot::inv.logit(as.numeric(measure)))) +
  geom_point(aes(group = well, col = experiment_id)) +
  #geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
  geom_line(aes(group = well, col = experiment_id)) +
  
  #ylim(.9,2)+
  #xlim(0,.6)+
  facet_grid(bacteria~experiment_id)

#compare 1.30 and 2.5, these are so different in the growth kinetics for bacterial control,
#do all samples looked so fucked up?

bka.merge %>%
  filter(sample != 'blank') %>%
  filter(experiment_id == '1.30.19_Evelyn Benson' | experiment_id == '2.4.19_Evelyn Benson') %>%
  #filter(time != 0.00000000) %>%
  mutate(sample = ifelse(grepl('bat',sample),'bat serum', sample)) %>%
  #dplyr::filter(grepl(pattern='edta|bacteriaonly', x=sample)) %>%
  #dplyr::filter(experiment_id == '1.30.19_Evelyn Benson') %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot(aes(x=time, y= (as.numeric(measure)))) +
  #ggplot(aes(x=time, y= log(as.numeric(measure)))) +
  #ggplot(aes(x=time, y= boot::inv.logit(as.numeric(measure)))) +
  geom_point(aes(group = well, col = experiment_id)) +
  #geom_smooth(aes(group = experiment_id, col = experiment_id), method = 'loess') +
  geom_line(aes(group = well, col = experiment_id)) +
  #ylim(.9,2)+
  #xlim(0,.6)+
  facet_grid(experiment_id~sample)
