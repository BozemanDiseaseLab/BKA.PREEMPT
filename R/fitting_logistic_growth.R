rm(list=(ls()[ls()!="bka.merge"]))

unique(bka.merge$experiment_id)

data <- bka.merge %>%
  filter(experiment_id == "1.29.19_Evelyn Benson") %>%
  filter(sample == 'edta') %>%
  unite(well, c(which_row, column)) 
 

plot( 1 / (1 + exp(-seq(-12,12,.1))), type = 'l')

K = 1.4
t0 = 0
t1 = .5 
time = seq(0, .5, .01)
b = 15
a = 5
gc <- as.data.frame(unlist(K / (1 + exp(a + -b*time))))
gc$x <- time
plot(y = K / (1 + exp(a + -b*time)) , type = 'l', x=time)
gc <- as.data.frame(gc)

  ggplot() +
  geom_line(aes(x=data$time, y= as.numeric(data$measure), group = data$well, col = data$experiment_id)) +
  ylim(0,2.5) + 
  xlim(0,2.5) + 
  geom_point(aes(y=gc[,1], x= gc$x), line = 'black')
  

