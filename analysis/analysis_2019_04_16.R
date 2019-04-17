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

rm(list=(ls()[ls()!="bka.merge"]))
source('R/control.ratio.data.R')
source('R/control.ratio.data2.R')

ratio <- control.ratio.data(bka.merge, positive.control = 'bacteriaonly')
ratio2 <- control.ratio.data2(bka.merge, positive.control = 'bacteriaonly')

#tp12 <- tp12[tp12$bacteria == 'S.aureus', ]

df <- ratio %>% 
  ungroup() %>%
  select(sample, bacteria, time.t1_12, deltaratio, experiment_id) %>%
  filter(!is.na(deltaratio)) %>%
  group_by(sample, bacteria, experiment_id, time.t1_12) %>%
  summarise(deltaratio= mean(deltaratio)) %>%
  tidyr::spread(time.t1_12, deltaratio) %>%
  filter(bacteria == 'S.aureus')

plot(df$`0.0833333333333333`, df$`0.5`)
plot(df$`0.416666666666667`, df$`0.5`)


df1 <- df[,c(4:9)]
colnames(df1) <- seq(1,6,by =1)
cor <- cor(df1, use="complete.obs")

library(lattice) 
levelplot((cor))

df <- ratio %>% 
  ungroup() %>%
  select(sample, bacteria, time.t1_12, deltaratio, experiment_id) %>%
  filter(!is.na(deltaratio)) %>%
  group_by(sample, bacteria, experiment_id, time.t1_12) %>%
  summarise(deltaratio= mean(deltaratio)) %>%
  tidyr::spread(time.t1_12, deltaratio) %>%
  filter(bacteria == 'S.aureus') %>%
  filter(grepl('bat', sample))

df1 <- df[,c(5:9)]
colnames(df1) <- seq(1,5,by =1)
cor <- cor(df1, use="complete.obs")
levelplot((cor))
cov <- cov(df1, use="complete.obs")
levelplot((cov))
