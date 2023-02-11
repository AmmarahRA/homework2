pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

final.hcris.data<- readRDS("data/output/HCRIS_Data.rds")

#1 
duplicate.hcris<- readRDS("data/output/duplicate_hcris.rds")

fig.more.rep<- duplicate.hcris %>% filter(time_diff<366) %>% 
  group_by(street, fyear) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = fyear, y = count)) +
  geom_line() +
  labs(title = 'Number of Hospitals Filed More Than One Report', x = 'Year', y = 'Number of hospitals') +
  theme_minimal()

#2

medicare_id_table<- final.hcris.data %>% group_by(provider_number) %>% count() 
medicare_id<-nrow(medicare_id_table)
