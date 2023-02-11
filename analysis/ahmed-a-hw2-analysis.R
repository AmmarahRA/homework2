pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

final.hcris.data<- readRDS("data/output/HCRIS_Data.rds")

#1 

multiple_reports_table<- final.hcris.data %>% group_by(street, year) %>% 
  summarise(count = n()) %>%
  filter(count > 1) %>% 
  group_by(year) %>% 
  summarise(count = n())

multiple_reports<- sum(multiple_reports_table$count)

fig.mult.rep<- multiple_reports_table %>% ggplot(aes(x = year, y = count)) +
  geom_line() + 
  labs(title = 'Number of Hospitals with Multiple Reports', x = 'Year', y = 'Number of hospitals') +
  theme_minimal()
  
#2

medicare_id_table<- final.hcris.data %>% group_by(provider_number) %>% count() 
medicare_id<-nrow(medicare_id_table)

#3

tot_charges <- final.hcris.data %>% ggplot(aes(x = as.factor(year), y = tot_charges)) +
  geom_violin(trim = FALSE) +
  labs(title = 'Distribution of Total Charges', x = 'Year', y = 'Total Charges') +
  theme_minimal()









