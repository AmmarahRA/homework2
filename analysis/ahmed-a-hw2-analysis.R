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

#4

final.hcris.data2<- final.hcris.data %>%
  mutate(discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          est_price = price_num/price_denom)

est_prices<- final.hcris.data2 %>% 
  ggplot(aes(x = as.factor(year), y = est_price)) +
  geom_violin(trim = FALSE) + 
  labs(title = 'Distribution of Estimated Prices', x = 'Year', y = 'Estimated Prices') +
  theme_bw()
  
#5

hcris_2012 <- final.hcris.data2 %>% filter(year == 2012)

hcris_2012$penalty <- ifelse(hcris_2012$hrrp_payment + hcris_2012$hvbp_payment < 0, 1,0)

pen_price<- hcris_2012 %>%
  filter(!is.na(penalty)) %>% 
  filter(penalty == 1) %>%
  group_by(penalty) %>% 
  summarise(price = mean(est_price, na.rm = TRUE))

non_pen_price <- hcris_2012 %>%
  filter(!is.na(penalty)) %>% 
  filter(penalty == 0) %>%
  group_by(penalty) %>% 
  summarise(price = mean(est_price, na.rm = TRUE))

#6 

quantile(hcris_2012$beds, na.rm = TRUE)

hcris_2012$l_quart <- 
  
#hcris_2012$u_quart <- ifelse(hcris_2012$u_quart <= quantile(hcris_2012$beds, probs = 0.75, na.rm = TRUE), 1,0)

#hcris_2012$m_quart <- ifelse(hcris_2012$m_quart <= quantile(hcris_2012$beds, probs = 0.50, na.rm = TRUE), 1,0)


