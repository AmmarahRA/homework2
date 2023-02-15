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
tot_charges <- final.hcris.data %>% ggplot(aes(x = as.factor(year), y = tot_charges, fill = "")) +
  geom_violin(trim = FALSE) + scale_y_continuous(trans = "log10") +
  labs(title = 'Distribution of Log of Total Charges', x = 'Year', y = 'Log of Total Charges') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, size = 9, hjust = 1), plot.title = element_text(hjust = 0.5))


#4

final.hcris.data2<- final.hcris.data %>%
  mutate(discount_factor = 1-tot_discounts/tot_charges,
         price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
         price_denom = tot_discharges - mcare_discharges,
         est_price = price_num/price_denom)

final.hcris.data2 <- final.hcris.data2 %>%
  ungroup() %>% 
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         est_price<100000, 
         beds>30) %>%
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
          penalty = (hvbp_payment-hrrp_payment<0))

est_prices<- final.hcris.data2 %>% 
  ggplot(aes(x = as.factor(year), y = est_price)) +
  geom_violin(trim = FALSE) + 
  labs(title = 'Distribution of Estimated Prices', x = 'Year', y = 'Estimated Prices') +
  theme_bw()

#5

hcris_2012 <- final.hcris.data2 %>% filter(year == 2012)

hcris_2012$penalty <- ifelse(hcris_2012$hvbp_payment - hcris_2012$hrrp_payment < 0, 1,0)            

#hrrp absolute value used as it should always be negative but not always recorded as such. any hrrp is payment

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
hcris_2012$quartile <- ntile(hcris_2012$beds, 4)

hcris_2012$quartile_1 <- ifelse(hcris_2012$quartile == 1, 1,0)
hcris_2012$quartile_2 <- ifelse(hcris_2012$quartile == 2, 1,0)
hcris_2012$quartile_3 <- ifelse(hcris_2012$quartile == 3, 1,0)
hcris_2012$quartile_4 <- ifelse(hcris_2012$quartile == 4, 1,0)
  
table_6 <- hcris_2012 %>% filter(!is.na(penalty)) %>%
  group_by(quartile, penalty) %>%
  summarise(avg_price = mean(est_price, na.rm = TRUE))

table_6_2 <- pivot_wider(table_6, names_from = penalty, values_from = avg_price, 
                         names_prefix = "price")
  
#7

#Nearest neighbour matching with inverse variance distance

m.inv.var <- Matching::Match(Y = hcris_2012$est_price,
                             Tr = hcris_2012$penalty,
                             X= hcris_2012 %>% select(quartile_1, quartile_2, quartile_3),
                             M = 1,
                             Weight = 1,
                             estimand = "ATE")
summary(m.inv.var)

#Nearest neighbour matching with Mahalanobis distance 

m.mahala <- Matching::Match(Y = hcris_2012$est_price, 
                            Tr = hcris_2012$penalty,
                            X = hcris_2012 %>% select(quartile_1, quartile_2, quartile_3),
                            M = 1,
                            Weight = 2,
                            estimand = "ATE")
summary(m.mahala)

#Inverse propensity weighting 

logit.model <- glm(penalty ~ quartile_1 + quartile_2 + quartile_3,
                   family = binomial, data = hcris_2012)

ps <- fitted(logit.model)

hcris_2012_2 <- hcris_2012 %>%
  mutate(ipw = case_when(
    penalty==1 ~ 1/ps,
    penalty==0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))

mean.t1 <- hcris_2012_2 %>% filter(penalty==1) %>%
  select(est_price, ipw) %>% summarize(mean_p=weighted.mean(est_price,w=ipw))
mean.t0 <- hcris_2012_2 %>% filter(penalty==0) %>%
  select(est_price, ipw) %>% summarize(mean_p=weighted.mean(est_price,w=ipw))

inv.ps<- mean.t1$mean_p - mean.t0$mean_p

#Simple linear regression 

reg1.dat <- hcris_2012 %>% filter(penalty == 1)
reg1 <- lm(est_price ~ quartile_1 + quartile_2 + quartile_3, data=reg1.dat)

reg0.dat <- hcris_2012 %>% filter(penalty == 0)
reg0 <- lm(est_price ~ quartile_1 + quartile_2 + quartile_3, data=reg0.dat)

pred1 <- predict(reg1,new=hcris_2012)
pred0 <- predict(reg0,new=hcris_2012)

reg_res <- mean(pred1-pred0)

table_ate <- data.frame(Estimators = c("NN Matching, inverse variance", "NN Matching, mahalanobis",
                                       "Inverse pscore weighting", "Regression"),
                        ATE = c(m.inv.var$est, m.mahala$est, inv.ps, reg_res))


save.image("Hw2_workpsace.Rdata")

