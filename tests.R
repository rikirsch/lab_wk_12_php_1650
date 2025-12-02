#emmett tests

#create dummy arrival rates data set 
OBSERVED_bike_data<-read.csv('/Users/juliadubnoff/Downloads/sample_bike.csv')
OBSERVED_bike_data<-bike_data_clean(OBSERVED_bike_data)
OBSERVED_arrival_rates<-arrival_rates(OBSERVED_bike_data)




#practice the join
OBSV_LAMBDA_MAX <- OBSERVED_arrival_rates %>%
  #sort by start and end stations so to create samples based on each pair
  group_by(start_station, end_station) %>%
  #find lambda_max, the maximum arrival rate observed, for each pair of stations
  summarize(lambda_max=max(mu_hat)) 

OBSERVED_arrival_rates <- full_join(OBSERVED_arrival_rates, OBSV_LAMBDA_MAX)
OBSERVED_arrival_rates <- OBSERVED_arrival_rates %>%
  mutate(prob_keep = mu_hat/ lambda_max) %>%
  
OBSERVED_arrival_rates <- OBSERVED_arrival_rates %>%
  mutate(prob_keep = case_when( is.na(prob_keep) ~ 0, TRUE ~ prob_keep))

#seems to work until this point! yay

simulated_arrivals <- data.frame("start"=c(2), "end"=c(3), "arrival_time"=c())




#test through simulating arrivals
library(tidyverse)
OBSERVED_bike_data<-read.csv('/Users/juliadubnoff/Downloads/sample_bike.csv')
OBSERVED_bike_data<-bike_data_clean(OBSERVED_bike_data)
OBSERVED_arrival_rates<-arrival_rates(OBSERVED_bike_data)

#create dummy bike placement data set
TEST_bike_placement<-OBSERVED_bike_data%>%
  group_by(start_station)%>%
  summarize(num_initial_bikes=5)%>%
  rename(station=start_station)


TEST_arrivals<-run_a_day(OBSERVED_arrival_rates, TEST_bike_placement)

#something is wrong with thinning
# 
# TEST_lambda_maxes <- OBSERVED_arrival_rates %>%
#   #sort by start and end stations so to create samples based on each pair
#   group_by(start_station, end_station) %>%
#   #find lambda_max, the maximum arrival rate observed, for each pair of stations
#   summarize(lambda_max=max(mu_hat)) 
# 
# #join lambda_maxes data_frame to arrival_rates_df using a full join to not
# #drop any observations
# 
# OBSERVED_arrival_rates <- full_join(OBSERVED_arrival_rates, TEST_lambda_maxes)
# 
# #calculate, for each hour and set of stations, the probability of keeping 
# #an observation. this will be used when it comes to thinning.
# OBSERVED_arrival_rates <- OBSERVED_arrival_rates %>%
#   mutate(prob_keep = mu_hat/ lambda_max) %>%
#   #now, get rid of NAs for prob_keep and replace them with 0
#   mutate(prob_keep = case_when( is.na(prob_keep) ~ 0, TRUE ~ prob_keep))
# 

rbinom(1, 1, 0.5)
rbinom()

