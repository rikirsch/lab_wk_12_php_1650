#'run_a_day function
#'
#' @description simulates a singular day of bike usage based off the observed
#' data, the calculated mean arrival rates, and the proposed initial bike placement 
#' 
#' @param arrival_rates_df data frame of estimated average arrival rates (x_hat)
#'  for every combination of hour (h), start_station (s), and end_station (t).
#' @param bike_placement data frame showing how many bikes (bikes_initial) are 
#' initially placed are each starting_station
#' 
#' @return data frame showing number of unhappy people at each stop at 
#' each hour of the day

run_a_day <- function(arrival_rates_df, bike_placement){
  
  #FIND LAMBDA MAX FOR EACH PAIR OF STATIONS.
  
  simulated_arrivals <- arrival_rates_df %>%
    #sort by start and end stations so to create samples based on each pair
    group_by(start_station, end_station) %>%
    #find lambda_max, the maximum arrival rate observed, for each pair of stations
    summarize(lambda_max=max(mu_hat)) 
  
  #join lambda_maxes data_frame to arrival_rates_df using a full join to not
  #drop any observations
  
  arrival_rates_df <- full_join(arrival_rates_df, lambda_maxes)
  
  #calculate, for each hour and set of stations, the probability of keeping 
  #an observation. this will be used when it comes to thinning.
  arrival_rates_df <- arrival_rates_df %>%
    mutate(prob_keep = mu_hat/ lambda_max) %>%
    #now, get rid of NAs for prob_keep and replace them with 0
    mutate(prob_keep = case_when( is.na(prob_keep) ~ 0, TRUE ~ prob_keep))
  
  
  #SAMPLE NEW ARRIVAL TIMES
  #for each pair of stations, draw exponential random variables until the end 
  #of the day, testing each to see if it is eliminated by thinning
  
  #set up for loop
  for(i in 1:nrow(simulated_arrivals)) {
    
    #initialize time variable, elapsed_time  = hours since day started
    #once 24 hours have passed, the day is complete. 
    elapsed_time <- 0
    
    #initialize vector storing times at which arrivals come to the stations
    arrival_times <- c()
    
    #establish while loop until end of day
    while (elapsed_time < 24) {
      
      #store random variable of the interval in which next arrival comes
      next_arrival<-rexp(1, rate =  simulated_arrivals$lambda_max[i])
      #update elapsed time to designate time passed
      elapsed_time <- elapsed_time + next_arrival
      
      
      #RUN THINNING FUNCTION to see if observation will be kept, 
      #should return a vector
      arrival_times<-thinning(arrival_times, elapsed_time, arrival_rates_df, 
                              start, end)
      
      
      
    }
    
    
  
  }
  
  
  
}
  


CURRENT_TEST <- OBSERVED_arrival_rates %>%
  #sort by start and end stations so to create samples based on each pair
  group_by(start_station, end_station) %>%
  #find lambda_max, the maximum arrival rate observed, for each pair of stations
  summarize(lambda_max=max(mu_hat)) 


