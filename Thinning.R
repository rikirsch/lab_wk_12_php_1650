#'thinning function 
#' @description determines if simulated data based of lambda_max will be kept 
#' or not, based on observed hourly arrival rates
#' 
#' @param arrival_times vector storing all times in which a person has arrived
#'  (and not been eliminated by thinning) in this simulation so far
#' @param elapsed_time total time between start of day and this arrival
#' @param arrival_rates_df data frame of estimated average arrival rates (x_hat)
#' for every combination of hour (h), start_station (s), and end_station (t).
#' @param starting_station 
#' @param ending_station
#' 
#' @return vector arrival_times, either with the new elapsed_time added to it, 
#' or not depending on what function determines


thinning <- function(arrival_times, elapsed_time, arrival_rates_df, 
                     starting_station, ending_station) {
          
  #find which set of hours the time belongs to, store the lower_bound
  lower_bound<-floor(elapsed_time)
  
  #create temporary arrival_rates_df with only what we need
  temp_arrival_rates_df <- arrival_rates_df %>%
      filter(hour=lower_bound, start_station=starting_station, end_station=ending_station)
  
  #prob_keep is consistent across all rows left,
  #so we can index the first input and it will be equal
  prob<- temp_arrival_rates_df$prob_keep
  
  
  #run flip a coin w rbinom— next STEP
  
}

TEST_elapsed_time<- 2.5
TESTinterval<-floor(TEST_elapsed_time)


TESTlambda <- OBSERVED_arrival_rates[hour == 1 && start_station == 4 == end_station = 6]$mu_hat