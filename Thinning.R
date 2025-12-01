#'thinning function 
#' @description determines if simulated data based of lambda_max will be kept 
#' or not, based on observed hourly arrival rates
#' 
#' @param elapsed_time total time between start of day and this arrival
#' @param arrival_rates_df data frame of estimated average arrival rates (x_hat)
#' for every combination of hour (h), start_station (s), and end_station (t).
#' @param starting_station 
#' @param ending_station
#' 
#' @return boolean keep that = 1 if the arrival time will be added to the data, 
#' if it = 0 it will be discarded 


thinning <- function(elapsed_time, arrival_rates_df, 
                     starting_station, ending_station) {
          
  #find which set of hours the time belongs to, store the lower_bound
  lower_bound<-floor(elapsed_time)
  
  #create temporary arrival_rates_df with only what we need
  temp_arrival_rates_df <- arrival_rates_df %>%
      filter(hour==lower_bound, start_station==starting_station, 
             end_station==ending_station)
  
  #prob_keep is consistent across all rows left,
  #so we can index the first input and it will be equal
  prob<- temp_arrival_rates_df$prob_keep[1]
  
  #run flip a coin w rbinom: 1 success out of 1 trial with probability = prob. 
  #this will equal 0 or 1.
  keep <- rbinom(1, 1, prob)
  
  return(keep)
}
