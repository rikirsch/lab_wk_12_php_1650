#source(runaday) #uncomment once bug is fixed

#'Optimizing Bike Placement for Bike Share Program 
#' @description runs the RunADay simulation code n_day times to find the best 
#' initial bike placement for k bikes
#' @param arrival_rates_df  data frame 
#' @param num_bikes a double, the total number of bikes that can be placed
#' @param num_days a double, the number of days to simulate,
#'  default is 10
#' @param bike_step_size a double, the number of bikes to place at a time,
#'  default is 1
#' @return res table of the number of bikes to place at each station
#' at the start of the day


#NOTES/QUESTIONS:
#run_a_day is the simulation function,
#run_a_day should return a df with the simulated start_station,
#end_station, time, and unhappy_customers 
#(1 - bike was not there, 0 - bike was there)
#might not need end station or time to be returned 
#simulation must be arranged by start_station

optimize <- function(arrival_rates_df, num_bikes, num_days = 10, bike_step_size = 1){
  #make an empty vector that will store the number of bikes at each numbered station
  bike_placement_vect <- c(rep(0, times = n_distinct(arrival_rates_df$start_station)))
  #make a dataframe to store the final results/recommended initial bike placement
  bike_placement_df <- data.frame(station = c(unique(arrival_rates_df$start_station)),
                                  num_initial_bikes = bike_placement_vect)
  
  #while not all bikes have been placed
  while(sum(bike_placement_df[ ,2]) < num_bikes){
    
    #simulate placement for num_days
    sim_placement <- replicate(n = num_days,
                               run_a_day(arrival_rates_df, bike_placement_df[,2]))
    
    #group by the starting station and find the average number of unhappy 
    #customers at each starting station
    sim_placement <- sim_placement %>%
      group_by(start_station) %>%
      summarize(avg_unhappy = mean(unhappy_customers))
    
    #find which station has the most unhappy customers
    unhappy_station <- which.max(sim_placement$avg_unhappy)
    
    #place bike_step_size number of bikes at the station with the most unhappy customers
    bike_placement_df[unhappy_station, 2] <- bike_placement_df[unhappy_station, 2] + bike_step_size
  
  }
  
  #return the number of bikes that should be placed at each station
  return(bike_placement_df)
}
