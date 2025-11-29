#source(RunADay)
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

optimize <- function(arrival_rates_df, num_bikes, num_days = 10, bike_step_size = 1){
  replicate(n = num_days, run_a_day(arrival_rates_df)) #run_a_day is the simulation function,
  #run_a_day should return a df with the simulated start_station, end_station, time, and happy_rating 
  #(1 - bike was there, 0 - bike was not there)
  #how do I call the arrival_rates_df, do I call optimize from estimation? (then I can leave it like this)
  #but then I'll need to pass the num_bikes, num_days, and bike_step_size into the estimation params
  placed_bikes = 0
  while(placed_bikes < num_bikes){
    #look through the 
  }
}