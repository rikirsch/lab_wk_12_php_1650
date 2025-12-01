source(datacleaning)
source(estimation)
source(optimization)

#' Utilize the full bike optimal placement program
#' @description This function calls the necessary functions to find the optimal
#'  bike placement when given a dataframe of bike data, the total number of bikes
#'  to place, and the number of bikes to place at a time during simulation.
#' @param bike_datafram the dataframe of bike start and end stations
#' and the start and end time of use
#' @param total_bike_num double, the total number of bikes that can be placed,
#' default 100
#' @param step_size_bikes double, the number of bikes to place at a time during
#'  simulation, default 5
#'  @return a data frame with the optimal number of bikes placed at each station
#'  at the start of the day

util <- function(bike_dataframe, total_bike_num = 100, step_size_bikes = 5){
  #clean the data frame
  my_cleaned_bikes <- bike_data_clean(bikes)
  #estimate average arrival times
  data_samp_estim_arriv <- arrival_rates(my_cleaned_bikes)
  #find the optimized placement
  prefered_placement <- optimize(data_samp_estim_arriv,
                                 num_bikes = total_bike_num,
                                 bike_step_size = step_size_bikes)
  #return optimal placement data frame
  return(prefered_placement)
}
