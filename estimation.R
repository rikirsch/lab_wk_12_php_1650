#'Estimating Arrival Rates for Bike Share Program 
#' @description uses the data and models a non-homogeneous Poisson process
#'  to calculate the average arrival rates in hour h from each pair
#'  of starting and ending stations. 
#' @param cleaned_bike_df cleaned data frame of starting stations (start_station),
#'  destination stations (end_station), and hour of bike arrivals (end_time).
#'   Each observation is one bike trip from a start_station to an end_station.
#' @retufrn res data frame of estimated average arrival rates (x_hat)
#'  for every combination of hour (h), start_station (s), and end_station (t).

arrival_rates <- function(cleaned_bike_df){
  x_hat_df <- cleaned_bike_df %>%
    #add a column for day
    mutate(day = day(end_time))
  #find the number of days in the data
  n_days <- length(unique(x_hat_df$day))
  
  res <- x_hat_df %>% 
    #add a column for hour
    mutate(hour =  hour(end_time)) %>%
    #group by the the parameters of interest
    #(h, s, t as hour, start_station, end_station)
    group_by(hour, start_station, end_station) %>%
    #calculate the x_hat
    summarize(x_hat = n()/n_days)
  return(res)
}







#Code From Ed Stem with comments of how it relates to our code (will change)
estimate_arrival_rates <- function(data) {
  #This chunk is done in the data cleaning function 
  # data <- data %>%
  # mutate(
  #   start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
  #   end_time   = as.POSIXct(end_time,   format = "%Y-%m-%d %H:%M:%S")
  # )
  
  #This chunk is done in the bottom half of my code from lab 11 
  #(where I define and return res)
  # compute the average number of trips per hour between each pair
  # x_hat <- data %>%
  #   mutate(hour = hour(start_time)) %>%
  #   filter(start_station != "R", end_station != "R") %>%
  #   group_by(start_station, end_station, hour) %>%
  #   summarise(avg_trips = n() / n_distinct(as_date(start_time)), 
  #             .groups = "drop") 
  
  # pivot longer to get change in count 
  data$end_station <- as.character(data$end_station)
  trips_long <- data %>%
    pivot_longer(cols = c("start_station", "start_time", 
                          "end_station", "end_time"),
                 names_to = c("type", ".value"),   
                 names_pattern = "(start|end)_(.*)") %>%
    mutate(change = ifelse(type == "start", -1, 1),
           hour = hour(time)) %>%
    select(station, time, hour, change)
  
  # add hour markers so we can get cumulative time
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0,23,1), seq(0,23,1)+0.9999999)
  stations <- unique(trips_long$station)
  hr_pts <- expand.grid(time = dates, hour = hours, 
                        station = stations) %>%
    mutate(time = as.POSIXct(time) + hour*60*60,
           hour = hour(time))
  hr_pts$change <- 0
  trips_long <- rbind(trips_long, hr_pts)
  
  # find average availability 
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    #filter(station != "R") %>% # is in the data cleaning, can still keep as a check
    arrange(time) %>%  #order it by time
    #sum the number of bikes at the place 
    #(bc you're subtracting one for every bike starting there
    #and adding one for every bike ending there)
    mutate(count = cumsum(change), 
           #mututate to add a col for the date
           date = as_date(time)) %>%
    group_by(station, hour, date) %>%
    #add a column to find the amnt of time a bike is available at each station/hr/date
    summarize(time_avail = 
                sum(difftime(time, lag(time), units="hours")*(count > 0), 
                    na.rm = TRUE)) %>%
    #add a col to find the average amnt of time a bike is available at each station/hr/date
    summarize(avg_avail = mean(time_avail)) %>%
    #round the avg_avail
    mutate(avg_avail = round(as.numeric(avg_avail), digits = 4)) %>%
    ungroup()
  
  # join the data and compute arrival rates
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    #add a col that is the avg_trips/avg_available_bikes if avg_avail > 0
    #bc you don't want to decide by 0 or calculate the arrival rates
    #if there are no bikes there
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA))
  
  return(mu_hat)
}