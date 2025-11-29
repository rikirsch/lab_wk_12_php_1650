#'Estimating Arrival Rates for Bike Share Program 
#' @description uses the data and models a non-homogeneous Poisson process
#'  to calculate the average arrival rates in hour h from each pair
#'  of starting and ending stations. 
#' @param cleaned_bike_df cleaned data frame of starting stations (start_station),
#'  destination stations (end_station), and hour of bike arrivals (end_time).
#'   Each observation is one bike trip from a start_station to an end_station.
#' @return res data frame of estimated average arrival rates (x_hat)
#'  for every combination of hour (h), start_station (s), and end_station (t).

arrival_rates <- function(cleaned_bike_df){
  #Find the avg number of bikes at hr h 
  x_hat <- cleaned_bike_df %>% 
    #filter out Reset runs here so that it is still included in calculating the dates
    filter(start_station != "R", end_station != "R") %>%
    #group by the the parameters of interest
    #(h, s, t as hour, start_station, end_station)
    group_by(hour, start_station, end_station) %>%
    #calculate the x_hat
    summarize(avg_trips = n()/n_distinct(as_date(start_time)))
  
  
  # pivot longer to get change in count (number of bikes at the station)
  cleaned_bike_df$end_station <- as.character(cleaned_bike_df$end_station)
  trips_long <- cleaned_bike_df %>%
    #make each observation into two rows, one row for the starting information 
    #(station and time) and one row for the ending information (station and time)
    #station and time are the new columns
    pivot_longer(cols = c("start_station", "start_time", 
                          "end_station", "end_time"),
                 names_to = c("type", ".value"),   
                 names_pattern = "(start|end)_(.*)") %>%
    #add a col change that is 1 if a bike is arriving at the station (end)
    #and -1 is a bike is leaving the station (start)
    mutate(change = ifelse(type == "start", -1, 1),
           hour = hour(time)) %>%
    #keep the columns for station (#), time (#), hour (hour time), change(-1, 1)
    select(station, time, hour, change)
  
  
  # add hour markers so we can get cumulative time
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0,23,1), seq(0,23,1)+0.9999999)
  stations <- unique(trips_long$station)
  #add all possible combinations of day, hour, and station
  hr_pts <- expand.grid(time = dates, hour = hours, 
                        station = stations) %>%
    #make time in terms of day and hour and add a column for hour
    mutate(time = as.POSIXct(time) + hour*60*60,
           hour = hour(time))
  #give the stand in time/station pairs a neutral (0) change (instead of -1, 1)
  hr_pts$change <- 0
  #bind the time/station pairs with the neutral changes to the long data frame
  trips_long <- rbind(trips_long, hr_pts)
  
  
  # find average bike availability 
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    #order data by time
    arrange(time) %>%  
    #sum the number of bikes at the place 
    #-1 for every bike starting there and +1 for every bike ending there
    mutate(count = cumsum(change), 
           #mutate to add a col for the date
           date = as_date(time)) %>%
    group_by(station, hour, date) %>%
    #add a column to find the amnt of time a bike is available at each station/hr/date
    summarize(time_avail = 
                sum(difftime(time, lag(time), units = "hours")*(count > 0), 
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
    #since you don't want to divide by 0 or calculate the arrival rates
    #if there are no bikes there
    #NEED TO RENAME MU_HAT
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA))
  
  return(mu_hat)
}

