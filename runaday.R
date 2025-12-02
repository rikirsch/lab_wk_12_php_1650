#note: UNCOMMMENT ONCE BUG IS FIXED source(thinning)
#'run_a_day function
#'
#' @description simulates a singular day of bike usage based off the observed
#' data, the calculated mean arrival rates, and the proposed initial bike placement 
#' 
#' @param arrival_rates_df data frame of estimated average arrival rates (x_hat)
#'  for every combination of hour (h), start_station (s), and end_station (t).
#' @param bike_placement data frame showing how many bikes (num_initial_bikes) are 
#' initially placed are each station
#' 
#' @return data frame showing the total number of unhappy people that come to each stop

run_a_day <- function(arrival_rates_df, bike_placement){
  
  #FIND LAMBDA MAX FOR EACH PAIR OF STATIONS.
  
  lambda_maxes <- arrival_rates_df %>%
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
    mutate(prob_keep = case_when( is.na (prob_keep) ~ 0, TRUE ~ prob_keep))
  
  
  #SAMPLE NEW ARRIVAL TIMES
  #for each pair of stations, draw exponential random variables until the end 
  #of the day, testing each to see if it is eliminated by thinning
  
  
  #initialize simulated_arrivals
  simulated_arrivals <- data.frame("start_station" = 
                                     "TEMP", 
                                   "end_station" = "TEMP", 
                                   "arrival_time" = "TEMP")
  
  #set up for loop
  for(i in 1:nrow(lambda_maxes)) {
    
    #update start and end stations
    start_station = lambda_maxes$start_station[i]
    end_station = lambda_maxes$end_station[i]
  
    #initialize time variable, elapsed_time  = hours since day started
    #once 24 hours have passed, the day is complete. 
    elapsed_time <- 0
    
    #establish while loop until end of day
    while (elapsed_time < 24) {
      
      #store random variable of the interval in which next arrival comes
      next_arrival<-rexp(1, rate =  lambda_maxes$lambda_max[i])
      
      #update elapsed time to designate time passed
      elapsed_time <- elapsed_time + next_arrival
      
      
      #RUN THINNING FUNCTION to see if observation will be kept
      keep <- thinning(elapsed_time, arrival_rates_df, 
                       start_station, end_station)
      #if keep is = 1, add a new row to simulated arrivals. otherwise, 
      # do nothing
      if(is.na(keep)){
        keep<-0
      }
      else if (keep==1){
        new_row <- data.frame("start_station" = 
                                 start_station, 
                               "end_station" = end_station, 
                               "arrival_time" = elapsed_time)
        simulated_arrivals <- rbind(simulated_arrivals, new_row)
      }
      

    #end while loop
    }
  
  #end for loop
  }
  #remove temp row
  simulated_arrivals <- simulated_arrivals %>% 
    filter(start_station != "TEMP")
  
  #this should be all the arrival simulated, for each pair of stations
  
  #NEXT: order all the arrivals so that one can more easier check if bikes are
  #available
  simulated_arrivals <- simulated_arrivals %>% 
    arrange(arrival_time)  %>%
    #rename column name simulated arrivals start_station to station
    rename(station = start_station)
  
  #join data set with bikes 
  bike_simulation <- left_join(simulated_arrivals, bike_placement)
  
  #mutate to include unhappy_customers, event_success
  bike_simulation <- bike_simulation%>%
    mutate(unhappy_customers=0) %>%
    rename(bikes_available = num_initial_bikes)
  
  #cycle through each row of bike_simulation to see if each an event occurs.
  for (i in nrow(bike_simulation)){
    #test if bike available 
    if (bike_simulation$bikes_available[i] >=1){
      
      #do NOT add an unhappy customer. 
      
      #cycle through rows below and add a bike to each occurrence of the end 
      #station that bike is moved to and subtract a bike from each occurrence of 
      #the start station that bike is taken from
      
      for (j in (i+1):nrow(bike_simulation)){
        if(is.na(bike_simulation$station[j]) | is.na(bike_simulation$station[i])){
          #do nothing
          break
        }
        #if start stations are the same:
        if (bike_simulation$station[j] == bike_simulation$station[i]){
          bike_simulation$bikes_available[j] <- bike_simulation$bikes_available[j] -1
          
        }
        
        #if end stations are the same: 
        if (bike_simulation$end_station[j] == bike_simulation$end_station[i]){
          bike_simulation$bikes_available[j] <- bike_simulation$bikes_available[j] +1
          
        }
      
      }
      
    }
    #if bikes are NOT available 
    else {
      #turn the value of the unhapphy_customers in the row to 1
      bike_simulation$unhappy_customers[i] <- 1
    }
      
    
    
    
  }
  
  #total number of unhappy people to arrive at each station through the day
  bike_simulation <- bike_simulation%>% 
    group_by(station) %>%
    summarize(unhappy_customers=sum(unhappy_customers)) %>%
    #order by start station
    arrange(station)
  
  #return data frame
  return(bike_simulation)
  
}
  

