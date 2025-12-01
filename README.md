# lab_wk_12_php_1650
Emmett and Rachel take on the week 12 lab PHP1650

This repository finds the optimal bike placement for a bike share program given they have n number of stations and k bikes. 

The datacleaning.R script should be run first to clean the data. Then call the estimation.R script on the results of datacleaning to estimate the average arrival times at each station and hour. Finally, call optimization.R on the results of estimation to optimize where the bikes should start to make the least customers unhappy. Optimize.R calls the runaday.R script which calls the Thinning.R script to run a simulation of bike placements. The simulation will find the number of unhappy people at each station depending on the placements given as an argument to runaday as are chosen by the optimize function. Optimize.R will return the dataframe of interest with the first column representing each possible station and the second column representing the optimal number of bikes to start at each station.

Results for optimal bike placement for small, medium, and large fleets of bikes can be found under the results folder. Results are shown as a dataframe with the stations listed in column one and the optimal initial bikes for the station listed in column two.

Current bugs:
- Currently, thinning is creating an error by finding no temp_row and then trying to use a NULL probability to run rbinom