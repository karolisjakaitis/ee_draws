source(file = 'scripts/clean_data.R')

draw <- function(days_to_simulate = 120,
                 spreadsheet_total_share = 0.05,
                 draws_per_month = 2,
                 draw_size = 1600) {
  
  #Get clean data
  data <- clean_data()
  
  
### DEFINE CONSTANTS ####
  
  # Last draw date
  last_draw_date <- as.Date('2015-03-27')

  #Retrieve the minimum CRS at the last ITA
  start_min_score <- 453

  days_to_simulate <- days_to_simulate  

  #Assume the share of population that spreasheet is representing
  spreadsheet_total_share <- spreadsheet_total_share

  #Define number of draws per month
  draws_per_month <- draws_per_month
  draw_days <- 30/draws_per_month
  
  #Assume number of ITA issued
  draw_size <- draw_size

  #Scores vector
  all_scores <- data$`1_and_2_and_3_and_4_total_points_out_of_1200` 

  #Number of people in the spreadsheet that did not receive ITA yet
  people_without_ITA <- nrow(data[all_scores < start_min_score & data$datestamp <= last_draw_date,])
  new_people_after_last_draw <- nrow(data[data$datestamp > last_draw_date,])
  starting_number_people <- people_without_ITA + new_people_after_last_draw

  #Get all scores in the spreadsheet without LMIA 
  score_distribution <- sort(all_scores[all_scores<600 & all_scores>0])


### TRAIN PARAMETERS ####

  #Define a training period
  start <- as.Date('2015-03-01')
  end <- as.Date('2015-03-28')
  period_in_days <- as.numeric(end-start)

  #Define a training dataset
  train_data <- data[data$datestamp >= start & data$datestamp <= end,]
  train_data_scores <- train_data$`1_and_2_and_3_and_4_total_points_out_of_1200`

  #Calculate number of entries in the spreadsheet for the given training period
  new_entries <- nrow(train_data)
  
  #Calculate spreadsheet rate of growth
  spreadsheet_growth_rate <- round(new_entries/period_in_days, 1)

  #Get how many scores are above 600
  num_scores_600_plus <- sum(train_data_scores > 600)

  #Share of entries that have LMIA
  lmia_prob <- round(num_scores_600_plus/new_entries, 3)
  
 
### POPULATION PARAMETERS ####

  #Calculate growth rate of the total pool
  pool_growth <- spreadsheet_growth_rate/spreadsheet_total_share
  
  #Calculate the size of the total pool at day 0
  total_pool <- starting_number_people/spreadsheet_total_share
  

### POPULATE THE POOL ####
  #Populate the pool based on the scores distribution after the last draw
  pool <- sample(score_distribution[score_distribution < start_min_score], total_pool, replace=TRUE)


### PERFORM A SIMULATION ####
  #Run simulation for specified length
  for(i in 1:days_to_simulate) {
    
    #Simulate new entries to the pool
    new_entries <- sample(score_distribution, pool_growth, replace=TRUE)
    
    #Estimate how many entries have LMIA
    lmia_entries <- which(sample(1:1000, pool_growth, replace=TRUE) <= lmia_prob * 1000)
    new_entries[lmia_entries] <- new_entries[lmia_entries] + 600
    
    #Append new entries to the existing pool
    pool <- c(pool, new_entries)
    
    #Perform a draw
    if(i %% draw_days == 0) {
      
      if(length(pool) < draw_size + 1) {
        print('-----NEW DRAW INCOMING------ ')
        print(paste0('Draw will take place on: ', Sys.Date()+i))
        print(paste0('There is only: ', length(pool), ' people in the pool and you are trying to draw 3000!!'))
        print('No draw today')
        cat('\n')
        
      } else {
        print('-----NEW DRAW INCOMING------ ')
        print(paste0('Draw will take place on: ', Sys.Date()+i))
        print(paste0('Minimum score required for ITA: ', sort(pool, decreasing = T)[draw_size + 1])) 
        cat('\n')
        
        #Remove people from the pool that received ITA
        pool <- pool[pool<sort(pool, decreasing = T)[draw_size + 1]]       
        
      }
      
    }
    
    i <- i + 1
    
  }
}
