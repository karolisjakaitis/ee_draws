source(file = 'scripts/clean_data.R')
source(file = 'scripts/train_parameters.R')

perform_draw <- function(days_to_simulate = 120,
                         spreadsheet_total_share = 0.05,
                         draws_per_month = 2,
                         draw_size = 1600,
                         lmia_prob = NULL,
                         draw_dates = NULL,
                         save_results = FALSE) {
  
#   days_to_simulate <- 120
#   spreadsheet_total_share <- 0.05
#   draws_per_month <- 2
#   draw_size <- 1600
#   lmia_prob <- NULL
  
  #Get clean data
  data <- clean_data()
  draws <- draw_data()
  
### DEFINE CONSTANTS ####
  
  #Define draw dates if specified
  if(is.null(draw_dates)) {
    draw_dates <- Sys.Date()
  }

  # Last draw date
  last_draw_date <- draws$date

  #Retrieve the minimum CRS at the last ITA
  start_min_score <- draws$min_crs

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
  save(all_scores, file = 'all_scores')

  #Number of people in the spreadsheet that did not receive ITA yet
  people_without_ITA <- nrow(data[all_scores < start_min_score & data$datestamp <= last_draw_date,])
  new_people_after_last_draw <- nrow(data[data$datestamp > last_draw_date,])
  starting_number_people <- people_without_ITA + new_people_after_last_draw

  #Get all scores in the spreadsheet without LMIA 
  score_distribution <- sort(all_scores[all_scores<600 & all_scores>0])


### TRAIN PARAMETERS ####

  #Train parameters
  parameters <- train_parameters(data)
  
  #Spreadsheet rate of growth
  spreadsheet_growth_rate <- parameters$spreadsheet_growth_rate

  #Share of entries that have LMIA
  if(is.null(lmia_prob)) {
    lmia_prob <- parameters$lmia_prob
  } else {
    lmia_prob <- lmia_prob
  }

  #Old algorithm of training
  
  #   #Define a training period
  #   start <- as.Date('2015-03-01')
  #   end <- as.Date('2015-03-28')
  #   period_in_days <- as.numeric(end-start)
  # 
  #   #Define a training dataset
  #   train_data <- data[data$datestamp >= start & data$datestamp <= end,]
  #   train_data_scores <- train_data$`1_and_2_and_3_and_4_total_points_out_of_1200`
  # 
  #   #Calculate number of entries in the spreadsheet for the given training period
  #   new_entries <- nrow(train_data)
  #   
  #   #Calculate spreadsheet rate of growth
  #   spreadsheet_growth_rate <- round(new_entries/period_in_days, 1)
  # 
  #   #Get how many scores are above 600
  #   num_scores_600_plus <- sum(train_data_scores > 600)
  # 
  #   #Share of entries that have LMIA
  #   if(is.null(lmia_prob)) {
  #     lmia_prob <- round(num_scores_600_plus/new_entries, 3)
  #   } else {
  #     lmia_prob <- lmia_prob
  #   }
  
  
 
### POPULATION PARAMETERS ####

  #Calculate growth rate of the total pool
  pool_growth <- spreadsheet_growth_rate/spreadsheet_total_share
  
  #Calculate the size of the total pool at day 0
  total_pool <- starting_number_people/spreadsheet_total_share
  

### POPULATE THE POOL ####
  #Populate the pool based on the scores distribution after the last draw
  pool <- sample(score_distribution[score_distribution < start_min_score], total_pool, replace=TRUE)


### PERFORM A SIMULATION ####

  cat('--------------------------------\n')
  cat('Starting Simulation\n')
  cat('--------------------------------\n\n')

  #Run simulation for specified length
  n_draw <- 1
  for(i in 1:days_to_simulate) {
    
    day <- Sys.Date() + i
    
    #Simulate new entries to the pool
    new_entries <- sample(score_distribution, pool_growth, replace=TRUE)
    
    #Estimate how many entries have LMIA
    lmia_entries <- which(runif(pool_growth) <= lmia_prob)
    new_entries[lmia_entries] <- new_entries[lmia_entries] + 600
    
    #Append new entries to the existing pool
    pool <- c(pool, new_entries)
    
    #Perform a draw
    
    if(is.null(draw_dates) | day > max(as.Date(draw_dates))) {
      if(i %% draw_days == 0) {
        
        if(length(pool) < draw_size) {
          print('-----NEW DRAW INCOMING------ ')
          print(paste0('#', n_draw, '; Draw will take place on: ', Sys.Date()+i))
          print(paste0('There is only: ', length(pool), ' people in the pool and you are trying to draw ,', draw_size, '!!'))
          print('No draw today')
          cat('\n')
          
        } else {
          print('-----NEW DRAW INCOMING------ ')
          print(paste0('#', n_draw, '; Draw will take place on: ', Sys.Date()+i))
          print(paste0('Minimum score required for ITA: ', sort(pool, decreasing = T)[draw_size + 1])) 
          cat('\n')
          
          #Remove people from the pool that received ITA
          pool <- pool[pool<sort(pool, decreasing = T)[draw_size + 1]]       
          
        }
        
        n_draw <- n_draw + 1
        
      }
      
    } else if(day %in% as.Date(draw_dates)) {
      
      if(length(pool) < draw_size) {
        print('-----NEW DRAW INCOMING------ ')
        print(paste0('#', n_draw, '; Draw will take place on: ', Sys.Date()+i))
        print(paste0('There is only: ', length(pool), ' people in the pool and you are trying to draw ,', draw_size, '!!'))
        print('No draw today')
        cat('\n')
        
      } else {
        print('-----NEW DRAW INCOMING------ ')
        print(paste0('#', n_draw, '; Draw will take place on: ', Sys.Date()+i))
        print(paste0('Minimum score required for ITA: ', sort(pool, decreasing = T)[draw_size + 1])) 
        cat('\n')
        
        #Remove people from the pool that received ITA
        pool <- pool[pool<sort(pool, decreasing = T)[draw_size + 1]]       
        
      }
      
      n_draw <- n_draw + 1
      
    }
    
    i <- i + 1
    
  }

  cat('--------------------------------\n')
  cat('End of Simulation\n')
  cat('--------------------------------\n\n')

}
