train_parameters <- function(data, n_windows = 7, window_length = 15) {
  
  cat('--------------------------------\n')
  cat('Training Model Parameters\n')
  cat('--------------------------------\n\n')
  
  #Define training parameters
  n_windows <- n_windows
  window_length <- window_length
  current_date <- Sys.Date()
  
  #Define weights for the periods. I assign the highest weight to lates 3 windows
  first_3_periods_weigths <- c(0.3, 0.25, 0.2)
  weigths_for_the_rest <- rep((1-sum(first_3_periods_weigths))/(n_windows - 3), n_windows - 3)
  period_weigths <- c(first_3_periods_weigths, weigths_for_the_rest)
  
  train_list <- vector(length = n_windows, mode = 'list')
  
  for(i in 1:n_windows) {
    
    #Define training period
    window_end <- current_date - (i - 1)
    window_start <- current_date - 15 - (i - 1)
    
    #Define a training dataset
    train_data <- data[data$datestamp >= window_start & data$datestamp <= window_end,]
    train_data_scores <- train_data$`1_and_2_and_3_and_4_total_points_out_of_1200`
    
    #Calculate number of entries in the spreadsheet for the given training period
    new_entries <- nrow(train_data)
    
    #Calculate spreadsheet rate of growth
    spreadsheet_growth_rate <- round(new_entries/window_length, 1)
    
    #Get how many scores are above 600
    num_scores_600_plus <- sum(train_data_scores > 600)
    
    #Share of entries that have LMIA
    lmia_prob <- round(num_scores_600_plus/new_entries, 3)
  
    train_list[[i]] <- data.frame(window_start = window_start,
                                  window_end = window_end,
                                  spreadsheet_growth_rate = spreadsheet_growth_rate,
                                  lmia_prob = lmia_prob)
  }
  
  trained_windows <- cbind(do.call('rbind', train_list), weight = period_weigths)
  
  list(spreadsheet_growth_rate = sum(trained_windows$spreadsheet_growth_rate * trained_windows$weight, na.rm = TRUE),
       lmia_prob = sum(trained_windows$lmia_prob * trained_windows$weight, na.rm = TRUE))
  
}

