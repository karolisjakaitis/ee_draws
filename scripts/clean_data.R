library(magrittr)

clean_data <- function() {
  data <- read.csv(file = 'data/Express Entry Spreadsheet - CRS - Entry Sheet.csv', stringsAsFactors=FALSE)
  
  #Remove the notes how to add a new entry
  data <- data[-1,]
  
  #Clean up the header
  header <- unname(t(data[1,])[,1]) %>% 
    tolower() %>% 
    gsub(' ', '_', .) %>% 
    gsub('[), ?, (, /]', '', .) %>%
    gsub('\\+', '_and_', .)
  
  #Assign the header
  names(data) <- header
  
  #Remove the header row from data
  data <- data[-1,]
  
  #remove NA columns
  data <- data[,-which(is.na(names(data)))]
  rownames(data) <- NULL
  
  #Replace invalid datestamps with date from timestamp
  invalid_dates <- !grepl('[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}', as.character(data$datestamp))
  matches <- regexpr('[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}', data$timestamp[invalid_dates])
  data$datestamp[invalid_dates] <- regmatches(data$timestamp[invalid_dates], matches)
  
  #Remove invalid datestamps that do not have valid timestamps either
  data <- data[grepl('[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}', as.character(data$datestamp)),]
  
  #Convert datestamp to date
  data$datestamp <- as.Date(as.character(data$datestamp), "%m/%d/%Y")
  
  #Convert total score to numeric
  data$`1_and_2_and_3_and_4_total_points_out_of_1200` <- as.numeric(data$`1_and_2_and_3_and_4_total_points_out_of_1200`)
  
  data
}
