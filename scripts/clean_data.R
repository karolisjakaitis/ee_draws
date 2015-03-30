library(magrittr)
library(lubridate)

clean_data <- function() {
  #Url for spreadsheet pool data
  url_pool <- 'https://docs.google.com/spreadsheets/d/13Hf5KT35xOJMzlHo9KVenqYSrZFTjt0qc_PZgrI4mls/export?gid=2117108251&format=csv'
  
  #Load spreadsheet pool data
  data <- read.csv(file=url_pool, stringsAsFactors=FALSE)
  
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

draw_data <- function(last_draw = TRUE) {  
  #Url for draws info
  url_draws <- 'https://docs.google.com/spreadsheets/d/13Hf5KT35xOJMzlHo9KVenqYSrZFTjt0qc_PZgrI4mls/export?gid=1489650995&format=csv'
  
  #Load spreadsheet pool data
  data <- read.csv(file=url_draws, stringsAsFactors=FALSE)
  
  #Get draw data
  if(last_draw) {
    last_draw_col <- ncol(data)
    data <- data[c(1,last_draw_col)]
    
  }
  
  #Initialize rows of interest
  day_r <- which(data[,1] == 'Draw Date')
  crs_r <- which(data[,1] == 'Cutoff CRS Score (out of 1200)')
  ita_r <- which(data[,1] == 'Number of ITAs')
  
  if(length(day_r) == 0 | length(crs_r) == 0 | length(ita_r) == 0) {
    stop('Draws spreadsheet has changed. Please check draw_data function from clean_data.R file')
    
  }
  
  draws <- data.frame(date=vector(), min_crs=vector(), number_ita=vector())
  
  for(i in 2:ncol(data)) {
    
    #Get date information
    month <- match(tolower(gsub('[\\. 0-9]', '', names(data)[i])), tolower(month.name))
    day <- gsub('[a-zA-Z]', '', data[day_r, i])
    year <- year(Sys.Date()) 
    draw_date <- as.Date(paste(year, month, day, sep='-'))
   
    if(as.numeric(Sys.Date()-draw_date) > 60) {
      stop('Something is wrong with the draw dates. Please check draw_data function from clean_data.R file')
    }
    
    #Get min CRS score
    crs <- as.numeric(data[crs_r, i])
    
    #Get number of ITA
    ita <- as.numeric(data[ita_r, i])
    
    #Update the data.frame
    draws <- rbind(draws, data.frame(date=draw_date, min_crs=crs, number_ita=ita))
    
  }
  
  draws
  
}
