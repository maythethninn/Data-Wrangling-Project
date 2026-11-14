library(fs)
library("tidyverse")

data_folder <- path("data")
output_csv <- "out/filtered_dataset.csv"

scrape_dates <- c(
  "October 2025" = "2025-10-05",
  "November 2025" = "2025-11-07",
  "December 2025" = "2025-12-11",
  "January 2026" = "2026-01-16",
  "February 2026" = "2026-02-13",
  "March 2026" = "2026-03-17",
  "April 2026" = "2026-04-16",
  "May 2026" = "2026-05-23",
  "June 2026" = "2026-06-19",
  "July 2026" = "2026-07-12",
  "August 2026" = "2026-08-13"
)


filter_airbnb_data <- function() {
  
  # Find only monthly Airbnb CSV files
  csv_files <- character()
  
  for (file_path in list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)) {
    print(file_path)
    
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    if (grepl("^[A-Za-z]{3}[0-9]{4}$", file_name)) {
      file_date <- tryCatch(
        # as.Date(file_name, format= "%b%Y"),
        as.Date(paste0("01", file_name), format = "%d%b%Y"),
        error = function(e)
          NA
        
      )
      # print(file_date)
      if (!is.na(file_date)) {
        csv_files <- c(csv_files, file_path)
        
      }
    }
  }
  
  # Sort files chronologically
  csv_files <- csv_files[
    order(as.Date(
      paste0("01", tools::file_path_sans_ext(basename(csv_files)))
      , format = "%d%b%Y"
    ))
  ]
  
  # Check how many Airbnb CSV files were found
  print(paste("Airbnb files found:", length(csv_files)))
  
  # Create dataframe list
  df  <- list()
  
  # Go through all CSV files
  for (file_path in csv_files){
    
    # Extract month and year from filename
    date <- format(
      as.Date(
        paste0("01", file_name <- tools::file_path_sans_ext(basename(file_path)))
        , format = "%d%b%Y"
      ),
      format = "%B %Y"
    )
    
    # Read CSV
    dataset <- read_csv(file_path, show_col_types = FALSE)
    
    # Filter for Christchurch City
    filtered_data <- dataset[dataset$neighbourhood_group == "Christchurch City",]
    
    # Add month + year
    filtered_data$date <- date
    
    # Add scrape date
    filtered_data$scrape_date <- scrape_dates[[date]]
    
    # Add dataframe to list
    df <- append(df, list(filtered_data))
    
  }
  
  # Concatenate all months
  concat_dataset <- do.call(rbind,df)
  
  # Save final dataset
  write.csv(concat_dataset, output_csv, row.names = FALSE)
  
  # Print saved file location
  print(paste("Filtered dataset saved to", output_csv))

  return(concat_dataset)
}

# filter_airbnb_data()
