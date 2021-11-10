# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)
library(here)

# Scrape Data -------------------------------------------------------------

#Initialize empty tibble with proper column types
full_data <- tibble(state = character(),
              year = character(),
              period = character(),
              labor_force_participation_rate = character(),
              employment_population_ratio = character(),
              labor_force = character(),
              employment = character(),
              unemployment = character(),
              unemployment_rate = character())

#For each 56 states and territories, grab the data
for (i in 1:56){
  tryCatch({
    #Deal with leading zeroes
    if (i <10) {
      i <- paste0("0",i)
    }
    
    print(i)
    
    #get URL
    URL <- glue::glue("https://data.bls.gov/timeseries/LASST{i}0000000000003")
    print(URL)
    
    #Get data table labor market indicators
    data <- URL %>%
      read_html() %>% 
      html_nodes(xpath = '//*[@id="table0"]') %>% 
      html_table()
    
    # Unnest the data
    data <- data[[1]]
    
    # Get the state name for this iteration
    state <- URL %>%
      read_html() %>% 
      #This is a table that has state name in it
      html_nodes(xpath = '//*[@id="catalog0"]') %>% 
      html_table()
    
    # Extract state name from above table
    state <- state[[1]] %>% 
      filter(X1 == "State/Region/Division:") %>% 
      pull(X2)
    
    #Add state name to data
    data <- data %>% 
      mutate(state = state) %>% 
      clean_names() %>% 
      relocate(state, .before = everything())
    
    # Bind this iteration to full data
    full_data <- full_data %>% bind_rows(data)
  
  #Handle errors
  }, error = function(e){
    message(glue::glue("Scrape failed for i = {i}"))
  })
  
}



# Save Data ---------------------------------------------------------------
write_csv(full_data, here("raw-data/state-labor-markets.csv"))
