# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(glue)


# Initialize empty tibbles to bind to -------------------------------------
state <- tibble()
state_sector <- tibble()


# Extract Weekly Pulse Data -----------------------------------------------

# For each week...
for(i in 1:54){
  tryCatch({
    #Unzip file for given week and save it in unzipped location
    unzip(zipfile = here(glue("raw-data/pulse-data/zipped/week{i}.zip")),
          exdir = here(glue("raw-data/pulse-data/unzipped/week{i}/")))
    
    #Get the list of files names, but only keep
    # the state and state sector ones
    files <- list.files(here(glue("raw-data/pulse-data/unzipped/week{i}"))) %>% 
      as_tibble() %>% 
      filter(str_detect(value, "national_state")) %>% 
      #Arrange so index 1 and 2 always correspond to the right file
      arrange()
    
    # Load the state data for week i
    state_to_bind <- readxl::read_excel(
      here(
        glue("raw-data/pulse-data/unzipped/week{i}/{files$value[1]}"))) %>% 
      mutate(date = str_remove(files$value[1], "national_state_"),
             date = str_remove(date, ".xlsx"))
    
    # Load the state sector data for week i
    state_sector_to_bind <-  readxl::read_excel(
      here(
        glue("raw-data/pulse-data/unzipped/week{i}/{files$value[2]}"))) %>% 
      mutate(date = str_remove(files$value[2], "national_state_sector_"),
             date = str_remove(date, ".xlsx"))
    
    #Bind data
    state <- state %>% 
      bind_rows(state_to_bind)
    
    state_sector <- state_sector %>% 
      bind_rows(state_sector_to_bind)
  
    }, error = function(e){
    message(glue::glue("Unzip failed for i = {i}"))
  }) 
  print(glue("Week {i} completed. {round((i/54)*100,2)} percent of the way done"))
}


# Save Data ---------------------------------------------------------------
write_csv(state, here("raw-data/pulse-state-data.csv"))
write_csv(state_sector, here("raw-data/pulse-state-sector-data.csv"))

