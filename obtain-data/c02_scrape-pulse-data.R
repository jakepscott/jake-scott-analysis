# Load Libs ----------------------------------------------------------
library(tidyverse)
library(rvest)
library(RSelenium)


# Set up virtual browser --------------------------------------------------
rD <- rsDriver(browser="chrome", port=4549L, verbose=F, chromever = "95.0.4638.54")
remDr <- rD[["client"]]


# Download the zipped file of all pulse data for each week ----------------
for(i in 1:54){
  print(i)
  tryCatch({
    remDr$navigate(glue::glue("https://portal.census.gov/pulse/data/downloads/{i}/week{i}.zip"))
  }, error = function(e){
    message(glue::glue("Failed for week {i}"))
  })
}



# Previous attempt to use Selenium to click dropdown menu and download ----
# remDr$navigate("https://portal.census.gov/pulse/data/#downloads")
# 
# #Go to the "45" option in the weeks list (a list from 1 to 52 starting with 
# # 4/26/2020 to 5/02/2020 all the way to 10/11/2021 to 10/17/2021 )
# selector <- remDr$findElement(using = "xpath", 
#                   value = "//select[(@id = 'dl-week-selector')]/option[@value='45']")
# 
# # Click on the selected date range
# selector$clickElement()
# 
# remDr$findElement(using = "xpath",
#                   value = "//*[@id='download-tbody']/tr[486]/td[1]/a")$clickElement()

