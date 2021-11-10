# Clean Most Up to Date Data ----------------------------------------------
# This function corrects for the fact that for some rows, the Excel data date
# col is in the form of days since 1899-12-31
date_correction <- function(days){
  if(!is.na(days)){
    seq.Date(from = as.Date("1900-01-01"), 
             length.out = as.numeric(days) - 1, 
             by = "day") %>% 
      tail(1)
  } else {
    NA
  }
}

# Fix date column
mslp <- mslp_raw %>% 
  # The issue is that read_excel puts some rows for the date column into
  # "days since 1899-12-31" and other rows into mdy form. I extract the 
  # former and put them as date1, and the latter I extract to form date2
  mutate(date1 = ifelse(substr(date_of_loan_origination,1,2) == "44", 
                        date_of_loan_origination, 
                        NA),
         date2 = ifelse(substr(date_of_loan_origination,1,2) != "44", 
                        date_of_loan_origination, 
                        NA)) %>% 
  #For the "days since 1899-12-31", I run the date correction function
  mutate(date1 = map(date1, date_correction)) %>% 
  unnest(date1) %>% 
  # For the mdy formatted dates, I simply use the mdy() function
  mutate(date2 = lubridate::mdy(date2)) %>% 
  # I put these both into character form to avoid odd behavior (like
  # R converting to days since 1970-01-01)
  mutate(date2=as.character(date2),
         date1 = as.character(date1)) %>% 
  # To make the final date column, I say use date1 if date1 is isn't missing, and I use date2
  # if date1 is missing
  mutate(date = ifelse(is.na(date1),date2,date1),
         date = as.Date(date))

# Clean State names
mslp <- mslp %>% 
  mutate(# Make borrower state upper case
    borrower_state_clean = str_to_upper(borrower_state),
    # Remove any numbers 
    borrower_state_clean = str_remove_all(borrower_state_clean,'[0-9]+'),
    # Remove periods
    borrower_state_clean = str_remove_all(borrower_state_clean,c("\\.")),
    # Remove spaces
    borrower_state_clean = str_remove_all(borrower_state_clean," "),
    # Only grab the first two letters, which is the state abbreviation
    borrower_state_clean = substr(borrower_state_clean, 1, 2),
    # Correct abbreviation for US Virgin Islands
    borrower_state_clean = ifelse(borrower_state_clean == "US", 
                                  "USVI", 
                                  borrower_state_clean)) %>% 
  # Grab just the columns of interest
  select(date, 
         facility, 
         borrower_state_clean, 
         principal_amount_of_loan_when_purchased_by_mslp)

# Exclude the non-profit program, as was done in the paper
mslp <- mslp %>% 
  filter(facility !="NONLF")


# Clean Original Data They Used -------------------------------------------

# Fix date column
mslp_org <- mslp_org_raw %>% 
  # The issue is that read_excel puts some rows for the date column into
  # "days since 1899-12-31" and other rows into mdy form. I extract the 
  # former and put them as date1, and the latter I extract to form date2
  mutate(date1 = ifelse(substr(date_of_loan_origination,1,2) == "44", 
                        date_of_loan_origination, 
                        NA),
         date2 = ifelse(substr(date_of_loan_origination,1,2) != "44", 
                        date_of_loan_origination, 
                        NA)) %>% 
  #For the "days since 1899-12-31", I create and run a function to figure out
  # what date that comes out to
  mutate(date1 = map(date1, date_correction)) %>% 
  unnest(date1) %>% 
  # For mdy, I simply use the mdy() function
  mutate(date2 = mdy(date2)) %>% 
  # I put these both into character form to avoid odd behavior (like
  # R converting to days since 1970-01-01)
  mutate(date2=as.character(date2),
         date1 = as.character(date1)) %>% 
  # To make the final date column, I say use date1 if is isn't missing, and use date2
  # if date1 is missing
  mutate(date = ifelse(is.na(date1),date2,date1),
         date = as.Date(date))

# Clean state names
mslp_org <- mslp_org %>% 
  mutate(# Make borrower state upper case
    borrower_state_clean = str_to_upper(borrower_state),
    # Remove any numbers 
    borrower_state_clean = str_remove_all(borrower_state_clean,'[0-9]+'),
    # Remove periods
    borrower_state_clean = str_remove_all(borrower_state_clean,c("\\.")),
    # Remove spaces
    borrower_state_clean = str_remove_all(borrower_state_clean," "),
    # Only grab the first two letters, which is the state abbreviation
    borrower_state_clean = substr(borrower_state_clean, 1, 2),
    # Correct abbreviation for US Virgin Islands
    borrower_state_clean = ifelse(borrower_state_clean == "US", 
                                  "USVI", 
                                  borrower_state_clean)) %>% 
  # Grab just the columns of interest
  select(date, 
         facility, 
         borrower_state_clean, 
         principal_amount_of_loan_when_purchased_by_mslp)

# Exclude the non-profit program, as was done in the paper
mslp_org <- mslp_org %>% 
  filter(facility !="NONLF")