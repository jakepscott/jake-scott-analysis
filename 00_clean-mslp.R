# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(lubridate)
library(zoo)

# Load and clean MSLP data ----------------------------------------------------------
# Load most recent raw data
mslp_raw <- read_xlsx(here("raw-data", "mslp-transaction-specific-disclosures-04-12-21_MSNLF.xlsx")) %>% 
  clean_names()

#Load the December 11th, 2020 version used by the authors
mslp_org_raw <- read_xlsx(here("raw-data", "mslp-nov-2020.xlsx")) %>% 
  clean_names()

#Clean the data (creates mslp and mslp_org)
source(here("helper-scripts","clean-mslp-data.r"))

# Save the data
write_csv(mslp, here("clean-data/mslp.csv"))
write_csv(mslp_org, here("clean-data/mslp_org.csv"))

# Make Aggregated Data ----------------------------------------------------
# Aggregated original data
mslp_org_agg <- mslp_org %>% 
  group_by(borrower_state_clean) %>% 
  summarise(median_loan = median(principal_amount_of_loan_when_purchased_by_mslp),
            average_loan = mean(principal_amount_of_loan_when_purchased_by_mslp),
            total_loan = sum(principal_amount_of_loan_when_purchased_by_mslp),
            num_loans = n()) %>% 
  mutate(data = "old_data")

#Aggregated new data
mslp_agg <- mslp %>% 
  group_by(borrower_state_clean) %>% 
  summarise(median_loan = median(principal_amount_of_loan_when_purchased_by_mslp),
            average_loan = mean(principal_amount_of_loan_when_purchased_by_mslp),
            total_loan = sum(principal_amount_of_loan_when_purchased_by_mslp),
            num_loans = n()) %>% 
  mutate(data = "new_data")

#Bind the old and new data
mslp_agg_full <- mslp_org_agg %>% 
  bind_rows(mslp_agg) %>% 
  rename(abb = borrower_state_clean)

# Join in state full names ------------------------------------------------
# Create a table matching state abbreviations with state names
# This allows me to match all 3 of my data sets
state_to_abb <- tibble(abb = state.abb) %>% 
  mutate(state = state.name)

#Join full state names
mslp_agg_full <- mslp_agg_full %>% 
  left_join(state_to_abb) %>% 
  mutate(state = case_when(abb == "DC"~"District of Columbia",
                           abb == "PR" ~ "Puerto Rico",
                           TRUE ~ state)) %>% 
  relocate(state, .before = everything())

# Make per cap col --------------------------------------------------
# Load State Population Data 
state_pop_raw <- read_excel(here("raw-data/state-population.xlsx"), skip = 3) 

#Clean state population data
state_pop <- state_pop_raw %>% 
  #Remove non-state rows
  tail(-5) %>% 
  head(-5) %>%
  rename(state = `...1`) %>% 
  # Put into long form
  pivot_longer(cols = -(state:`Estimates Base`), 
               names_to = "year",
               values_to = "population") %>% 
  #Clean col names
  clean_names() %>%
  # Remove period from state names
  mutate(state = str_remove_all(state,"\\.")) %>% 
  #Focus on 2019 pops
  filter(year == 2019) %>% 
  select(state, population)

# Join state populations and use them to make a per capita column
mslp_agg_full <- mslp_agg_full %>% 
  left_join(state_pop) %>% 
  mutate(per_cap_loan = total_loan/population,
         per_cap_num_loans = num_loans/population*100000)


# Load Labor Market Data --------------------------------------------------
labor <- read_csv(here("raw-data/state-labor-markets.csv"))

# Turn mmm abbreviation to month number
labor$month <- match(labor$period,month.abb)


#Turn columns to proper types and make date column
labor <- labor %>% 
  mutate_at(vars(year, month, labor_force_participation_rate:unemployment_rate),
            .funs = as.numeric) %>% 
  mutate(date = as.Date(glue::glue("{year}-{month}-01"))) %>% 
  filter(year(date) >= 2020)

# Create change in unemployment data -------------------------------------
unemp <- labor %>% 
  relocate(date, .before = everything()) %>% 
  # Keep Jan 2020 (baseline), Sep 2020 (original paper range)
  # and Jan 2021 (most recent data)
  filter(date %in% c(as.Date("2020-01-01"), 
                     as.Date("2020-09-01"), 
                     as.Date("2021-01-01"))) %>%  
  #Keep only needed columns
  select(date, state, unemployment_rate) %>% 
  # Put in wide format to subtract current from baseline unemp
  pivot_wider(names_from = date, values_from = unemployment_rate) %>% 
  mutate(old_data = `2020-09-01` - `2020-01-01`,
         new_data = `2021-01-01` - `2020-01-01`) %>% 
  select(state, contains("data")) %>% 
  # Put back in wide format, where data == old_data means 
  # Sep 2020 - Jan 2020 and new_data means Jan 2021 - Jan 2020
  pivot_longer(-state, 
               names_to = "data",
               values_to = "unemp_change")

write_csv(unemp,here("clean-data/unemployment-change.csv"))


# Create change in epop data ----------------------------------------------
epop_change <- labor %>% 
  relocate(date, .before = everything()) %>% 
  # Grab just Jan 2021 and Jan 2020
  filter(date %in% c(as.Date("2020-01-01"), 
                     as.Date("2021-01-01"))) %>%  
  select(date, state, employment_population_ratio) %>% 
  # Put in wide form to make change column
  pivot_wider(names_from = date, values_from = employment_population_ratio) %>% 
  mutate(epop_change = `2021-01-01` - `2020-01-01`) %>% 
  select(state, epop_change)


# Join unemployment and epop change ---------------------------------------
full_data <- mslp_agg_full %>% 
  left_join(unemp) %>% 
  left_join(epop_change)


# Save Full Data (not-time-sensitive) -------------------------------------
write_csv(full_data,here("clean-data/mslp_agg.csv"))
