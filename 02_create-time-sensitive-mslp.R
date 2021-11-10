# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(lubridate)
library(zoo)
library(readxl)
library(janitor)

# Load Data ---------------------------------------------------------------
mslp <- read_csv(here("clean-data/mslp.csv"))
labor <- read_csv(here("raw-data/state-labor-markets.csv")) 


# Clean MSLP --------------------------------------------------------------
mslp <- mslp %>% 
  select(date,
         abb = borrower_state_clean, 
         loan_amount = principal_amount_of_loan_when_purchased_by_mslp) %>% 
  mutate(year = year(date),
         month = month(date))

state_to_abb <- tibble(abb = state.abb) %>% 
  mutate(state = state.name)


# Aggregate to state-month level ------------------------------------------
mslp_agg <- mslp %>% 
  group_by(abb, year, month) %>% 
  summarise(total = sum(loan_amount),
            mean = mean(loan_amount),
            num_loans = n()) %>% 
  ungroup() %>% 
  mutate(year_month = as.yearmon(glue::glue("{year}-{month}"))) %>% 
  left_join(state_to_abb)

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
mslp_agg <- mslp_agg %>% 
  left_join(state_pop) %>% 
  mutate(per_cap_loan = total/population,
         per_cap_num_loans = num_loans/population*100000)

# Initialize Labor Market Data --------------------------------------------
# Turn mmm abbreviation to month number
labor$month <- match(labor$period,month.abb)


#Turn columns to proper types and make date column
labor <- labor %>% 
  mutate_at(vars(year, month, labor_force_participation_rate:unemployment_rate),
            .funs = as.numeric) %>% 
  mutate(date = as.Date(glue::glue("{year}-{month}-01")))


# Make change since Jan 2020 cols -----------------------------------------
# Get the unemployment and EPOP values for Jan 2020
init_vals <- labor %>% 
  filter(year == 2020, month == 1) %>% 
  select(state, 
         epop_jan_2020=employment_population_ratio,
         unemp_jan_2020=unemployment_rate)

#For each state-month pair, subtract from the current epop/unemp the Jan 2020 value
# This gives us change in labor market conditions for each state-month pair compared to Jan 2020 baseline
labor_change <- labor %>% 
  select(state, year, month, 
         epop=employment_population_ratio,
         unemp=unemployment_rate) %>% 
  left_join(init_vals) %>% 
  mutate(epop_change = epop - epop_jan_2020,
         unemp_change = unemp - unemp_jan_2020)  


# Create difference compared to national value cols -----------------------
# Create columns for the difference between national unemployment/epop and the given state's
# value in the given month. Should correct for time trend.
labor <- labor %>% 
  select(state, year, month, date, employment_population_ratio, unemployment_rate) %>% 
  group_by(year, month) %>% 
  mutate(nat_unemp = mean(unemployment_rate),
         nat_epop = mean(employment_population_ratio)) %>% 
  arrange(year, month) %>% 
  ungroup() %>% 
  mutate(unemp_diff = unemployment_rate - nat_unemp,
         epop_diff = employment_population_ratio - nat_epop)


# Create diff in diff cols ------------------------------------------------
# This column says: What is the difference between the *current* difference between
# national vs state-level unemployment/epop in the given state and what the difference
# was in 2019. 

#This controls for the trend in overall labor market improvement over time, as well as 
# state level fixed effects in labor market conditions (ND has lower unemployment than the national
# level normally, for example). 

# Here, I just am getting the "normal" difference between state-level and national 
# labor market indicators, for each state, using 2019 as the proxy 
norm_diff <- labor %>% 
  filter(year(date) == 2019) %>% 
  group_by(year, month) %>%
  mutate(nat_unemp = mean(unemployment_rate),
         nat_epop = mean(employment_population_ratio)) %>% 
  ungroup() %>% 
  mutate(unemp_diff = unemployment_rate - nat_unemp,
         epop_diff = employment_population_ratio - nat_epop) %>% 
  select(state, date, unemp_diff, epop_diff) %>% 
  group_by(state) %>% 
  summarise(normal_unemp_diff = mean(unemp_diff),
            normal_epop_diff = mean(epop_diff)) %>% 
  ungroup()

# Join the "normal" difference in, and, for each state-month pair, find the difference
# between "normal" difference and the given state-month difference
diff_in_diff <- labor %>% 
  filter(year(date) > 2019) %>%
  select(state, year, month, unemp_diff, epop_diff) %>% 
  left_join(norm_diff) %>% 
  mutate(unemp_diff_in_diff = unemp_diff - normal_unemp_diff,
         epop_diff_in_diff = epop_diff - normal_epop_diff) %>% 
  select(state, year, month, unemp_diff_in_diff, epop_diff_in_diff)


# Monthly change column ---------------------------------------------------
month_change <- labor %>% 
  filter(year >= 2020) %>% 
  mutate(year_month = as.yearmon(glue::glue("{year}-{month}"))) %>% 
  select(state, year, month, year_month,
         epop=employment_population_ratio,
         unemp=unemployment_rate) %>% 
  arrange(state) %>% 
  group_by(state, year) %>% 
  mutate(unemp_month_change = unemp - lag(unemp,n = 1),
         epop_month_change = epop - lag(epop,n = 1)) %>% 
  ungroup() %>% 
  select(state, year, month, unemp_month_change, epop_month_change)


# Join state-month labor market changes to MSLP ---------------------------
full_data <- mslp_agg %>% 
  left_join(labor_change) %>% 
  left_join(labor %>% 
              select(state, year, month, unemp_diff, epop_diff)) %>% 
  left_join(diff_in_diff) %>% 
  left_join(month_change)



# Save Data ---------------------------------------------------------------
write_csv(full_data, here("clean-data/time-sensitive-mslp.csv"))

