# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(zoo)

# Load Data ---------------------------------------------------------------
pulse <- read_csv(here("raw-data/pulse-state-data.csv")) %>% 
  clean_names()


# Clean Data --------------------------------------------------------------

# Get just the question of interest (asked in two different wordings over time)
q_of_int <- pulse %>% 
  clean_names() %>% 
  distinct(question) %>% 
  mutate(n = row_number()) %>% 
  filter(n %in% c(1,21))  

business_sentiment <- pulse %>%
  # Join the q of int, so now the question I want will have n = 1 or n = 21, 
  # so filter just that
  left_join(q_of_int) %>% 
  # Just look at Covid effect question (split into two)
  filter(n %in% c(1,21)) %>% 
  # Grab just cols of int
  select(st, question, date, answer_text, estimate_percentage) %>% 
  # Unify the two diff wordings of Q
  mutate(question = "How has Covid affected your business") %>% 
  # Turn the 26Apr20_02May20 date format into actual date (using end of survey)
  separate(date, into = c("start","end"), sep = "_") %>% 
  mutate(date = dmy(end)) %>% 
  select(st, question, date, answer_text, estimate_percentage) %>% 
  # Look just at % of businesses saying negative effect in given state in given week
  filter(answer_text %in% c("Large negative effect", "Moderate negative effect")) %>% 
  # Make month and year cols and pull number from percent col
  mutate(year = year(date),
         month = month(date),
         neg_effect = parse_number(estimate_percentage)) %>%
  select(-estimate_percentage) %>% 
  # Add the two possible answers saying negative effect
  pivot_wider(names_from = answer_text, values_from = neg_effect) %>% 
  mutate(neg_effect = `Large negative effect` + `Moderate negative effect`) %>% 
  select( -`Large negative effect`, -`Moderate negative effect`) %>% 
  # Get the state-month pair for percent of businesses reporting negative covid effects
  group_by(st, year, month) %>% 
  filter(st != "-") %>% 
  summarise(mean_effect = mean(neg_effect)) %>% 
  ungroup() %>% 
  select(abb = st,
         year, 
         month,
         mean_effect) %>% 
  mutate(year_month = as.yearmon(glue::glue('{year}-{month}'))) %>% 
  #Get monthly change in sentiment
  group_by(abb, year) %>% 
  mutate(change_sentiment = mean_effect - lag(mean_effect,1)) %>% 
  ungroup()



# business_sentiment %>% 
#   ggplot(aes(year_month, mean_effect, color = abb)) +
#   geom_line(show.legend = F)


# Save data ---------------------------------------------------------------
write_csv(business_sentiment, here("clean-data/pulse-clean.csv"))
