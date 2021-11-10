# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(here)
library(zoo)
library(patchwork)

#Set font
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))


# Load Data ---------------------------------------------------------------
data <- read_csv(here("clean-data/time-sensitive-mslp.csv")) %>% 
  mutate(year_month = as.yearmon(year_month))


# Create plot function ----------------------------------------------------
plot <- function(data,x, y){
  data %>% 
    ggplot(aes({{x}}, {{y}})) +
    geom_point() +
    geom_smooth(method = 'lm', se = F)
}

# Original Method --------------------------------------------------------
# Look at change since Jan 2020 for each state-month pair- plagued by confounders
a <- data %>% 
  plot(unemp_change, log(per_cap_loan)) +
  labs(x = "Percentage point change in unemployment",
       y = "Log of total loan amount \nper person") +
  theme_bw(base_size = 10, base_family = "roboto") 

b <- data %>% 
  plot(unemp_change, log(mean)) +
  labs(x = "Percentage point change in unemployment",
       y = "Log of average borrower \nloan amount") +
  theme_bw(base_size = 10, base_family = "roboto") 

c <- data %>% 
  plot(epop_change, log(per_cap_loan)) +
  labs(x = "Percentage point change in EPOP",
       y = "Log of total loan amount \nper person") +
  theme_bw(base_size = 10, base_family = "roboto") 

d <- data %>% 
  plot(epop_change, log(mean))  +
  labs(x = "Percentage point change in EPOP",
       y = "Log of average borrower \nloan amount") +
  theme_bw(base_size = 10, base_family = "roboto") 

windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

a + b + c + d +
  plot_annotation(title = "There is no clear relationship between the loan support flowing to a state in \na given month and the labor market conditions in that state during that month",
                  subtitle = "For the per capita loan values, the relationship is actually reversed: harder hit states got less support. \nThis could be due to the confounding variable of trends over time.",
                  caption = "Sources: BLS for unemployment, Federal Reserve Board for loan amounts",
                  theme = theme(plot.title = element_text(family = "Roboto Condensed",
                                                          face = "bold", 
                                                          size = rel(1.3)),
                                plot.subtitle = element_text(family = "Roboto Condensed",
                                                             face = "italic", 
                                                             size = rel(1), 
                                                             color = "grey40"),
                                plot.caption = element_text(family = "Roboto Condensed",
                                                            face = "italic", 
                                                            size = rel(0.8), 
                                                            color = "grey55")))

# Diff in Diff Method -----------------------------------------------------
a <- data %>% 
  plot(unemp_diff_in_diff, log(per_cap_loan)) +
  labs(x = "Percentage point change in unemployment",
       y = "Log of total loan amount \nper person") +
  theme_bw(base_size = 10, base_family = "roboto") 

b <- data %>% 
  plot(unemp_diff_in_diff, log(mean)) +
  labs(x = "Percentage point change in unemployment",
       y = "Log of average borrower \nloan amount") +
  theme_bw(base_size = 10, base_family = "roboto") 

c <- data %>% 
  plot(epop_diff_in_diff, log(per_cap_loan)) +
  labs(x = "Percentage point change in EPOP",
       y = "Log of total loan amount \nper person") +
  theme_bw(base_size = 10, base_family = "roboto") 

d <- data %>% 
  plot(epop_diff_in_diff, log(mean))  +
  labs(x = "Percentage point change in EPOP",
       y = "Log of average borrower \nloan amount") +
  theme_bw(base_size = 10, base_family = "roboto") 

windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

a + b + c + d +
  plot_annotation(title = "The relationship between the loan support and labor market conditions depends \non the measure used for loan support; per capita versus average",
                  subtitle = "For the per capita loan values, the relationship is actually reversed: harder hit states got less support",
                  caption = "Sources: BLS for unemployment, Federal Reserve Board for loan amounts",
                  theme = theme(plot.title = element_text(family = "Roboto Condensed",
                                                          face = "bold", 
                                                          size = rel(1.3)),
                                plot.subtitle = element_text(family = "Roboto Condensed",
                                                             face = "italic", 
                                                             size = rel(1), 
                                                             color = "grey40"),
                                plot.caption = element_text(family = "Roboto Condensed",
                                                            face = "italic", 
                                                            size = rel(0.8), 
                                                            color = "grey55")))

# Monthly change method -----------------------------------------------------
a <- data %>% 
  plot(unemp_month_change, log(per_cap_loan)) +
  labs(x = "Percentage point change in unemployment",
       y = "Log of total loan amount \nper person") +
  theme_bw(base_size = 10, base_family = "roboto") 

b <- data %>% 
  plot(unemp_month_change, log(mean)) +
  labs(x = "Percentage point change in unemployment",
       y = "Log of average borrower \nloan amount") +
  theme_bw(base_size = 10, base_family = "roboto") 

c <- data %>% 
  plot(epop_month_change, log(per_cap_loan)) +
  labs(x = "Percentage point change in EPOP",
       y = "Log of total loan amount \nper person") +
  theme_bw(base_size = 10, base_family = "roboto") 

d <- data %>% 
  plot(epop_month_change, log(mean))  +
  labs(x = "Percentage point change in EPOP",
       y = "Log of average borrower \nloan amount") +
  theme_bw(base_size = 10, base_family = "roboto") 

a + b + c + d +
  plot_annotation(title = "There is no clear relationship between the loan support flowing to a state in a given month and the labor market conditions in that state during that month",
                  subtitle = "For the per capita loan values, the relationship is actually reversed: harder hit states got less support",
                  caption = "Sources: BLS for unemployment, Federal Reserve Board for loan amounts",
                  theme = theme(plot.title = element_text(family = "Roboto Condensed",
                                                          face = "bold", 
                                                          size = rel(1.3)),
                                plot.subtitle = element_text(family = "Roboto Condensed",
                                                             face = "italic", 
                                                             size = rel(1), 
                                                             color = "grey40"),
                                plot.caption = element_text(family = "Roboto Condensed",
                                                            face = "italic", 
                                                            size = rel(0.8), 
                                                            color = "grey55")))

