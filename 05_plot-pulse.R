# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(zoo)
library(patchwork)
library(ggtext)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Load Data ---------------------------------------------------------------
pulse <- read_csv(here("clean-data/pulse-clean.csv")) %>% 
  mutate(year_month = as.yearmon(year_month)) %>% 
  rename(neg_impact = mean_effect)

mslp <- read_csv(here("clean-data/time-sensitive-mslp.csv")) %>% 
  mutate(year_month = as.yearmon(year_month))


# Join Data ---------------------------------------------------------------
full_data <- mslp %>% 
  left_join(pulse)

# Create plot function ----------------------------------------------------
plot <- function(data,x, y){
  data %>% 
    ggplot(aes({{x}}, {{y}})) +
    geom_point() +
    geom_smooth(method = 'lm', se = F)
}

# Absolute negative impact --------------------------------------------
full_data %>% 
  plot(neg_impact, log(mean)) +
  labs(title = "Average loan size was bigger in state-months pairs where more businesses \nreported being negatively effected by Covid",
       x = "% of Businesses Negatively Affected by Covid",
       y = "Log mean loan size") +
  theme_bw() +
  theme(plot.title.position = "plot",
        plot.title = element_text(family = "Roboto Condensed",
                                  face = "bold", 
                                  size = rel(1.3)),
        plot.subtitle = element_text(family = "Roboto Condensed",
                                     face = "italic", 
                                     size = rel(1), 
                                     color = "grey40"),
        plot.caption = element_text(family = "Roboto Condensed",
                                    face = "italic", 
                                    size = rel(0.8), 
                                    color = "grey55"))

full_data %>% 
  plot(neg_impact, log(per_cap_loan)) +
  labs(title = "Total loan amount per capita was *smaller* in state-month pairs where businesses <br>reported being negatively effected by Covid",
       x = "% of Businesses Negatively Affected by Covid",
       y = "Log per capita total dollar value of loans") +
  theme_bw() +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(family = "Roboto Condensed",
                                  face = "bold", 
                                  size = rel(1.3)),
        plot.subtitle = element_text(family = "Roboto Condensed",
                                     face = "italic", 
                                     size = rel(1), 
                                     color = "grey40"),
        plot.caption = element_text(family = "Roboto Condensed",
                                    face = "italic", 
                                    size = rel(0.8), 
                                    color = "grey55"))


# Change in reported negative effect --------------------------------------

mean <- full_data %>% 
  plot(change_sentiment, log(mean)) +
labs(title = "Log of average loan size",
       x = "Percentage Point Change in Businesses \nNegatively Affected by Covid",
       y = NULL) +
  theme_bw() +
  theme(plot.title = element_text(family = "Roboto Condensed",
                                  face = "bold", 
                                  size = rel(1.3)),
        plot.subtitle = element_text(family = "Roboto Condensed",
                                     face = "italic", 
                                     size = rel(1), 
                                     color = "grey40"),
        plot.caption = element_text(family = "Roboto Condensed",
                                    face = "italic", 
                                    size = rel(0.8), 
                                    color = "grey55"))

per_cap <- full_data %>% 
  plot(change_sentiment, log(per_cap_loan)) +
  labs(title = "Log of per capita total value of loans",
       x = "Percentage Point Change in Businesses \nNegatively Affected by Covid",
       y = NULL) +
  theme_bw() +
  theme(plot.title = element_markdown(family = "Roboto Condensed",
                                      face = "bold", 
                                      size = rel(1.3)),
        plot.subtitle = element_text(family = "Roboto Condensed",
                                     face = "italic", 
                                     size = rel(1), 
                                     color = "grey40"),
        plot.caption = element_text(family = "Roboto Condensed",
                                    face = "italic", 
                                    size = rel(0.8), 
                                    color = "grey55"))

mean + per_cap +
  plot_annotation(title = "The degree of MSLP support is positively related to the month-\nover-month change in the proportion of businesses reporting \nnegative Covid impacts, by state",
                  #subtitle = "The original analysis used MSLP data up to November 7th, 2020. The updated analysis uses MSLP \ndata up to January 2021. Both analyses quantify labor market deterioration as the change in unemployment \nbetween January and September 2020",
                  caption = "Sources: BLS for Small Business Pulse Survey, Federal Reserve Board for loan amounts",
                  theme = theme(plot.title = element_text(family = "Roboto Condensed",
                                                          face = "bold", 
                                                          size = rel(1.6)),
                                plot.subtitle = element_text(family = "Roboto Condensed",
                                                             face = "italic", 
                                                             size = rel(1.2), 
                                                             color = "grey40"),
                                plot.caption = element_text(family = "Roboto Condensed",
                                                            face = "italic", 
                                                            size = rel(0.8), 
                                                            color = "grey55")))
ggsave(here("figures/pulse.png"), bg = "white", 
       dpi = 600, height = 3.75, width = 5*1.62, units = "in")
