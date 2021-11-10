# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(showtext)

#Add font
# font_add_google("Roboto", "roboto")
# ## Automatically use showtext to render text for future devices
# showtext_auto()
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Load Data ---------------------------------------------------------------
full_data_raw <- read_csv(here("clean-data/mslp_agg.csv"))
unemp <- read_csv(here("clean-data/unemployment-change.csv"))

# Fix Old vs New Factor Col -----------------------------------------------
full_data <- full_data_raw %>% 
  mutate(data = ifelse(data == "new_data", "MSLP data to Jan. 2021", "MSLP data to Nov. 2020"), 
         data = factor(data,levels = c( "MSLP data to Nov. 2020", "MSLP data to Jan. 2021")))

# Use unemp to Sep 2020, but full loan data -------------------------------
# Combine updated MSLP data with Jan to Sep 2020 change in unemp
updated_mslp_old_unemp <- full_data %>% 
  filter(data == "MSLP data to Jan. 2021") %>% 
  select(-unemp_change) %>% 
  left_join(unemp %>% 
              filter(data == "old_data") %>% 
              select(-data)) 

# Compare new vs old avg loan vs unemp ------------------------------------
average <- full_data %>% 
  ggplot(aes(unemp_change, 
             log(average_loan))) +
  geom_smooth(method = "lm", se = F) +
  geom_text(aes(label = abb)) +
  labs(x = "Percentage point change in unemployment\n from January to September 2020",
       y = NULL,
       title = "Log of average borrower loan amount") +
  facet_wrap(~data) +
  theme_bw(base_size = 12, 
           base_family = "Roboto Condensed") +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(size = rel(1.3)),
        plot.subtitle = element_text(face = "italic", size = rel(1), color = "grey40"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey55"),
        axis.text = element_text(siz=rel(1)),
        legend.position = "none")

# Compare new vs old per capita loan vs unemp ------------------------------------
per_cap <- full_data %>% 
  ggplot(aes(unemp_change, log(per_cap_loan))) +
  geom_smooth(method = "lm", se = F) +
  geom_text(aes(label = abb)) +
  labs(x = "Percentage point change in unemployment\n from January to September 2020",
       y = NULL,
       title = "Log of total loan amount per person") +
  facet_wrap(~data) +
  theme_bw(base_size = 12, 
           base_family = "Roboto Condensed") +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(size = rel(1.3)),
        plot.subtitle = element_text(face = "italic", size = rel(1), color = "grey40"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey55"),
        axis.text = element_text(siz=rel(1)),
        legend.position = "none")


library(patchwork)

average + per_cap +
  plot_layout(ncol = 1) +
  plot_annotation(title = "States with larger labor market deteriorations appear to have received marginally more \nsupport from the Main Street Lending Program",
                  #subtitle = "The original analysis used MSLP data up to November 7th, 2020. The updated analysis uses MSLP \ndata up to January 2021. Both analyses quantify labor market deterioration as the change in unemployment \nbetween January and September 2020",
                  caption = "Sources: BLS for unemployment, Federal Reserve Board for loan amounts",
                  theme = theme(plot.title = element_text(family = "Roboto Condensed",
                                                          face = "bold", 
                                                          size = rel(1.45)),
                                plot.subtitle = element_text(family = "Roboto Condensed",
                                                             face = "italic", 
                                                             size = rel(1.2), 
                                                             color = "grey40"),
                                plot.caption = element_text(family = "Roboto Condensed",
                                                            face = "italic", 
                                                            size = rel(0.8), 
                                                            color = "grey55")))
ggsave(here("figures/org_vs_updated.png"), bg = "white", 
       dpi = 600, height = 6.5, width = 5*1.62, units = "in")


# Compare reg lines -------------------------------------------------------
# # Takeaway: The relationship is basically the same for the updated and original data
# full_data %>% 
#   ggplot(aes(unemp_change, log(average_loan), color = data)) +
#   geom_smooth(method = "lm")
# 
# # Of course, there is an upwards intercept shift for total loans,
# # since the time span is longer
# full_data %>% 
#   ggplot(aes(unemp_change, log(per_cap_loan), color = data)) +
#   geom_smooth(method = "lm")
# 
# 
# # Loans versus EPOP -------------------------------------------------------
# #Imperfect relationship between unemployment and EPOP
# unemp %>% 
#   filter(data == "new_data") %>% 
#   left_join(epop_change) %>% 
#   ggplot(aes(unemp_change, epop_change)) +
#   geom_text(aes(label = state))
# 
# # Using EPOP
# # Takeaway: Relationship appears to attenuate a bit, but is still marginally positive
# 
# #Average
# full_data %>% 
#   filter(data == "MSLP data to Jan. 2021") %>% 
#   ggplot(aes(epop_change, log(average_loan))) +
#   geom_smooth(method = "lm", se = F) +
#   geom_text(aes(label = abb))
# 
# # Per capita
# full_data %>% 
#   filter(data == "MSLP data to Jan. 2021") %>% 
#   ggplot(aes(epop_change, log(per_cap_loan))) +
#   geom_smooth(method = "lm", se = F) +
#   geom_text(aes(label = abb))
