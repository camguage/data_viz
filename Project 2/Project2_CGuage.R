## Data Visualization (GOVT16-QSS17) Spring 2020
## LAB 2
##
## Name: Cameron Guage
## Date: May 13 - May 20


library(tidyverse)
library(ggrepel)
library(lubridate)
covid_data <- read_csv("daily.csv")


covid_data %>%
  mutate(total_tests = positive + negative) %>%
  mutate(new_england = ifelse(state %in% c("CT", "ME", "MA", "NH", "RI", "VT"), 1, 0)) %>%
  mutate(mid_atlantic = ifelse(state %in% c("NJ", "NY", "PA"), 2, 0)) %>%
  mutate(east_north_central = ifelse(state %in% c("IL", "IN", "MI", "OH", "WI"), 3, 0)) %>%
  mutate(west_north_central = ifelse(state %in% c("IA", "KS", "MN", "MO", "NE", "ND", "SD"), 4, 0)) %>%
  mutate(south_atlantic = ifelse(state %in% c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV"), 5, 0)) %>%
  mutate(east_south_central = ifelse(state %in% c("AL", "KY", "MS", "TN"), 6, 0)) %>%
  mutate(west_south_central = ifelse(state %in% c("AR", "LA", "OK", "TX"), 7, 0)) %>%
  mutate(mountain = ifelse(state %in% c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"), 8, 0)) %>%
  mutate(pacific = ifelse(state %in% c("AK", "CA", "HI", "OR", "WA"), 9, 0)) %>%
  mutate(territories = ifelse(state %in% c("AS", "GU", "MP", "PR", "VI"), 10, 0)) %>%
  mutate(region = new_england + mid_atlantic + east_north_central + west_north_central + south_atlantic + east_south_central + west_south_central + mountain + pacific + territories) %>%
  mutate(region = factor(region, levels = c(4, 3, 1, 9, 8, 2, 7, 6, 5, 10), labels = c("West North Central", "East North Cental", "New England", "Pacific", "Mountain", "Mid-Atlantic", "West South Central", "East South Central", "South Atlantic", "US Territories"))) %>%
  filter(region != "US Territories") %>%
  mutate(state_labels = ifelse(date == 20200518, state, NA)) %>%
  mutate(date = ymd(date)) %>%
  ggplot(aes(x = date, y = total_tests, color = state, label = state_labels)) +
  stat_smooth(geom = "line", alpha = 0.8, se = FALSE) +
  geom_text_repel() +
  facet_wrap(~ region) +
  coord_cartesian(xlim = as.Date(c("2020-03-01", "2020-05-20"))) +
  labs(x = "Date", y = "Number of Tests", title = "Total COVID-19 Tests by US Region over Time", caption = "Data Source: Atlantic’s Covid Tracker Project (https://covidtracking.com/api)") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "serif"),
        plot.caption = element_text(color = "#717171"),
        axis.title = element_text(size = 12),
        strip.background = element_rect(fill = "#E8E8E8"))

## Facets ordered to show relative geographic locations of the regions on a map
## Using ggrepel package was an idea I found on the following website (https://blog.exploratory.io/ggrepel-when-things-get-too-crowded-ffefd845665f)
