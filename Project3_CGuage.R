## Data Visualization (GOVT16-QSS17) Spring 2020
## Project 3
##
## Name: Cameron Guage
## Date: May 22 - May 29, 2020

library(ggplot2)
library(tidyverse)
library(gridExtra)


covid_deaths <- read_csv("time_series_covid19_deaths_global.csv"); covid_deaths

##  Make dates into columns
covid_deaths_to_merge <- covid_deaths %>%
  gather(key = "date", value = "deaths", `1/22/20`:`5/28/20`) %>%
  rename(region = `Country/Region`) %>%
  mutate(region = str_replace_all(region, "US", "USA")) %>%  # for consistency with map names
  mutate(region = str_replace_all(region, "United Kingdom", "UK"))  # for consistency with map names

## Plot 1
covid_deaths_to_merge_1 <- covid_deaths_to_merge %>%
  filter(date == "3/15/20")

world_map <- map_data("world")

## Merge together
merged_covid_deaths_1 <- left_join(world_map, covid_deaths_to_merge_1, by = "region")

## Save plot for later
march_15 <- ggplot(merged_covid_deaths_1, aes(x = long, y = lat, group = group)) +
geom_polygon(aes(fill = deaths), color = "lightgrey", size = .1) +
labs(x = "Longitude", y = "Latidtude", fill = "Number of deaths", title = "COVID-19 Hotspots over Time", subtitle = "Date: March 15") +
scale_fill_gradientn(colors = c("#FCFF8F", "#FB7E00", "#5A0EA1")) +
theme(panel.background = element_blank(),
      panel.grid = element_line(color = "lightgrey"))


## Plot 2
covid_deaths_to_merge_2 <- covid_deaths_to_merge %>%
  filter(date == "4/1/20")

## Merge together
merged_covid_deaths_2 <- left_join(world_map, covid_deaths_to_merge_2, by = "region")


## Save plot for later
april_1 <- ggplot(merged_covid_deaths_2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = deaths), color = "lightgrey", size = .1) +
  labs(x = "Longitude", y = "Latidtude", fill = "Number of deaths", subtitle = "Date: April 1") +
  scale_fill_gradientn(colors = c("#FCFF8F", "#FB7E00", "#5A0EA1")) +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = "lightgrey"))

## Plot 3
covid_deaths_to_merge_3 <- covid_deaths_to_merge %>%
  filter(date == "4/15/20")

## Merge together
merged_covid_deaths_3 <- left_join(world_map, covid_deaths_to_merge_3, by = "region")

## Save plot for later
april_15 <- ggplot(merged_covid_deaths_3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = deaths), color = "lightgrey", size = .1) +
  labs(x = "Longitude", y = "Latidtude", fill = "Number of deaths", subtitle = "Date: April 15") +
  scale_fill_gradientn(colors = c("#FCFF8F", "#FB7E00", "#5A0EA1")) +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = "lightgrey"))


## Plot 4
covid_deaths_to_merge_4 <- covid_deaths_to_merge %>%
  filter(date == "5/1/20")

## Merge data
merged_covid_deaths_4 <- left_join(world_map, covid_deaths_to_merge_4, by = "region")  

## Save plot for later
may_1 <- ggplot(merged_covid_deaths_3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = deaths), color = "lightgrey", size = .1) +
  labs(x = "Longitude", y = "Latidtude", fill = "Number of deaths", subtitle = "Date: May 1", caption = "Data Source: https://github.com/CSSEGISandData\n/COVID-19/blob/master/csse_covid_19_data/csse_\ncovid_19_time_series/time_series_covid19_deaths\n_global.csv") +
  scale_fill_gradientn(colors = c("#FCFF8F", "#FB7E00", "#5A0EA1")) +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = "lightgrey"),
        plot.caption = element_text(color = "grey", size = 7))


library(grid)
library(ggplotify)
library(cowplot)

## Plot them all together
plot_grid(march_15, april_1, april_15, may_1)

  