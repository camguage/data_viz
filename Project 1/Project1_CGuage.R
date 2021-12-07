## Data Visualization (GOVT16-QSS17) Spring 2020
## LAB 1
##
## Name: Cameron Guage
## Data: May 5 - May 12

covid_data = read_csv("31117262.csv"); covid_data

covid_data %>%
  filter(COVIDTHREAT_d_W64 != "Refused" & F_IDEO != "Refused") %>%
  mutate(F_IDEO = factor(F_IDEO, levels = c("Very liberal", "Liberal", "Moderate", "Conservative", "Very conservative"))) %>%
  mutate(COVIDTHREAT_d_W64 = factor(COVIDTHREAT_d_W64, levels = c("Not a threat", "A minor threat", "A major threat"))) %>%
  rename(threat_to_financials = COVIDTHREAT_d_W64, ideology = F_IDEO, trump_opinion = POL1DT_W64) %>%
  mutate(trump_opinion = paste(trump_opinion, POL1DTSTR_W64, sep = ", ")) %>%
  filter(!str_detect(trump_opinion, "Refused")) %>%
  mutate(trump_opinion = factor(trump_opinion, levels = c("Approve, Very strongly", "Approve, Not so strongly", "Disapprove, Not so strongly", "Disapprove, Very strongly"), labels = c("Strongly approve", "Approve", "Disapprove", "Strongly disapprove"))) %>%
  group_by(ideology, threat_to_financials) %>%
  count(trump_opinion) %>%
  ungroup() %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = trump_opinion, y = prop, fill = ideology)) +
  geom_col(color = "black") +
  facet_grid(. ~ threat_to_financials) +
  scale_fill_manual(values = c("#001CD1", "#8898FF", "#FFFFFF", "#FF7B79", "#CB0300")) +
  labs(x = "Opinion on how Donald Trump is Handling his Job", y = "Proportion of Respondents", fill = "Political Ideology", title = "American Opinion of President Trump", subtitle = "By Personal Fianancial Threat Posed by COVID-19 and Political Ideology", caption = "Data Source: Pew Research Center for the People & the Press. Pew Research Center: American Trends Panel Wave 64, 2020 [Dataset]. Roper #31117262,\nVersion 2. Ipsos [producer]. Cornell University, Ithaca, NY: Roper Center for Public Opinion Research [distributor]. doi:10.25940/ROPER-31117262") +
  theme(plot.caption = element_text(size = 7, color = "#717171", hjust = 0, vjust = -5),
        plot.margin = margin(.5, .5, 1, .7, "cm"),
        axis.title.x = element_text(size = 12, vjust = -2),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1.15, hjust = 1.1, color = "black"),
        axis.title.y = element_text(size = 12, vjust = 4),
        text = element_text(family = "serif"),
        panel.spacing = unit(1, "cm"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(color = "#717171", size = 0.2),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12))


## paste function on line 15 used to combine the two columns on opinion of Donald Trump and strength of opinion on Donald Trump
## idea inspired by: https://stackoverflow.com/questions/50845474/concatenating-two-text-columns-in-dplyr

## !str_detect removes any string that contains "Refused" from this column, which was especially useful because many different strings in this new column contained "Refused".
## idea inspired by: https://stackoverflow.com/questions/22249702/delete-rows-containing-specific-strings-in-r

