library(tidyverse)
library(schmidtytheme)
extrafont::loadfonts()

theme_set(theme_schmidt()+
            theme(
              text = element_text(family="Public Sans")
            ))

tuesdata <- tidytuesdayR::tt_load(2021, week = 2)
transit_cost <- tuesdata$transit_cost

transit_cost %>%
  count(country, sort = T)

transit_cost %>%
  group_by(country, year) %>%
  summarise(real_cost = sum(as.numeric(real_cost), na.rm = T))%>%
  ungroup()%>%
  ggplot(aes(year, real_cost, color = country))+
  geom_line()

transit_cost%>%
  group_by(country)%>%
  summarize(median_cost_per_km_millions = median(cost_km_millions))%>%
  ungroup()%>%
  filter(!is.na(country))%>%
  mutate(country = fct_reorder(country, median_cost_per_km_millions))%>%
  ggplot(aes(country, median_cost_per_km_millions))+
  geom_point()+
  coord_flip()
