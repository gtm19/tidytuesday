library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(scales)
library(usmap)
library(extrafont)

statemilk <-
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv") %>% 
  clean_names() %>% 
  mutate(decade = dmy(paste(1,1,year)),
         decade = floor_date(decade, "10 years"))

mapdata <- 
  usmap::us_map(regions = "states") 

plot_data <-
  statemilk %>% 
  group_by(state, decade)  %>% 
  summarise_at(vars(milk_produced), sum) %>% 
  group_by(decade) %>% 
  mutate(milk_prop = milk_produced/sum(milk_produced)) %>% 
  right_join(mapdata, by = c("state" = "full"))

p <-
  plot_data %>% 
  filter(!is.na(decade) & !state %in% c("Alaska", "Hawaii")) %>% 
  ggplot(aes(long, lat, group = group, fill = milk_prop)) +
  geom_polygon() +
  coord_fixed(ratio = 1.1) +
  facet_wrap(~factor(decade, labels = c("70s", "80s", "90s", "00s", "10s")), nrow = 2) +
  scale_fill_viridis_c(option = "E", labels = function(x) percent(x, accuracy = 1)) +
  theme_void(base_family = "Bahnschrift", base_size = 18) +
  theme(legend.position = c(0.85,0.27), plot.margin = unit(c(rep(0.5,4)), units = "cm"), plot.subtitle = element_text(margin = margin(t = 0, r = 0, b = 0.5, l = 0, unit = "cm"))) +
  labs(title = "Got Milk?", subtitle = "The states milking America through the years", fill = "Proportion of \nNational Production")

ggsave("plots/milk.png", p, dpi = 450)


