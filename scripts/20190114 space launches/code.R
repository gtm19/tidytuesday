library(tidyverse)
library(lubridate)
library(extrafont)

launches <-
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv" %>% 
  read_csv()

plot_data <-
  launches %>% 
  mutate(country = fct_collapse(state_code,
                                "Russia/USSR" = c("RU", "SU"),
                                "United States" = "US",
                                "China" = "CN",
                                "France" = "F",
                                "Japan" = "J"),
         country = fct_lump(country, 5)) %>% 
  group_by(country, launch_year) %>% 
  count() 

plot_data %>% 
  ggplot(aes(launch_year, n)) + 
  geom_bar(aes(fill = country), stat = "identity") +
  scale_x_continuous(breaks = seq(1960, 2010, 10)) +
  theme(rect = element_rect(fill = "#0a1926"), 
        panel.grid = element_blank(), 
        text = element_text(colour = "white", 
                            family = "Tw Cen MT Condensed", size = 26), 
        panel.background = element_rect("#0a1926"), 
        legend.position = c(0.8,0.9), 
        legend.text = element_text(size = 16), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(colour = rgb(230,230,230, maxColorValue = 255), size = 16),
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 14)) +
  labs(title = "The Space Races",
       subtitle = "The countries demonstrating how humanity's reach still exceeds its grasp",
       y = "Number of Launches",
       x = "Launch Year",
       fill = "",
       caption = "Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-15") + scale_fill_brewer(palette = "RdBu")

ggsave(p, filename = "scripts/20190114 space launches/space_races.png", dpi = 300, width = 10, height = 7)
