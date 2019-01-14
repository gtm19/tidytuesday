library(tidyverse)
library(lubridate)
library(extrafont)

launches <-
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv" %>% 
  read_csv() %>% 
  left_join("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/agencies.csv" %>% 
              read_csv() %>% 
              distinct(agency, short_name, name))

launches %>% 
  count(agency_type, launch_year) %>% 
  ggplot(aes(launch_year, n, fill = str_to_title(agency_type))) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1960, 2010, 10)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_d() +
  theme(rect = element_rect(fill = "#0a1926"), 
        panel.grid = element_blank(), 
        text = element_text(colour = "white", 
                            family = "Barlow Condensed", size = 26), 
        panel.background = element_rect("#0a1926"), legend.position = "bottom") +
  labs(title = "The Rise of Private Space Agencies",
       subtitle = "",
       y = "Number of Launches",
       x = "Launch Year",
       fill = "Agency Type")
