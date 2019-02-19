library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(scales)
library(here)
library(extrafont)

phd_field <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv") %>% 
  remove_missing()

var <- expr(major_field)

phd_field %>% 
  mutate_at(vars(!!var), str_to_title) %>%
  mutate_at(vars(!!var), ~str_replace_all(., "And", "and")) %>%
  group_by(year, !!var) %>% 
  summarise_at(vars(n_phds), sum) %>% 
  arrange(year, !!var) %>% 
  group_by(!!var) %>% 
  mutate(index = n_phds/first(n_phds)) %>% 
  ggplot(aes(year, index)) + 
  geom_hline(yintercept = 1, alpha = .5, linetype = "dotted") +
  geom_line(aes(colour = !!var), linetype = "dashed") +
  geom_point(aes(colour = !!var), size = 2) +
  facet_wrap(facets = vars(!!var), labeller = label_wrap_gen(30)) +
  theme_minimal(base_family = "Gill Sans MT", base_size = 14) +
  theme(legend.position="none", 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(size = 20), 
        plot.subtitle = element_text(margin = margin(0,0,20,0)), 
        panel.spacing = unit(2, "lines"), 
        axis.text.x = element_text(angle = 270, vjust = .5)) +
  scale_x_continuous(breaks = seq(2009, 2017, by=2)) +
  scale_y_continuous(labels = percent) +
  scale_color_viridis_d(option = "C", end = 0.75) +
  labs(title = "Relative Change in PhD Awarded by Subject Over Time",
       subtitle = "Benchmarked to PhDs awarded in 2008",
       x = "Year",
       y = "Index Relative to 2008")

ggsave(here("plots", "phds.png"), width = 12, height = 10, units = "in", dpi = 450)

