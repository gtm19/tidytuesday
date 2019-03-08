library(tidyverse)
library(lubridate)
library(ggforce)
library(janitor)
library(scales)
library(here)
library(extrafont)

# (specific) jobs by gender -------------------------------------------------------------------

career_length <- 45

caption <- paste0("Source : US Census Bureau\n[https://www.census.gov/data/tables/time-series/demo/industry-occupation/median-earnings.html]\nAssumes a ",career_length," year working life")

font <- "Open Sans Light"

boldfont <- "Open Sans"

jobs_gender <-
  "../R4DS Tidy Tuesday/data/2019/2019-03-05/jobs_gender.csv" %>% 
  read_csv() %>% 
  clean_names() %>% 
  filter(!is.na(wage_percent_of_male)) %>% 
  filter(year == 2016) %>% 
  mutate(extra_years = (100*career_length/wage_percent_of_male) - career_length,
         major_category = fct_reorder(major_category, extra_years, .fun = median)) %>% 
  group_by(major_category) %>% 
  mutate(median = median(extra_years))

jobs_gender  %>%
  ggplot(aes(x = major_category, y = extra_years)) + 
  geom_boxplot(aes(fill = median), outlier.colour = "transparent") +
  geom_jitter(width = 0.15, alpha = 0.1) +
  geom_text(
    aes(x = major_category, y = 45, label = major_category),
    data = distinct(jobs_gender, major_category, median),
    inherit.aes = FALSE,
    hjust = 1,
    nudge_x = 0.2,
    size = 5,
    family = boldfont
  ) +
  geom_label(
    aes(
      x = major_category,
      y = 45,
      label = paste0("Median: ", comma(median, 0.1), " years"),
      fill = median
    ),
    colour = "white",
    data = distinct(jobs_gender, major_category, median),
    inherit.aes = FALSE,
    hjust = 1,
    nudge_x = -0.2,
    size = 5,
    family = font
  ) +
  coord_flip() +
  scale_fill_gradient(low = "#CD5C6D", high = "#F2A289") +
  theme_minimal(base_size = 18, base_family = font) +
  theme(legend.position = "none", axis.text.y = element_blank(), 
        panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), plot.caption = element_text(colour = "darkgrey", size = 14, face = "italic")
        ) +
  labs(title = "A Woman's Work is Never Done",
       subtitle = "an occupation by occupation analysis of the number of extra years women\nwould have to work to match the earnings of their male colleagues",
       caption = caption,
       x = "",
       y = "Additional Working Years")

ggsave(filename = "plots/gender_gap.png", dpi = 450, width = 12, height = 8)


