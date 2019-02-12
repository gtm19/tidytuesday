library(tidyverse)
library(lubridate)
library(janitor)


# Read in the data --------------------------------------------------------

fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
energy_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
climate_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")


# Plot --------------------------------------------------------------------

us_admin <-
  tribble(~admin, ~start, ~end,
          "Democrat", dmy(20011993), dmy(20012001),
          "Republican", dmy(20012001), dmy(20012009),
          "Democrat", dmy(20012009), dmy(20012017),
          "Republican", dmy(20012017), dmy(20012021))

permille <- scales::unit_format(unit = "‰", scale = 1000, accuracy = 0.01)

climate_spend %>% 
  left_join(fed_rd %>% 
              distinct(year, gdp)) %>% 
  mutate(year = dmy(paste0("0107", as.character(year)))) %>% 
  group_by(year) %>% 
  summarise(gcc_spending = sum(gcc_spending),
            gdp = mean(gdp)) %>% 
  mutate(gcc_spend_pc_gdp = gcc_spending/gdp) %>% 
  ggplot(aes(year, gcc_spend_pc_gdp)) +
  geom_rect(aes(xmin = start, xmax = end, fill = admin), ymin = -Inf, ymax = Inf, data = us_admin, inherit.aes = F, alpha = 0.5) +
  geom_line() +
  coord_cartesian(xlim = c(dmy(01072000), today()), ylim = c(0, 0.00025)) +
  scale_y_continuous(labels = function(x)permille(x)) +
  scale_fill_manual(values = c("Democrat" = "#7796dd", "Republican" = "#e26a70")) +
  theme_minimal()
