library(tidyverse)
library(lubridate)
library(janitor)
library(extrafont)


# Read in the data --------------------------------------------------------

fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
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
  group_by(year) %>% 
  summarise_at(vars(gcc_spending), sum) %>% 
  left_join(fed_rd %>% 
              filter(department == "DOD") %>% 
              select(year, dod_rd_budget = rd_budget, gdp)) %>%
  mutate(year = dmy(paste0("0107", as.character(year)))) %>% 
  mutate_at(vars(gcc_spending, dod_rd_budget), funs(./gdp)) %>% 
  gather(measure, value, gcc_spending, dod_rd_budget) %>% 
  ggplot(aes(year, value, linetype = measure)) +
  geom_rect(aes(xmin = start, xmax = end, fill = admin), ymin = -Inf, ymax = Inf, data = us_admin, inherit.aes = F, alpha = 0.5) +
  geom_line(lwd = 1.2) +
  coord_cartesian(xlim = c(dmy(01072000), today())) +
  scale_linetype_discrete(labels = c("Defense", "Climate")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Democrat" = "#7796dd", "Republican" = "#e26a70")) +
  theme_minimal(base_family = "Roboto Mono", base_size = 18) +
  labs(x = "Year", y = "R&D Expenditure as %age of GDP", linetype = "Department", fill = "Administration")
