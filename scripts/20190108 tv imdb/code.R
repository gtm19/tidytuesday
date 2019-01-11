library(tidyverse)
library(lubridate)


# Download and read in data -----------------------------------------------

# RUN ONCE
# download.file("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv",
#               "scripts/20190108 tv imdb/data/data.csv", mode = "wb")

data <-
  read_csv("scripts/20190108 tv imdb/data/data.csv") %>% 
  mutate_at(vars(seasonNumber), as.factor)

data_rejigged <-
  data %>% 
  arrange(titleId, seasonNumber) %>%
  group_by(titleId) %>% 
  filter(any(seasonNumber == 1) & 
           max(as.integer(seasonNumber)) == n() &
           n() > 1 &
           any(share >= 2.5)) %>%
  mutate(rolling_mean_rating = cumsum(av_rating)/as.integer(seasonNumber)) %>% 
  filter(titleId != "tt0106179")  # exclude the X files, as it is skewing things massively

test <-
  lm(data_rejigged$share ~ data_rejigged$seasonNumber)

data_rejigged %>% 
  gather(category, value, av_rating, share) %>% 
  ggplot(aes(x = seasonNumber, y = value, colour = category, group = title)) +
  geom_line(lwd = 1.2)
