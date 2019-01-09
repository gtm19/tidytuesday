library(tidyverse)
library(lubridate)


# Download and read in data -----------------------------------------------

# RUN ONCE
# download.file("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv",
#               "scripts/20190108 tv imdb/data/data.csv", mode = "wb")

data <-
  read_csv("scripts/20190108 tv imdb/data/data.csv")
