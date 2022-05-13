library(tidyverse)
library(lubridate)
library(here)

# dashboard analytics -----------------------------------------------------

dashusr <- read.csv(here('data/dashusrraw.csv')) %>% 
  mutate(Date = mdy(Date)) %>% 
  rename(
    `Search engine` = Organic.Search,
    `Social media` = Organic.Social, 
    `Website referral` = Referral
  ) %>% 
  filter(Date <= ymd('2021-10-01'))

save(dashusr, file = here('data/dashusr.RData'))

# May 13 2021 to Oct 1 2021
dashdem <- read.csv(here('data/dashdemraw.csv'))

save(dashdem, file = here('data/dashdem.RData'))