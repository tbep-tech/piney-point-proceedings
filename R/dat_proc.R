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
  )

save(dashusr, file = here('data/dashusr.RData'))

dashdem <- read.csv(here('data/dashdemraw.csv'))

save(dashdem, file = here('data/dashdem.RData'))