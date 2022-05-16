library(tidyverse)
library(lubridate)
library(here)

data(rsphydat)
data(rswqdat)
data(rstrndat)
data(dashdem)

# sampling effort by month and type ---------------------------------------

efftab <- list('Algae' = rsphydat, 'Water Quality' = rswqdat, 'Seagrass/macroalgae' = rstrndat) %>% 
  enframe %>% 
  unnest('value') %>% 
  select(name, date, station) %>% 
  unique %>% 
  filter(date < ymd('2021-10-01')  & date >= ymd('2021-04-01')) %>% 
  mutate(
    Month = month(date, label = TRUE), 
    name = factor(name, levels = c('Water Quality', 'Algae', 'Seagrass/macroalgae'))
  ) %>% 
  group_by(name, Month) %>% 
  summarise(
    cnt = length(unique(station)),
    .groups = 'drop'
  ) %>% 
  group_by(name) %>% 
  mutate(
    percent = round(100 * cnt / sum(cnt), 1), 
    percent = paste0('(', percent, ')')
  ) %>% 
  ungroup() %>% 
  unite('cnt', cnt, percent, sep = ' ') %>% 
  pivot_wider(names_from = 'name', values_from = 'cnt', values_fill = '-')

save(efftab, file = here('data/efftab.RData'))

# dashboard user demographics ---------------------------------------------

grps <- unique(dashdem$Group)

demtab <- NULL
for(grp in grps){
  
  tmp <- dashdem %>%
    filter(Group == grp) %>% 
    mutate(
      perc = round(100 * Users / sum(Users), 1), 
      perc = paste0('(', perc, ')')
    ) %>% 
    arrange(-Users) %>% 
    unite('Users', Users, perc, sep = ' ') %>% 
    slice(1:10) %>% 
    select(-Group)
  
  tot <- sum(dashdem$Group %in% grp)
  nms <- paste0(grp, ' (', tot, ')')
  
  tmp <- tmp %>% 
    rename(
      !!nms := Name
    )

  demtab <- bind_cols(demtab, tmp, .name_repair = 'minimal') 
  
}

save(demtab, file = here('data/demtab.RData'))  
