library(tidyverse)
library(lubridate)
library(here)

data(dashdem)

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
