library(tidyverse)
library(lubridate)
library(patchwork)
library(zoo)

data(dashdem)
data(dashusr)

# analytics ---------------------------------------------------------------

win <- 7
tbepcol <- c('#427355', '#5C4A42', '#958984', '#004F7E', '#00806E')

thm <- theme_minimal() + 
  theme(
    legend.position = 'top', 
    axis.title.x = element_blank()
  )

toplo <- dashusr %>%
  mutate(
    total = rowSums(across(-matches('Date'))), 
    rolltotal = zoo::rollmean(total, win, na.pad = T, align = 'right')
    ) 

toplo1 <- toplo %>% 
  select(Date, total, rolltotal) %>% 
  pivot_longer(cols = -matches('Date')) %>% 
  mutate(
    name = factor(name, levels = c('total', 'rolltotal'), labels = c('Daily total', paste(win, 'day total')))
  )

toplo2 <- toplo %>% 
  mutate(
    cumtot = cumsum(total)
  )

toplo3 <- toplo %>% 
  select(-total, -rolltotal) %>% 
  pivot_longer(cols = -matches('Date'))

p1 <- ggplot(toplo1, aes(x = Date, y = value, color = name, size = name)) + 
  geom_line() + 
  scale_colour_manual(values = c('black', 'tomato1')) +
  scale_size_manual(values = c(0.5, 1)) +
  labs(
    y = 'Users', 
    color = NULL,
    size = NULL, 
    subtitle = '(a) By day'
  )

p2 <- ggplot(toplo2, aes(x = Date, y = cumsum(total))) + 
  geom_area(color = 'black',fill = 'grey', alpha = 0.2) + 
  labs(
    y = 'Users', 
    subtitle = '(b) Cumulative'
  )

p3 <- ggplot(toplo3, aes(x = Date, y = value, fill = name)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  scale_fill_manual(values = colorspace::qualitative_hcl(5, palette = 'Dark3')) +
  labs(
    y = 'Users', 
    fill = NULL, 
    subtitle = '(c) By source'
  )

pout <- p1 + p2 + p3 + plot_layout(ncol = 1) & thm

jpeg(here('figs/analytics.jpeg'), height = 6.5, width = 6, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()