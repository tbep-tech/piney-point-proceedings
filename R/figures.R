library(tidyverse)
library(lubridate)
library(patchwork)
library(zoo)
library(sf)
library(here)
library(ggmap)
library(ggspatial)
library(patchwork)
library(units)
library(grid)
library(scales)
library(USAboundaries)
box::use(
  scales = scales[muted], 
  units = units[set_units], 
)

data(dashdem)
data(dashusr)
data(rsallpts)
data(ppseg)
data(segmask)

# map ---------------------------------------------------------------------

# piney point loc
pineypoint <- tibble(
  lon = -82.52469352586753, 
  lat = 27.629819505234703
) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# segments
areas <- ppseg %>% 
  rename(area = Name) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_make_valid() %>%
  mutate(
    area = factor(area)
  ) %>% 
  st_intersection(segmask) %>% 
  st_cast('POLYGON') %>% 
  mutate(
    acres = st_area(.), 
    acres = set_units(acres, 'acres'), 
    acres = as.numeric(acres)
  ) %>% 
  dplyr::filter(acres > 1e4)

buffdist <- 0.01
northloc <- 'tr' 
scaleloc <- 'bl'

# layer extent as bbox plus buffer
dat_ext <- areas %>% 
  st_bbox %>% 
  st_as_sfc %>% 
  st_buffer(dist = set_units(buffdist, degree)) %>%
  st_bbox %>% 
  unname

# reference data for ggsn, MUST have geometry named column
ggsnref <- areas %>% 
  st_bbox %>% 
  st_as_sfc %>%
  st_buffer(dist = set_units(buffdist / 2, degree)) %>% 
  st_as_sf %>%
  st_cast('POINT') %>% 
  rename(geometry = x)

# stamen base map
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 10)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                         alpha.f = 0.2), 
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

tomap <- rsallpts %>% 
  mutate(
    source_lng = case_when(
      source_lng %in% c('City of St. Pete', 'ESA', 'Florida DEP', 'UF', 'USF', 'New College Fl.') ~ 'other',
      T ~ source_lng
    ), 
    source_lng = factor(source_lng, levels = c(c('FWC-FWRI', 'Hillsborough Co.', 'Manatee Co.', 'Pinellas Co.', 
                                                 'SBEP', 'TBEP', 'other')))
  ) %>% 
  .[areas, ]

p1 <- ggmap(bsmap1_transparent) +
  geom_sf(data = tomap, aes(color = source_lng), inherit.aes = F, pch = 18, size = 2) +
  geom_sf(data = pineypoint, fill = 'black', pch = 24, color = 'black', size = 3, inherit.aes= F) + 
  geom_sf_text(data = pineypoint, label = 'Piney Point', color = 'black', size = 4, inherit.aes= F, nudge_x = 0.07, nudge_y = 0) + 
  scale_color_manual(values = colorspace::qualitative_hcl(length(unique(tomap$source_lng)), palette = 'Dark3')) +
  scale_shape_manual(values= c(24)) +
  scale_fill_manual(values = c('black')) +
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    axis.title = element_blank(), 
    legend.justification = 'left',
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA), 
    legend.position = 'bottom',
    plot.caption = element_text(size = 6)
  ) +
  labs(
    color = NULL,
    fill = NULL, 
    shape = NULL, 
    title = "(a) Monitoring groups", 
    caption = ' '
  ) +
  guides(color = guide_legend(override.aes = list(size = 4), byrow = T)) + 
  annotation_north_arrow(location = northloc, which_north = "true", height = grid::unit(0.75, "cm"), 
                         width = grid::unit(0.75, "cm")) +
  annotation_scale(location = scaleloc)

tomap <- rsallpts %>% 
  mutate(
    type = case_when(
      grepl('\\,', type) ~ 'mixed', 
      T ~ type
    ), 
    type = factor(type, 
                  levels = c('water quality', 'algae', 'seagrass and macroalgae', 'mixed'), 
                  labels = c('water quality', 'algae', 'seagrass/macroalgae', 'mixed')
    )
  ) %>% 
  .[areas, ]

p2a <- ggmap(bsmap1_transparent) +
  geom_sf(data = tomap, aes(color = type), inherit.aes = F) +
  geom_sf(data = pineypoint, fill = 'black', pch = 24, color = 'black', size = 3, inherit.aes= F) + 
  scale_color_manual(values = colorspace::sequential_hcl(length(unique(tomap$type)), palette = 'Viridis')) +
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    axis.title = element_blank(), 
    legend.justification = 'left',
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA), 
    legend.position = 'bottom', 
    legend.box = 'vertical'
  ) +
  labs(
    color = NULL,
    title = '(b) Monitoring types'
  ) +
  guides(color = guide_legend(override.aes = list(size = 3), byrow = F)) + 
  annotation_north_arrow(location = northloc, which_north = "true", height = grid::unit(0.75, "cm"), 
                         width = grid::unit(0.75, "cm"))

# for inset
states <- us_states() %>% 
  filter(name %in% c('Florida', 'Georgia', 'Alabama'))
ylimrng <- states %>% 
  filter(name %in% 'Florida')
insetbb <- st_buffer(areas, dist = units::set_units(0.5, degree)) %>% 
  st_bbox() %>% 
  st_as_sfc(crs = 4326)
statebuff <- st_buffer(ylimrng, dist = 0.25)
insetylim <- st_bbox(statebuff)[c('ymin', 'ymax')]
insetxlim <- st_bbox(statebuff)[c('xmin', 'xmax')]

lbs1 <- tibble(
  lon = -85.9, lat = 25.6, label = 'Gulf of\nMexico'
) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)
lbs2 <- tibble(
  lon = -81.2, lat = 29, label = 'Florida'
) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

p2b <- ggplot() + 
  geom_sf(data = states, fill = 'grey', colour = 'grey') +
  geom_sf(data = insetbb, fill = NA, color = 'blue', size = 1.25) +
  geom_sf_text(data = lbs1, aes(label = label), size = 3.25) + 
  geom_sf_text(data = lbs2, aes(label = label), size = 3.5, angle = -65) + 
  coord_sf(ylim = insetylim, xlim = insetxlim) +
  theme_void() +
  theme( 
    panel.background = element_rect(fill = '#FFFFFF', colour = 'white'), 
    panel.border = element_rect(colour = 'black', fill = 'transparent')
  ) 

p2 <- p2a + 
  inset(ggplotGrob(p2b), xmin = -82.5035, xmax = -82.3435, ymin = 27.32, ymax = 27.5)

pout <- p1 + p2 + plot_layout(ncol = 2, guides = 'keep') & 
  theme(
    legend.justification = 'top',
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    legend.spacing.x = unit(-0.05, 'cm')
  )

# pout
jpeg(here('figs/map.jpeg'), height = 5.75, width = 8, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

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
    name = factor(name, levels = c('total', 'rolltotal'), labels = c('Daily total', paste(win, 'day rolling average')))
  )

toplo2 <- toplo %>% 
  select(-total, -rolltotal) %>% 
  pivot_longer(cols = -matches('Date')) %>% 
  group_by(name) %>% 
  mutate(value = cumsum(value))

p1 <- ggplot(toplo1, aes(x = Date, y = value, color = name, size = name)) + 
  geom_line() + 
  scale_colour_manual(values = c('black', 'tomato1')) +
  scale_size_manual(values = c(0.5, 1)) +
  labs(
    y = 'Users', 
    color = NULL,
    size = NULL, 
    subtitle = '(a) Users by day'
  )

p2 <- ggplot(toplo2, aes(x = Date, y = value, fill = name)) + 
  geom_area(position = 'stack', alpha = 0.8) + 
  # scale_fill_manual(values = colorspace::qualitative_hcl(5, palette = 'Dark3')) +
  scale_fill_manual(values = colorspace::sequential_hcl(5, palette = 'Hawaii')) +
  labs(
    y = 'Users', 
    fill = NULL, 
    subtitle = '(b) Cumulative users by source'
  )

pout <- p1 + p2 + plot_layout(ncol = 1) & thm

jpeg(here('figs/analytics.jpeg'), height = 6.5, width = 6, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()