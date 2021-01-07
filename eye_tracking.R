library(tidyverse)
library(visdat)
library(janitor)

ds <- read_csv('data_raw/et.txt') #obviously not

col_names <- read_delim('data_raw/et.txt', delim = " ", skip = 5) %>% 
  rename(
    sceneQTtime = `sceneQTtime(d:h:m:s.tv/ts)`, 
    porQTtime = `porQTtime(d:h:m:s.tv/ts)`) %>% 
  names()

ds <- read_delim('data_raw/et.txt', delim = " ", skip = 6, col_names = col_names) %>% 
  clean_names()

ds %>% ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled()

header <- read_delim('data_raw/et.txt', delim = ":", n_max = 5, col_names = F)
header <- header %>% rename(field = X1)
header <- header %>% unite("value", X2:X4, remove = T, na.rm = T)

vis_dat(ds)
ds %>% select(por_y) %>% vis_expect(~ .x > 0)
ds %>% select(por_x) %>% vis_expect(~ .x < 640)

ds %>% filter(por_x < 0 | por_x > 640 | por_y < 0 | por_y > 480) %>% head

ds %>% filter(por_x > 0 & por_x < 640 & por_y > 0 & por_y < 480) %>% 
ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled() + 
  theme_minimal()
