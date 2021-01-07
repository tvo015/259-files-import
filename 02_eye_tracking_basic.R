library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(visdat)

ds <- read_csv('data_raw/101.txt') #obviously not

ds <- read_delim('data_raw/101.txt', delim = " ", skip = 6) %>% 
  clean_names()

ds %>% ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled()

header <- read_delim('data_raw/101.txt', delim = ":", n_max = 5, col_names = F)
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

ds_cleaned <- ds %>% 
  mutate(por_x = ifelse(por_x < 0 | por_x > 640, NA, por_x),
         por_y = ifelse(por_y < 0 | por_y > 480, NA, por_y))

ds_cleaned <- ds_cleaned %>% drop_na()
ds_cleaned <- ds_cleaned %>% select(record_frame_count:pupil_y)

ds_cleaned %>% 
  ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled() + 
  theme_minimal()

ds_cleaned %>% write_csv("data_cleaned/101.csv")



