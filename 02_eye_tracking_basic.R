#Load packages
library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(visdat)
library(janitor)

rm(list = ls()) #clean variables out of environment

ds <- read_csv('data_raw/101.txt') #obviously not
glimpse(ds)

#Check the file and use the write delimiter (a space)
#Skip the metadata rows, clean up the names
ds <- read_delim('data_raw/101.txt', delim = " ", skip = 6) %>% clean_names()

#How do our data look?
ds %>% ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled()

#Were they parsed correctly? Yes 
vis_dat(ds)

#Do we have out of range values? Yes
ds %>% select(por_y) %>% vis_expect(~ .x > 0)
ds %>% select(por_x) %>% vis_expect(~ .x < 640)

#Let's try filtering and regraphing
ds %>% filter(por_x > 0 & por_x < 640 & por_y > 0 & por_y < 480) %>% 
ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled() + 
  theme_minimal()

#Let's make those changes permanent, save to a cleaned tibble
ds_cleaned <- ds %>% 
  mutate(por_x = ifelse(por_x < 0 | por_x > 640, NA, por_x),
         por_y = ifelse(por_y < 0 | por_y > 480, NA, por_y))

#Decide what to save
#ds_cleaned <- ds_cleaned %>% drop_na() #Is this actually a good idea?
ds_cleaned <- ds_cleaned %>% select(record_frame_count:pupil_y)

#Double-check that our cleaned dataset plots correctly
ds_cleaned %>% 
  ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled() + 
  theme_minimal()

#Write to file
ds_cleaned %>% write_csv("data_cleaned/101.csv")



