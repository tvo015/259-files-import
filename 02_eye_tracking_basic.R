#Load packages
library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(visdat)

rm(list = ls()) #clean variables out of environment

ds <- read_csv('data_raw/101.txt') #obviously not
glimpse(ds)

#Check the file and use the write delimiter (a space)
#Skip the metadata rows, but the column names are terrible
ds <- read_delim('data_raw/101.txt', delim = " ", skip = 6) 

#c() function means 'concatenate'. Make a list. This is a list of names
col_names <- c("record_frames", "scene_frames", "avg_fps", "scene_time", "por_time", "por_x", "por_y",   
               "pupil_x", "pupil_y", "corneal_ref_x", "corneal_ref_y", "diameter_w", "diameter_h")  
ds <- read_delim('data_raw/101.txt', delim = " ", col_names = col_names, skip = 7) #Skip 1 more line!

#We got some warnings...why?

#How do our data look?
ds %>% ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled()

#Were they parsed correctly? Yes 
vis_dat(ds)

#Let's try filtering and save to a ds_cleaned tibble
ds_cleaned <- ds %>% 
  mutate(por_x = ifelse(por_x < 0 | por_x > 640, NA, por_x),
         por_y = ifelse(por_y < 0 | por_y > 480, NA, por_y))

#Double-check that our cleaned dataset plots correctly
ds_cleaned %>% 
  ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled() + 
  theme_minimal()
vis_miss(ds_cleaned)

#Let's also add a row with the subject id to the data file
ds_cleaned <- ds_cleaned %>% 
  add_column(id = 101, .before = "record_frames")

#Decide what to save
#ds_cleaned <- ds_cleaned %>% drop_na() #Is this actually a good idea?
ds_cleaned <- ds_cleaned %>% select(record_frame_count:pupil_y)

#Write to file
ds_cleaned %>% write_csv("data_cleaned/101.csv")



