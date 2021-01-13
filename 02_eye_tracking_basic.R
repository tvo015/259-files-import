#Load packages
library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(visdat)

rm(list = ls()) #clean variables out of environment

ds <- read_csv('data_raw/101.txt') #obviously not
glimpse(ds)

#Check the file and use the write delimiter (a space)
#Skip the metadata rows
ds <- read_delim('data_raw/101.txt', delim = " ", skip = 6) 
#These column names are terrible. What can we do?

#c() function means 'combine' to make a list. This creates a list of column names
col_names <- c("record_frames", "scene_frames", "avg_fps", "scene_time", "por_time", "por_x", "por_y",   
               "pupil_x", "pupil_y", "corneal_ref_x", "corneal_ref_y", "diameter_w", "diameter_h")  
ds <- read_delim('data_raw/101.txt', delim = " ", col_names = col_names, skip = 7) #Skip 1 more line! Why?

#We got some warnings...why?

#How do our data look? We'll cover ggplot later, but just go with it for now!
ds %>% ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled()

#Were they parsed correctly? Yes 
vis_dat(ds)

#Let's set bad values to missing and save to a ds_cleaned tibble
ds_cleaned <- ds %>% 
  mutate(por_x = ifelse(por_x < 0 | por_x > 640, NA, por_x),
         por_y = ifelse(por_y < 0 | por_y > 480, NA, por_y))

#Double-check that our cleaned dataset plots correctly
ds_cleaned %>% 
  ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled() + 
  theme_minimal()

#Save the plot to eda as a data check
ggsave(paste0("eda/et_distribution_figs/",id,"_check.png"), width = 8, height = 6, units = "in")

#Let's also add a row with the subject id to the data file
ds_cleaned <- ds_cleaned %>% 
  add_column(id = 101, .before = "record_frames")

#Decide what to save
#ds_cleaned <- ds_cleaned %>% drop_na() #Is this actually a good idea?
ds_cleaned <- ds_cleaned %>% select(record_frame_count:pupil_y)

#Write to file
ds_cleaned %>% write_csv("data_cleaned/101.csv")



