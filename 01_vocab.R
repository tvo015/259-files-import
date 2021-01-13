#Load packages
library(tidyverse)
library(visdat)

rm(list = ls()) #clean variables out of environment

ds <- read_csv('vocab.csv')
getwd()

#Read the data file (all defaults work so all we need is a filename)
ds <- read_csv('data_raw/vocab.csv')

#Check for 'missing' values
vis_miss(ds) 

#Wide to long (we'll go over this next week)
ds <- pivot_longer(ds, cols = everything(), names_to = "age", values_to = "word")

#Age should be a number -- let's find that problem and fix it
glimpse(ds) #Age is a character, that's not useful
ds$age <- as.numeric(ds$age) #Reassign age to be numeric
glimpse(ds) #Age correctly numeric now (dbl = number with decimals)

vis_miss(ds) #Wide to long helped, but we still have all of those missings

#Order data set by age, remove missing rows
ds <- ds %>% 
  arrange(age) %>%
  drop_na()
#What's with all the %>% ? (pipes)

vis_miss(ds) #No more missing data, age/word are correct formats

write_csv(data = ds, file = 'data_cleaned/vocab.csv') #Write to data_cleaneds

##COOL THINGS WE CAN DO THAT WE'RE NOT READY FOR YET

vis_expect(ds, ~ nchar(.x) > 10)

ds <- ds %>% group_by(age) %>% mutate(n = n()) %>% ungroup()
ds <- ds %>% mutate(item = 1, vocab_size = cumsum(item), item = NULL)

ds %>% 
  group_by(age) %>% 
  summarize(vocab_size = max(vocab_size)) %>% 
  ggplot(aes(x = age, y = vocab_size)) +  
  geom_line() + 
  geom_point() + 
  scale_x_continuous(name = "Age (months)", breaks = seq(12,24,1)) +
  ylab("Productive vocabulary size") +
  theme_minimal()

ds %>% 
  group_by(age) %>% 
  mutate(item = 1, item_by_age = cumsum(item))%>% 
  ungroup %>% 
  ggplot(aes(x = age, y = item_by_age, label = word)) + 
  geom_text(size = 3) +
  scale_x_continuous(name = "Age (months)", breaks = seq(12,24,1)) +
  theme_minimal()
