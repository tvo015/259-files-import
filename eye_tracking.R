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

vis_dat(ds)
ds %>% select(por_x) %>% vis_expect(~ .x > 0)
ds %>% select(por_y) %>% as.numeric() %>% vis_expect(~ .x > 0 & ~ .x <640)


ds <- pivot_longer(ds, cols = everything(), names_to = "age", values_to = "word")
ds$age <- as.numeric(ds$age)

ds <- ds %>% 
  arrange(age) %>%
  filter(!is.na(word)) %>% 
  filter(!duplicated(word))

vis_dat(ds)
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
