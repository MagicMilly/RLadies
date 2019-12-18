#############################
####    Heidi Steiner    ####
#### Tidy Comic book data  ##
#### R Ladies Tucson Meetup #
####     18 DEC 2019     ####
#############################

# Load packages
library(tidyverse)
library(skimr)

#Load data
comicbook <- read_csv("data/comicbook.csv")

# View data
comicbook %>% skim()

## plot 1 tibble 
newchars <- comicbook %>%
  filter(grepl("Male|Female", sex)) %>%
  mutate(Character = gsub(" Characters", "", sex)) %>%
  group_by(publisher, Character, year) %>%
  summarise(Total = n()) %>%
  ungroup() %>%
  na.omit()

## plot 1 labeller
publishernames = c("DC, New Earth continuity", "Marvel, Earth-616 continuity")
names(publishernames) = c("DC", "Marvel")


## plot 2 tibble
percfem <- comicbook %>%
  filter(grepl("Male|Female", sex), year >= 1980, year <= 2011) %>%
  mutate(Character = gsub(" Characters", "", sex)) %>%
  group_by(publisher, year) %>%
  summarise(Total = n(), female = sum(Character == "Female")) %>%
  mutate(percentfemale = female/Total) %>% 
  ungroup() %>%
  na.omit() 


## plot 3 tibble
table(comicbook$align)
comicbook %>% count(is.na(align))
comicbook$align = if_else(is.na(comicbook$align), "Neutral", comicbook$align)

percalign <- comicbook %>% 
  filter(grepl("Male|Female", sex)) %>%
  mutate(Character = gsub(" Characters", "", sex)) %>%
  group_by(publisher, sex) %>%
  summarise(good = sum(align == "Good Characters"), 
            neutral = sum(align == "Neutral"), 
            bad = sum(align == "Bad Characters")) %>%
  mutate(percentgood = good/(good + neutral + bad),
         percentneutral = neutral/(good +neutral + bad),
         percentbad = bad/(good + neutral + bad)) %>% 
  ungroup() %>%
  na.omit() %>% 
  gather("align", "percent", 6:8)
percalign$sex = factor(percalign$sex, labels = c("Female", "Male"), 
                          levels = c("Female Characters", "Male Characters"))
percalign$align = factor(percalign$align, levels = c("percentgood", "percentneutral", "percentbad"), 
                         labels = c( "Good", "Neutral", "Bad"))

