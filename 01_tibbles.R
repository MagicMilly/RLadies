#############################
####    Heidi Steiner    ####
#### Tidy Comic book data  ##
#### R Ladies Tucson Meetup #
####     18 DEC 2019     ####
#############################

# Load packages
library(tidyverse)
library(skimr)


dc <- read_csv("data/dc.csv")
marvel = read_csv("data/marvel.csv")
names(marvel)[names(marvel) == "Year"] = "YEAR"

marvel$publisher = "marvel"
dc$publisher = "dc"

comicbook = full_join(dc, marvel)

# View data
comicbook %>% skim()



## plot 1 tibble 
newchars <- comicbook %>%
  filter(grepl("Male|Female", SEX)) %>%
  mutate(Character = gsub(" Characters", "", SEX)) %>%
  group_by(publisher, Character, YEAR) %>%
  summarise(Total = n()) %>%
  ungroup() %>%
  na.omit()

## plot 1 labeller
publishernames = c("dc" = "DC, New Earth continuity","marvel" = "Marvel, Earth-616 continuity")



## plot 2 tibble
percfem <- comicbook %>%
  filter(grepl("Male|Female", SEX), YEAR >= 1980, YEAR <= 2011) %>%
  mutate(Character = gsub(" Characters", "", SEX)) %>%
  group_by(publisher, YEAR) %>%
  summarise(Total = n(), female = sum(Character == "Female")) %>%
  mutate(percentfemale = female/Total) %>% 
  ungroup() %>%
  na.omit() 


## plot 3 tibble
table(comicbook$ALIGN)
comicbook %>% count(is.na(ALIGN))

comicbook$ALIGN[comicbook$ALIGN == "Bad Characte"] = "Bad Characters"

comicbook$ALIGN = as.factor(comicbook$ALIGN)


percalign <- comicbook %>% 
  filter(grepl("Male|Female", SEX), ALIGN != "Reformed Criminals") %>%
  group_by(publisher, SEX) %>%
  summarise(good = sum(ALIGN == "Good Characters"), 
            neutral = sum(ALIGN == "Neutral Characters"), 
            bad = sum(ALIGN == "Bad Characters")) %>%
  mutate(percentgood = (good/(good + neutral + bad))*100,
         percentneutral = (neutral/(good +neutral + bad))*100,
         percentbad = (bad/(good + neutral + bad))*100) %>% 
  ungroup() %>%
  na.omit() %>% 
  gather("align", "percent", 6:8) %>% 
  group_by(publisher, SEX) %>% 
  mutate(label_ypos = cumsum(percent)) %>% 
  ungroup() %>% 
  mutate_at(8, round, 0)

percalign$SEX = factor(percalign$SEX, labels = c("Male", "Female"), 
                          levels = c("Male Characters", "Female Characters"))


percalign$align = factor(percalign$align, levels = c("percentbad", "percentneutral", "percentgood"), 
                         labels = c( "Bad", "Neutral", "Good"))

align_annotationtext = data.frame(lab = c("Good", "Neutral", "Bad"),
                            publisher = c("dc"),
                            align = c("Good", "Neutral", "Bad"),
                            percent = c(3, 59, 97),
                            cols = c("forestgreen","gold", "brickred1"))


dcpercent_annotationtext = data.frame(lab = c("42", "8", "50", "54%", "12%", "34%"), 
                                      publisher = c("dc"),
                                      align = c("Good", "Neutral", "Bad",
                                                "Good", "Neutral", "Bad"),
                                      percent = c(2.8, 47, 97, 4.2, 61, 96))


marvelpercent_annotationtext = data.frame(lab = c("30", "15", "55", "48", "21", "31"), 
                                          publisher = c("marvel"),
                                          align = c("Good", "Neutral", "Bad",
                                                    "Good", "Neutral", "Bad"),
                                          percent = c(2.8, 35, 97, 2.8, 58, 97))


publisher_annotation = data.frame(lab = c("DC", "Marvel"), 
                                  publisher = c("dc", "marvel") ,
                                  align = c("Good"))
