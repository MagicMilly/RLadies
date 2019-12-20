#############################
####    Heidi Steiner    ####
#### Plot Comic book data  ##
#### R Ladies Tucson Meetup #
####     18 DEC 2019     ####
#############################

#### DATA #### 
# Load packages
library(tidyverse)
library(skimr)
library(extrafont) ## if you get bored of R fonts 
font_import() ## load this package and import!

# Load data
dc <- read_csv("data/dc.csv")
marvel = read_csv("data/marvel.csv")
names(marvel)[names(marvel) == "Year"] = "YEAR" ## need to have colnames match for merging
marvel$publisher = "marvel" ## needed a variable for publishing company 
dc$publisher = "dc" ## needed a variable for publishing company 

# join the data together
comicbook = full_join(dc, marvel) # rstudio.com/resources/cheatsheets > dplyr

# View data
comicbook %>% skim() # HUGE THANKS TO ADRIANA! 


## plot 1 tibble #####
newchars <- comicbook %>%
  filter(grepl("Male|Female", SEX)) %>% ## filter only male/female characters 
  mutate(Character = gsub(" Characters", "", SEX)) %>% ## remove word "characters" from sex
  group_by(publisher, Character, YEAR) %>% ## group for summary stats
  summarise(Total = n()) %>%
  ungroup() %>% ## ALWAYS UNGROUP! 
  na.omit() 


## plot 1 labeller
publishernames = c("dc" = "DC, New Earth continuity","marvel" = "Marvel, Earth-616 continuity")


## plot 2 tibble ####
percfem <- comicbook %>%
  filter(grepl("Male|Female", SEX), YEAR >= 1980, YEAR <= 2011) %>%
  mutate(Character = gsub(" Characters", "", SEX)) %>%
  group_by(publisher, YEAR) %>%
  summarise(Total = n(), female = sum(Character == "Female")) %>%
  mutate(percentfemale = female/Total) %>% 
  ungroup() %>%
  na.omit() 


## plot 3 tibble ####
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
                                      percent = c(2.8, 45.5, 97, 3.2, 61, 96))


marvelpercent_annotationtext = data.frame(lab = c("30", "15", "55", "48", "21", "31"), 
                                          publisher = c("marvel"),
                                          align = c("Good", "Neutral", "Bad",
                                                    "Good", "Neutral", "Bad"),
                                          percent = c(2.8, 36, 97, 2.8, 58.3, 97))


publisher_annotation = data.frame(lab = c("DC", "Marvel"), 
                                  publisher = c("dc", "marvel") ,
                                  align = c("Good"))


##############################################################################################################
#### GGPLOT #### 
## The Basics!
#create a canvas
ggplot(comicbook)

# "Map" the variables of interest
ggplot(comicbook, aes(HAIR, APPEARANCES))

# Plot the data
ggplot(comicbook, aes(HAIR, APPEARANCES)) + 
  geom_col() ### How do we know what to put here?!

## additional aesthetics
# position, color, size, shape, etc. 
ggplot(comicbook, aes(HAIR, APPEARANCES, fill = SEX)) + ## fill is colors-in;  color outlines :p 
  geom_col()

# not what you wanted ?? 
## color everything not by a group
ggplot(comicbook, aes(HAIR, APPEARANCES, fill = SEX)) + 
  geom_col(fill = "red") # code in lines beneath cancel out code above 

# LAYERS! 
ggplot(comicbook, aes(HAIR, APPEARANCES, fill = SEX)) + 
  geom_col(position = "dodge") + ## position adjustment! 
  geom_boxplot()
### what are we doing today? PLOTS! LOL

## plot 1 tibble ^^^^ ####
# Plot 1 here






## plot 1 finale
ggplot(newchars, aes(x = YEAR, y = Total, fill = publisher)) +
  geom_bar(stat = "identity", width = 1) +
  scale_x_continuous("", breaks = c(1940, 1960, 1980, 2000), labels = c("1940","'60","'80","2000")) +
  scale_y_continuous("") +
  scale_fill_manual(values = c("dodgerblue", "red2")) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 18, family = "mono"),
        panel.grid.minor = element_blank(), plot.background = element_rect(fill = "gray92", color = "white"),
        panel.grid.major = element_line(color = "gray")) +
  ggtitle("New Comic Book Characters Introduced Per Year") +
  facet_grid(~publisher, labeller = labeller(publisher = publishernames))


## plot 2 tibble ^^^^  ####
## plot 2 here 







## plot 2 
ggplot(percfem, aes(YEAR, percentfemale, color = publisher)) +
  geom_line(size = 1.2) + 
  scale_x_continuous("", 
                     breaks = c(1980, 1990, 2000, 2010), 
                     labels = c("1980", "'90", "2000", "'10")) +
  scale_y_continuous("",
                     limits = c(0, .5), 
                     breaks = c(0, .10, .20, .30, .40, .50), 
                     labels = c("0", "10", "20", "30", "40", "50%")) + 
  scale_color_manual(values = c("dodgerblue", "red2")) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        text = element_text(size = 14, family = "mono"), 
        plot.background = element_rect(fill= "gray92", color = "white"),
        panel.grid.major = element_line(color = "gray"),
        plot.subtitle = element_text(margin = margin(0,0,30,0))) +
  ggtitle("Comics Aren't Gaining Many Female Characters", 
          subtitle = "Percentage of new characters who are female") +
  geom_text(x = 2001,  y = .48, label = "DC", color = "dodgerblue", size = 5) +
  geom_text(x = 2001, y = .25, label = "Marvel", color = "red", size = 5)

## plot 3 tibble ^^^ ####
## plot 3 here 






### plot 3 
ggplot(data = percalign, aes(SEX, percent, fill = align)) +
  geom_col() +
  coord_flip(ylim = c(0,100), clip = "off") +
  facet_grid(rows = vars(publisher), switch = "both") +
  scale_fill_manual(values = c("firebrick1", "gold", "forestgreen")) +
  theme(legend.position = "none",
        strip.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 15, family = "sans"),
        plot.background = element_rect(fill = "gray92", color = "white"),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(3,0,3,2), "cm"),
        plot.subtitle = element_text(margin = margin(0,0,50,0))) +
  labs(y = " ", x = " ") +
  ggtitle("Good Girls Gone Meh", subtitle = "Character alignment by gender")+
  geom_text(data = align_annotationtext, aes(x = 3, y = percent, 
                                       label = lab, color = factor(align)), size = 4,
            fontface = "bold", inherit.aes = F, color = c("forestgreen", "gold", "firebrick1"))+
  geom_text(data = dcpercent_annotationtext, aes(x = c(1, 1, 1, 2, 2, 2), y = percent, 
                                                 label = lab), color = "white", fontface = "bold", family = "mono") +
  geom_text(data = marvelpercent_annotationtext, aes(x = c(1, 1, 1, 2, 2,2),
                                                     y = percent, label = lab),
            color = "white", fontface = "bold", family = "mono") +
  geom_text(data = publisher_annotation, aes(x = c(3,3), y = c(-12, -12),
                                             label = lab, fontface = "bold"))
         

