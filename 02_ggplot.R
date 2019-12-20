#############################
####    Heidi Steiner    ####
#### Plot Comic book data  ##
#### R Ladies Tucson Meetup #
####     18 DEC 2019     ####
#############################

library(extrafont)
font_import()

## plot 1
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
        panel.spacing = unit(5, "lines"),
        plot.margin = unit(c(3,0,3,2), "cm"),
        plot.subtitle = element_text(margin = margin(0,0,50,0))) +
  labs(y = " ", x = " ") +
  ggtitle("Good Girls Gone Meh", subtitle = "Character alignment by gender")+
  geom_text(data = align_annotationtext, aes(x = 3, y = percent, 
                                       label = lab, color = factor(align)), size = 4,
            fontface = "bold", inherit.aes = F)+
  geom_text(data = dcpercent_annotationtext, aes(x = c(1, 1, 1, 2, 2, 2), y = percent, 
                                                 label = lab), color = "white") +
  geom_text(data = marvelpercent_annotationtext, aes(x = c(1, 1, 1, 2, 2,2),
                                                     y = percent, label = lab),
            color = "white") +
  geom_text(data = publisher_annotation, aes(x = c(3,3), y = c(-12, -12),
                                             label = lab, fontface = "bold"))
         

