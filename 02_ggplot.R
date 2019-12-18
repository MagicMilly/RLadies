#############################
####    Heidi Steiner    ####
#### Plot Comic book data  ##
#### R Ladies Tucson Meetup #
####     18 DEC 2019     ####
#############################

## plot 1
ggplot(newchars, aes(x = year, y = Total, fill = publisher)) +
  geom_bar(stat = "identity") +
  facet_grid(~publisher, labeller = labeller(publisher = publishernames)) +
  scale_x_continuous("", breaks = c(1940, 1960, 1980, 2000), labels = c("1940","'60","'80","2000")) +
  scale_y_continuous("") +
  scale_fill_manual(values = c("dodgerblue", "red2")) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 18, family = "mono"),
        panel.grid.minor = element_blank(), plot.background = element_rect(fill = "gray92", color = "white"),
        panel.grid.major = element_line(color = "gray")) +
  ggtitle("New Comic Book Characters Introduced Per Year")



## plot 2 
ggplot(percfem, aes(year, percentfemale, color = publisher)) +
  geom_line(size = 1.5) + 
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
        panel.grid.major = element_line(color = "gray")) +
  ggtitle("Comics Aren't Gaining Many Female Characters", 
          subtitle = "Percentage of new characters who are female") +
  geom_text(x = 2001,  y = .48, label = "DC", color = "dodgerblue", size = 5) +
  geom_text(x = 2001, y = .25, label = "Marvel", color = "red", size = 5)

### plot 3 
ggplot(data = percalign, aes(sex, percent, fill = align)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("firebrick1", "gold", "forestgreen")) +
  facet_grid(rows = vars(publisher), switch = "both") +
  labs(x = " ", y = " ") +
  theme_minimal()+
  theme(strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        strip.background = element_blank(),
        legend.position = "none") +
  ggtitle("Good Girls Gone Meh", subtitle = "Character alignment by gender")
  
