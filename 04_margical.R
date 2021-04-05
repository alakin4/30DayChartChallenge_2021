#### libraries ####
library(tidyverse)
library(here)
library(patchwork)
library(colorspace)
library(ragg)
library(ggfx)
library(glue)
library(ggtext)

#Unknown time to get magical idea
ggplot() +
  xlim(0,1)+ylim(0,1)+
  geom_segment(aes(x = 0.5,  #Switch line ends
                   y = 0.5, 
                   xend = 0.0557, 
                   yend = 0.5), 
               size = 2.5,
               arrow = arrow(length = unit(0.03, "npc"),
                             type="closed"),
               colour = darken("gray",0.1))+
  geom_segment(aes(x = 0.5, 
                   y = 0.5, 
                   xend = 0.7, 
                   yend = 0.5), 
               linetype = "dotted",
               size = 2.5,
               colour = darken("gray",0.1))+
  geom_segment(aes(x = 0.7, 
                   y = 0.5, 
                   xend = 0.895, 
                   yend = 0.5),
               arrow = arrow(length = unit(0.03, "npc"),
                             type="closed"),
               size = 2.5,
               colour = darken("gray",0.1))+
  # Draw ticks for the two spots
  geom_segment(aes(x = 0.05, 
                   y = 0.53, 
                   xend = 0.05, 
                   yend = 0.47), 
               size = 1,
               colour = lighten("#bc833c",0.1))+
  geom_segment(aes(x = 0.9, 
                   y = 0.53, 
                   xend = 0.9, 
                   yend = 0.47), 
               size = 1,
               colour = lighten("#1b9693",0.1))+
  # Add man looking at time, add another to add outline effect
  geom_text(aes(y = 0.5,
                x = 0.25, 
                label = "M"), 
                         color = "#1b9693", 
                         size = 40.8, 
                         family = "WeePeople", hjust = 0.5)+
  geom_text(aes(y = 0.5,
                x = 0.25, 
                label = "M"), 
            color = darken("gray",0.4), 
            size = 40, 
            family = "WeePeople", hjust = 0.5)+
  # Add text/ labels
  geom_richtext(aes(label = "**Maldives**", 
                    x = 0.009, 
                    y = 0.6), 
                size = 7,
                lineheight = 0.9,
                fill = NA,
                hjust = 0, 
                label.color = NA,
                color = lighten("#bc833c",0.1))+
  geom_richtext(aes(label = "**Margical-themed<br>
                               visualisation conneced<br>
                               to aviation**", 
                    x = 0.88, 
                    0.6), 
                size = 7,
                lineheight = 0.9,
                fill = NA,
                hjust = 0.5, 
                label.color = NA,
                color = lighten("#1b9693",0.1))+
  # Add time lables
  geom_richtext(aes(label = "12h 5m", 
                    x = 0.105, 
                    y = 0.45), 
                size = 7,
                lineheight = 0.9,
                fill = NA,
                hjust = 0, 
                label.color = NA,
                color = darken("gray",0.5))+
  geom_richtext(aes(label = "Unknown", 
                    x = 0.55, 
                    y = 0.45), 
                size = 7,
                lineheight = 0.9,
                fill = NA,
                hjust = 0, 
                label.color = NA,
                color = darken("gray",0.5))+
  labs(title = "UNKNOWN TIME",
       subtitle = "The time I will need to get a **<span style='color:#1b9693'>Magical-themed visualisation connected to aviation</span>**<br>
                   is unknown, I can make a trip to the **<span style='color:#bc833c'>Maldives</span>**!",
       caption = "30DayChartChallenge 04 | **Data**: Own Calculations | @kinenealan | **Font**: WeePeople by ProPublica")+
  theme_void()+
  theme(text = element_text(family = "Roboto-Regular",
                            color = "#4B5556"),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_markdown(color = darken("gray",0.5),
                                      family = "IBM Plex Sans",
                                      size = 48, 
                                      face = "bold", 
                                      hjust = 0.5,
                                      margin = margin(t = 10, b = 10)),
        plot.subtitle = element_markdown(color = darken("gray",0.5),
                                         hjust = 0.5 ,
                                         size = 22, 
                                         family = "IBM Plex Sans"),
        plot.caption = element_markdown(family = "IBM Plex Sans",
                                        size = 18,
                                        hjust = 0.5),
  )+
  ggsave(here::here("plots", "04_margical.png"), 
         dpi = 320, width = 16, height = 12)

