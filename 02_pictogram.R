#### libraries ####
library(tidyverse)
library(gridExtra)
library(grid)
library(cowplot)
library(patchwork)
library(ggtext)
library(glue)
library(here)
library(waffle)


#### Data ####
#1. https://en.wikipedia.org/wiki/Competition_between_Airbus_and_Boeing#cite_note-127
#2. https://www.flightglobal.com/download?ac=73559
airliners_in_service<-tribble(
  ~year, ~fa_icon, ~icon_n,
  #--|--|----
  2010, "Boeing", 9318,
  2010, "Airbus", 5647,
  2020, "Boeing", 8987,
  2020, "Airbus", 7698
) 



ggplot(data= airliners_in_service%>%
         filter(year==2020))+
  geom_pictogram(aes(color = fa_icon, 
                     label = fa_icon, 
                     values = icon_n),
                 family = "Font Awesome 5 Free Solid", 
                 size = 16,
                 flip = TRUE, 
                 n_rows = 10,
                 make_proportional = TRUE) +
  scale_label_pictogram(
    name = NULL,
    values = c("plane", "plane"),
    labels = c("apple","Twitter for Android")
  ) +
  expand_limits(x = 0, y = 0)+
  scale_color_manual(values = c("Airbus" = "#79cac3", "Boeing" = "#c09449")) +
  guides(color = FALSE, label= FALSE) +
labs(
    title = "Airliners in Service in 2020: **<span style='color:#bc833c'>Boeing</span>** Vs. **<span style='color:#1b9693'>Airbus</span>**",
    subtitle = "Despite the recent troubles, Boeing still leads Airbus in the number of aircraft in Service",
    caption = "30DayChartChallenge #02| Data: flightglobal.com | @kinenealan"
  )+
  theme_void()+
  theme(
    legend.position = "none",
    text = element_text(family = "Roboto-Regular",color = "#4B5556"),
    plot.background = element_rect(fill = "transparent", linetype = 'blank'),
    panel.grid = element_blank(),
    plot.title = element_markdown(color = "black",
                                  family = "IBM Plex Sans",
                                  size = 42, 
                                  face = "bold", 
                                  hjust = 0.5,
                                  margin = margin(t = 10)),
    plot.subtitle = element_text(hjust = 0.5 , 
                                 size = 22, family = "IBM Plex Sans Medium"),
    plot.caption = element_markdown(color = "black",size = 18,
                                    hjust = 0.5)
  )+
  ggsave(here::here("plots", "02_pictogram.png"), 
                    dpi = 320, width = 16, height = 12)


         