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
library(ggimage)
library(colorspace)


airbus_png <- here("img", "airbus.png")
boeing_png <- here("img", "boeing.png")


transparent <- function(img) {
  magick::image_fx(img, expression = "0.7*a", channel = "alpha")
}


ggplot() +
  xlim(0,1)+ylim(0,1)+
  geom_image(aes(image = boeing_png, 0.05, 0.55), 
             size = 0.55, 
             image_fun = transparent,
             asp = 1.33)+
  geom_richtext(aes(label = "BOEING", 
                0.13, 0.65),
            family = "Londrina Outline Regular", 
            size = 28,
            lineheight = 0.9,
            fill = NA,
            hjust = 0, 
            label.color = NA,
            color = darken("#c09449", 0.4, space = "HCL"))+
  geom_richtext(aes(label = "The first commercial flight<br>
                             was on **<span>October 26, 1958</span>** with<br>
                             a Boeing 707 jetliner with<br>
                             four engines and capacity of<br>
                             about 156 seats", 
                    0.05, 0.23),
                family = "IBM Plex Sans", 
                size = 10,
                fill = NA,
                hjust = 0, 
                label.color = NA,
                color = "#c09449")+
  geom_image(aes(image = airbus_png, 0.95, 0.55), 
             size = 0.55, 
             image_fun = transparent,
             asp = 1.33)+
  geom_richtext(aes(label = "**AIRBUS**", 
                0.72, 0.65),
            family = "Londrina Outline Regular", 
            size = 28,
            lineheight = 0.9,
            fill = NA,
            hjust = 0, 
            label.color = NA,
            color = darken("#79cac3", 0.4, space = "HCL"))+
  geom_richtext(aes(label = "The first commercial flight<br>
                             was on **October 28, 1972** with<br>
                             a more economical A300B of<br>
                             two engines and capacity of<br>
                             about 300 seats", 
                    0.65, 0.23),
                family = "IBM Plex Sans", 
                size = 10,
                fill = NA,
                hjust = 0, 
                label.color = NA,
                color = darken("#79cac3",0.5))+
  labs(title = "The First Commercial Passenger Flight<br>by the Two Most Dominant Aircraft Manufacturers",
      caption = "30DayChartChallenge 03 | Data: Boeing and airbus | @kinenealan")+
  theme_void()+
  theme(text = element_text(family = "Roboto-Regular",color = "#4B5556"),
        plot.background = element_rect(fill = "transparent", linetype = 'blank'),
        panel.grid = element_blank(),
        plot.title = element_markdown(color = darken("gray",0.5),
                                      family = "IBM Plex Sans",
                                      size = 48, 
                                      face = "bold", 
                                      hjust = 0.5,
                                      margin = margin(t = 10)),
        plot.caption = element_markdown(color = "black",size = 18,
                                        hjust = 0.5),
        )+
  ggsave(here::here("plots", "03_historical.png"), 
         dpi = 320, width = 16, height = 12)
 

  