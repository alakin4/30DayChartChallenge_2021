library(tidyverse)
library(here)
library(patchwork)
library(colorspace)
library(ragg)
library(scales)
#library(ggfx)
library(glue)
library(ggtext)

#### Data ####
#### https://data.icao.int/coVID-19/operational.htm
df_passengers<-tribble(
  ~region, ~year, ~flights, ~type,
  #--|--|----|----
  "Asia/pacific", 2019, 9783201, "Domestic",
  "North America", 2019, 16994209, "Domestic",
  "Latin America/Caribbean", 2019, 1904588, "Domestic",
  "Europe", 2019, 2617608, "Domestic",
  "Africa", 2019, 334450, "Domestic",
  "Middle East", 2019, 268761, "Domestic",
  "Total", 2019, 31902817, "Domestic",
  #2020
  "Asia/pacific", 2020, 6944443, "Domestic",
  "North America", 2020, 14525519, "Domestic",
  "Latin America/Caribbean", 2020, 1018118, "Domestic",
  "Europe", 2020, 1738036, "Domestic",
  "Africa", 2020, 189206, "Domestic",
  "Middle East", 2020, 151817, "Domestic",
  "Total", 2020, 24567139, "Domestic",
  #International
  "Asia/pacific", 2019, 6292203, "International",
  "North America", 2019, 2918525, "International",
  "Latin America/Caribbean", 2019, 1303732, "International",
  "Europe", 2019, 1085907, "International",
  "Africa", 2019, 878422, "International",
  "Middle East", 2019, 607390, "International",
  "Total", 2019, 13086179, "International",
  #2020
  "Asia/pacific", 2020, 2395239, "International",
  "North America", 2020, 847347, "International",
  "Latin America/Caribbean", 2020, 543486, "International",
  "Europe", 2020, 498777, "International",
  "Africa", 2020, 322618, "International",
  "Middle East", 2020, 244768, "International",
  "Total", 2020, 4852235, "International"
) 

df_passengers_total<-df_passengers%>%
  filter(region == "Total")

#### Plot - slope#####

p2<-ggplot(data = df_passengers_total,
       aes(x = year,
           y = flights,
           color = type))+
  geom_line(size = 1.5)+
  geom_point(aes(fill = type),
             shape = 21,
             size = 5.5,
             stroke = 1.1)+
  scale_fill_manual(values = lighten(c("#1b9693","#bc833c"),0.2))+
  scale_color_manual(values = darken(c("#1b9693","#bc833c"), 0.01))+
  scale_x_continuous(breaks = c(2019, 2020),
                     minor_breaks=NULL)+
  scale_y_continuous(breaks = c(0,df_passengers_total$flights),
                     limits = c(0,NA),
                     expand = expansion(mult = c(0, .1)),
                     labels = unit_format(unit = "",scale = 1e-6))+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", linetype = 'blank'),
    panel.background = element_rect(fill = "transparent"),
    text = element_text(family = "Roboto-Regular",
                        size =10,
                        color = "#4B5556"),
    axis.title = element_blank(),
    #axis.title.y = element_text(size =18,
    #                            family = "IBM Plex Sans"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = darken("gray",0.5),
                                size = 1.5),
    panel.grid.major.y = element_line(linetype = "dotted",
                                      color = darken("gray",0.5)),
    panel.grid.major.x = element_line(color = darken("gray",0.5),
                                      size  = 1.5),
    #axis.line.x = element_line(color = darken("gray",0.5),
    #                           size = 1),
    axis.text.y = element_text(margin = margin(r = 0),
                               size =18,
                               family = "IBM Plex Sans"
                               ), 
    axis.text.x = element_text(size =18,
                               family = "IBM Plex Sans",
                               margin = margin(t = 10)),
    )

p1<-ggplot()+
  labs(title = "Global passenger air travel plunged due to COVID-19",
       subtitle = "The reduction in the number of flights in 2020 compared to 2019<br> 
                   is more pronounced for **<span style='color:#bc833c'>international (-62.92%)</span>** than **<span style='color:#1b9693'>domestic (-22.99%)</span>** travel",
       caption = "30DayChartChallenge #05 | **Data**: International Civil Aviation Organization | @kinenealan")+
  geom_richtext(aes(label = "**Flights<br>(Millions)**", 
                    x = 0.28, 
                    y = 0.98), 
                size = 6,
                lineheight = 0.9,
                fill = NA,
                hjust = 0.5, 
                label.color = NA,
                color = darken("gray",0.5))+
  xlim(0, 1) + 
  ylim(0, 1)+
  theme_void()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", linetype = 'blank'),
    panel.background = element_blank(),
    text = element_text(family = "Roboto-Regular",
                        size =10,
                        color = "#4B5556"),
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    #axis.line.x = element_line(color = darken("gray",0.5),
    #                           size = 1),
    plot.title = element_markdown(color = darken("gray",0.5),
                                  family = "IBM Plex Sans",
                                  size = 35, 
                                  face = "bold", 
                                  hjust = 0.5,
                                  margin = margin(t = 10, b = 10)),
    plot.subtitle = element_markdown(color = darken("gray",0.5),
                                     hjust = 0.5 ,
                                     lineheight = 0.8,
                                     size = 20, 
                                     family = "IBM Plex Sans",
                                     margin = margin(b = 30)),
    plot.caption = element_markdown(family = "IBM Plex Sans",
                                    size = 12,
                                    hjust = 0.5,
                                    margin = margin(t = 20)),
  )
p_final<-p1 + inset_element(p2, 
                            0.3, 0.1, 0.7, 0.95,
                            align_to = "panel",
                            clip = TRUE)
p_final+
  ggsave(here::here("plots", "05_slope.png"), 
          dpi = 320, width = 16, height = 12)
