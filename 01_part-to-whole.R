#### libraries ####
library(tidyverse)
library(gridExtra)
library(grid)
library(cowplot)
library(patchwork)
library(ggtext)
library(ggforce)
library(glue)
library(here)

#### Data ####
share_of_operating_revenue<-tribble(
  ~group, ~share, ~year,
  #--|--|----
  "Network Domestic", 44.7, 2009,
  "Network International", 30.8, 2009,
  "Value Domestic", 21.6, 2009,
  "Value International", 0.4, 2009,
  "ULCC Domestic", 2.1, 2009,
  "ULCC International", 2.1, 2009,
  "Network Domestic", 41.2, 2018,
  "Network International", 29.3, 2018,
  "Value Domestic", 23.0, 2018,
  "Value International", 2.3, 2018,
  "ULCC Domestic", 3.8, 2018,
  "ULCC International", 0.3, 2018,
) 

#### Plot #####
#### RElative size of airline groups 

# Donut plot
# 1. The percentages are already given so no need to compute them
# 2. Compute the cumulative percentages (top of each rectangle), ymax
# 3. Compute the bottom of each rectangle, ymin
# 4. I will not use labels here

share_of_operating_revenue_2018<-share_of_operating_revenue%>%
  mutate(airline_group = str_extract(group, pattern = '\\w*'))%>%
  filter(year == 2018)%>%
  group_by(airline_group)%>%
  summarise(share = sum(share))%>%
  arrange(desc(share))%>%
  mutate(ymax = cumsum(share),
         ymin = ifelse(row_number()==1, 0,lag(ymax)))

# Make the plot

p1<-ggplot(data = share_of_operating_revenue_2018,
       aes(ymax=ymax, 
           ymin=ymin, 
           xmax=4, 
           xmin=3, 
           fill=airline_group)) +
  geom_rect() +
  coord_polar(theta="y")+
  xlim(c(1, 4))+
  labs(title = "Relative Size of Airline Categories <br>Based on Revenue",
       #subtitle = "A share of Revenue for Network, Value, and ULCC airlines",
       caption = "Alan Kinene<br>Source: Planestats.com"
  )+
  scale_fill_manual(values = c("#1b9693","#bc833c", "#a5a99b" ))+
  theme_void()+
  theme(
    legend.position = "none",
    text = element_text(family = "Roboto-Regular",color = "#4B5556", size = 12),
    plot.background = element_rect(fill = "transparent", linetype = 'blank'),
    panel.grid = element_blank(),
    plot.title = element_markdown(color = "black",
                                  family = "IBM Plex Sans",
                              size = 22, 
                              face = "bold", 
                              hjust = 0.5),
    plot.caption = element_markdown(color = "black",size = 10,hjust = 0.5)
  )

p1

p2<-ggplot()+
  geom_textbox(aes(x = 0.26,
                   y = 0.5), 
               label = "In 2018, **network carriers**, <br>**value cariers**, and **ultra-LCC** <br>had **<span style='color:#1b9693'>70%</span>**, **<span style='color:#868b78'>25.3%</span>**, and **<span style='color:#bc833c'>4.1%</span>** 
,<br>respectively.",
               color = "black",
               width = unit(0.55, "npc"),
               size = 6.5,
               fill = NA,
               box.colour = "transparent",
               hjust = 0, 
               family = "IBM Plex Sans"
               #vjust = 0.9
  )+
  #scale_x_continuous(minor_breaks = seq(0, 1, 0.001))+
  #scale_y_continuous(minor_breaks = seq(0, 1, 0.001))+
  xlim(0, 1) + 
  ylim(0, 1)+
  theme_void()+
  theme(#panel.grid = element_line(colour = "gray"),
        #panel.grid.major = element_line(colour = "red"),
        #plot.background = element_rect(fill = "transparent")
        text = element_text(family = "IBM Plex Sans")
        )


p3<-ggdraw() +
  draw_line(x = c(0.449, 0.684), 
            y = c(0.56, 0.56), 
            size = 8.6, 
            color = "#1b9693", 
            lineend = "round") +
  draw_line(x = c(0.309, 0.489), 
            y = c(0.5235, 0.5235), 
            size = 8.5, 
            color = "#a5a99b", 
            lineend = "round") +
  draw_line(x = c(0.59, 0.730), 
            y = c(0.523, 0.523), 
            size = 8.5, 
            color = "#bc833c", 
            lineend = "round") +
  draw_plot(p2)

p3


p1 + inset_element(p3, 
                   0.0, 0, 1, 1,
                   align_to = "panel",
                   clip = TRUE)





