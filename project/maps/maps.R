
libs <- c("readr","dplyr","tidyr","ggplot2","extrafont","ggmap","maps")
l <- lapply(libs,library,character.only=TRUE)

setwd("/Users/yuxiwu/git_svn/DV17/project")

yx_theme <- theme(plot.title = element_text(family="CMU Serif",
                                            face="bold",
                                            size=16,
                                            hjust=0,
                                            margin = margin(0, 24, 0, 0)),
                  plot.subtitle = element_text(size=10,
                                               family="CMU Serif",
                                               face="italic",
                                               color="#8E8883",
                                               margin = margin(10, 24, 24, 0)),
                  axis.title = element_text(family = "CMU Serif",
                                            size=12,
                                            face="italic",
                                            color="#635F5D",
                                            margin = margin(24, 0, 24, 0)),
                  axis.text = element_text(size=8),
                  legend.title = element_text(family = "CMU Serif",
                                              size=10),
                  legend.text = element_text(family = "CMU Bright",
                                             size=8),
                  legend.background = element_rect(fill='blanchedalmond'),
                  legend.key = element_rect(fill='blanchedalmond'),
                  plot.caption=element_text(family = "CMU Bright",
                                            hjust = 0,
                                            size=8,
                                            color="black"),
                  panel.grid = element_blank(),
                  panel.background = element_rect(fill='blanchedalmond'),
                  plot.background = element_rect(fill='blanchedalmond'),
                  plot.margin = unit(c(1,1,1,1), "cm"))

map_margins <- theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks = element_blank())

stations <- read_csv("divvystations.csv")
stations <- stations %>% extract(Location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
stations$Longitude <- as.numeric(stations$Longitude)
stations$Latitude <- as.numeric(stations$Latitude)
div1412 <- read_csv("divvy2014_12.csv")
div172 <- read_csv("divvy2017_2.csv")

chicago <- get_map("chicago", source="stamen", maptype="toner-lite", zoom=11)

s <- count(div1412, from_station_id)
s <- rename(s, "ID"="from_station_id")
s <- rename(s,"Freq"="n")
s <- s %>% left_join(stations)

d14 <- qmplot(Longitude, Latitude,
              data=s,
              maptype = "toner-background",
              darken = 0.6,
              extent = "device") +
  stat_density2d(data=s, 
                 aes(x=Longitude, y =Latitude,
                     fill = ..level..,
                     alpha = ..level..),
                 size = 0.01, 
                 bins = 30,
                 geom = "polygon") +
  scale_fill_gradient(low="green",high = "red",
                      guide = guide_legend(title = "Stations")) +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  labs(title="Early Divvy stations \nconcentrated along \nnorthern 'L' branches",
       subtitle="Traditionally wealthy, established, \nalready-gentrified neighbourhoods \nsaw more initial access",
       caption="\nSource: Divvy Bikes, Q1 & Q2 2014") +
  yx_theme + 
  map_margins

ggsave("map1.png", plot=d14)

s17 <- count(div172, from_station_id)
s17 <- rename(s17, "ID"="from_station_id")
s17 <- rename(s17,"Freq"="n")
s17 <- s17 %>% left_join(stations)

d17 <- qmplot(Longitude, Latitude,
              data=s17,
              maptype = "toner-background",
              darken = 0.6,
              extent = "device") +
  stat_density2d(data=s17, 
                 aes(x=Longitude, y =Latitude,
                     fill = ..level..,
                     alpha = ..level..),
                 size = 0.01, 
                 bins = 30,
                 geom = "polygon") +
  scale_fill_gradient(low="green",high = "red",
                      guide = guide_legend(title = "Stations")) +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  labs(title="Divvy continues uneven \ndock location expansion",
       subtitle="Stations reach more parts of Chicago \nbut South Side still more poorly served",
       caption="\nSource: Divvy Bikes, Q1 & Q2 2017") +
  yx_theme + 
  map_margins

ggsave("map2.png", plot=d17)
