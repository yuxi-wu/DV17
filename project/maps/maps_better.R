
libs <- c("readr","dplyr","tidyr","ggplot2",
          "extrafont", "ggmap","maps","stringr",
          "sp","rgeos","rgdal","maptools")
l <- lapply(libs,library,character.only=TRUE)
setwd("/Users/yuxiwu/git_svn/DV17/project")

`:=` <- `<-`

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

routes <- readOGR("CTA_Routes/CTA_Routes.shp")
com <- readOGR("communities/communities.shp")
com.fort <- fortify(com,region="community")
stops <- readOGR("CTA_BusStops/CTA_BusStops.shp")
stops.df <- data.frame(stops)

#routes.points = as(routes, "SpatialPointsDataFrame")
#r.fort <- fortify(routes)

coms <- ggplot(com.fort, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill=I("dodgerblue")) +
  geom_point(data=stops.df, aes(x=POINT_X, y=POINT_Y, group=OBJECTID), color="maroon",size=0.01) +
  labs(title="CTA buses extend into wealthy northern suburbs even when \nentire South Side neighbourhoods are not served",
       caption="\nSource: City of Chicago Open Data Portal, CTA Bus Stops") +
  yx_theme +
  map_margins

ggsave("CTA_map.png",plot=coms,width=19,units="cm", height=25)
