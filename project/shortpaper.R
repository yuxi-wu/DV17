# Yuxi Wu
# Short paper.

libs <- c("readr","haven","dplyr","tidyr","stringr","ggplot2","lubridate","reshape2","reshape")
lapply(libs,library,character.only=TRUE)

setwd("/Users/yuxiwu/git_svn/DV17/hw3_shortpaper")
bus <- read_csv("busridership_byroute.csv")
bus_rides <- bus[,!(names(bus) %in% c("daytype"))]
divvy <- read_csv("../hw2_charts/Divvy_Trips_2017_Q2.csv")

divvy <- na.omit(divvy)
bus_rides <- na.omit(bus_rides)

# looking at gender counts
gen <- count(divvy, gender)
gen <- rename(gen, "count"="n")
gen <- rename(gen, "Gender"="gender")
gen$pct <- round(((gen$count / sum(gen$count)) * 100), digits=2)
gen$pct <-  str_pad(as.character(gen$pct), pad="%", width=6, side="right")
labels.gen <- paste(factor(gen$Gender), "\n", gen$pct, sep="")

pie_gen <- ggplot(gen, aes(x="", y=count, fill=factor(Gender))) +
  geom_bar(stat="identity", width=1) +
  geom_text(aes(label=labels.gen), position = position_stack(vjust = 0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "none") + 
  labs(title="\nMale Divvy subscriber journeys outnumber female by three to one",
       subtitle="Source: Divvy Bikes system data, Q1 & Q2 2017")


# looking at daily journey counts over 3 month period
divvy$Date <- as.Date(strptime(divvy$start_time, format="%m/%d/%Y %H:%M:%S", tz = ""))
date_agg <- count(divvy, Date)
date_agg <- rename(date_agg, "Journeys"="n")

divvy_line <- ggplot(date_agg, aes(x=Date, y=Journeys, group=1)) +
  geom_line(aes(color=I("dodgerblue1"))) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(title="\nDivvy Ridership increases through season changes",
       subtitle="Source: Divvy Bikes system data, Q1 & Q2 2017",
       x="Date in 2017",
       y="Journeys")


#looking at types of journeys made by duration
divvy <- transform(divvy, group=cut(tripduration,
                                    breaks=c(-Inf,300, 900, 1800, 2700, 3600, Inf),
                                    labels=c('<5 minutes', '5-15 minutes', '15-30 minutes', '30-45 minutes','45-60 minutes', '60+ minutes')))
divvy <- rename(divvy,"tripdurtype"="group")
durations <- count(divvy, tripdurtype)
durations <- rename(durations, "Duration"="tripdurtype", "count"='n')

bar <- ggplot(durations, 
              aes(x=Duration, y=count, fill=Duration)) +
  geom_bar(stat="identity",
           fill="#FF6666") +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(title="\nTypes of Divvy journeys made, by duration",
       subtitle="Taken from second quarter of 2017")


bus_rides$date <- str_trim(bus_rides$date)
bus_rides$date <- as.Date(strptime(bus_rides$date, format="%m/%d/%Y", tz = ""))
date_bus <- count(bus_rides, date)
date_bus <- rename(date_bus, "Rides"="n")

bus_month <- bus_rides %>% group_by(month=floor_date(date, "month")) %>%
  summarize(amount=sum(rides))
bus_month <- rename(bus_month, "Rides"="amount")
bus_month <- rename(bus_month, "Month"="month")
bus_month <- transform(bus_month, Rides=Rides/1000000)

bus_line <- ggplot(bus_month, aes(x=Month, y=Rides, group=1)) +
  geom_line(aes(color=I("dodgerblue1"))) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(title="\nDownward trend in city-wide bus ridership since summer 2013",
       subtitle="Source: Daily Bus Ridership Totals, City of Chicago",
       x="Date",
       y="Rides (millions)")


  
top_buses <- aggregate(bus_rides$rides, by=list(Category=bus_rides$route), FUN=sum)
top_buses <- rename(top_buses, "Rides"="x")
top_buses <- rename(top_buses, "Route"="Category")
top_buses <- transform(top_buses, Rides=Rides/1000000)

top_buses <- top_buses[order(-top_buses$Rides),][1:20,]
tb <- ggplot(top_buses, aes(x=reorder(Route, -Rides),y=Rides)) + 
  geom_bar(stat='identity') +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(title="\nTop 20 most ridden bus routes across Chicago",
       subtitle="Source: Daily Bus Ridership Totals, City of Chicago",
       x="Route",
       y="Rides (millions)")


bus_wide <- spread(bus_rides, route, rides)
bus_rides$mo <- strftime(bus_rides$date, "%m")
bus_rides$yr <- strftime(bus_rides$date, "%Y")

b79 <- subset(bus_rides, route=="79")


b79m <- data.frame(matrix(, nrow=12, ncol=0))
y14 <- filter(bus_rides, (yr=="2014" & route=="79")) %>% 
  group_by(mo) %>%
  summarize(amount=sum(rides))
y15 <- filter(bus_rides, (yr=="2015" & route=="79")) %>%
  group_by(mo) %>%
  summarize(amount=sum(rides))
y16 <- filter(bus_rides, (yr=="2016" & route=="79")) %>% 
  group_by(mo) %>%
  summarize(amount=sum(rides))

b79m$y14 <- y14$amount / 10000
b79m$y15 <- y15$amount / 10000
b79m$y16 <- y16$amount / 10000
b79m$month <- c("01","02","03","04","05","06","07","08","09","10","11","12")


b79m_melt <- melt(b79m, id.vars="month")

ggplot(b79m_melt, aes(x=month, y=value, group=variable, colour=variable)) + 
  geom_line() +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_color_manual(labels = c("2014", "2015","2016"), values = c("#d2e8ff", "#bbddff","#ff8d1e")) +
  labs(title="\nYearly ridership on 79 bus route",
       subtitle="Source: Daily Bus Ridership Totals, City of Chicago",
       x="Month",
       y="Rides (10,000)",
       color = "Year\n")
