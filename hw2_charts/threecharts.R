# Yuxi Wu
# Three charts.

libs <- c("readr","haven","dplyr","tidyr","stringr","ggplot2")
install.packages(libs)
lapply(libs,library,character.only=TRUE)

setwd("/Users/yuxiwu/git_svn/DV17/hw2_charts")
divvy <- read_csv("Divvy_Trips_2017_Q2.csv")

# looking at gender counts
divvy <- na.omit(divvy)
gen <- count(divvy, gender)
gen <- rename(gen, "count"="n")
gen <- rename(gen, "Gender"="gender")
gen$pct <- round(((gen$count / sum(gen$count)) * 100), digits=2)
gen$pct <-  str_pad(as.character(gen$pct), pad="%", width=6, side="right")

pie <- ggplot(gen, aes(x="", y=count, fill=Gender)) +
  geom_bar(stat="identity", width=1) +
  geom_text(aes(label=pct), position = position_stack(vjust = 0.5)) +
  coord_polar(theta="y") +
  theme_void() +
  labs(title="\nGender distribution of Divvy subscriber journeys", subtitle="Taken from second quarter of 2017")


# looking at daily journey counts over 3 month period
divvy$Date <- as.Date(strptime(divvy$start_time, format="%m/%d/%Y %H:%M:%S", tz = ""))
date_agg <- count(divvy, Date)
date_agg <- rename(date_agg, "Journeys"="n")

daily_line <- ggplot(date_agg, aes(x=Date, y=Journeys, group=1)) +
  geom_line(aes(color=I("dodgerblue1"))) +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(title="\nDaily number of Divvy journeys made", subtitle="Taken from second quarter of 2017, for subscribers only")


#looking at types of journeys made by duration
divvy <- transform(divvy, group=cut(tripduration,
                                          breaks=c(-Inf,300, 900, 1800, 2700, 3600, Inf),
                                          labels=c('<5 minutes', '5-15 minutes', '15-30 minutes', '30-45 minutes','45-60 minutes', '60+ minutes')))
divvy <- rename(divvy,"tripdurtype"="group")
durations <- count(divvy, tripdurtype)
durations <- rename(durations, "Duration"="tripdurtype", "count"='n')
options(scipen=10000)
bar <- ggplot(durations, aes(x=Duration, y=count, fill=Duration)) +
  geom_bar(stat="identity") +
  theme_minimal() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(title="\nTypes of Divvy journeys made, by duration", subtitle="Taken from second quarter of 2017")
