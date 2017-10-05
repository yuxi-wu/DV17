# Yuxi Wu
# Exploratory data analysis of DoT FARS data.

install.packages(c("readr","haven","dplyr","tidyr","stringr","ggplot2"))
library("readr")
library("haven")
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")

setwd("/Users/yuxiwu/git_svn/DV17/hw1_rgit")
acc2015 <- read_csv("accident.csv")
acc2014 <- read_sas("accident.sas7bdat")
class(acc2014)
class(acc2015)

acc2014 <- mutate(acc2014, TWAY_ID2=na_if(acc2014$TWAY_ID2, ""))
table(is.na(acc2014$TWAY_ID2))

dim(acc2014)
dim(acc2015)

colnames(acc2014) %in% colnames(acc2015)
colnames(acc2015) %in% colnames(acc2014)

colnames(acc2014)[[19]] 
# missing from 2015: "ROAD_FNC"
colnames(acc2015)[c(19,20,21)]
# missing from 2014 "RUR_URB", "FUNC_SYS", "RD_OWNER"

acc <- bind_rows(acc2014, acc2015)
count(acc, RUR_URB)
# RUR_URB was not a column in the 2014 data, which accounts for half of the rows in acc.
# Thus, half the rows in acc do not have a RUR_URB entry.
