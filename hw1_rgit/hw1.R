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
