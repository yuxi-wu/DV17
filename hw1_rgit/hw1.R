# Yuxi Wu
# Exploratory data analysis of DoT FARS data.

libs <- c("readr","haven","dplyr","tidyr","stringr","ggplot2")
install.packages(libs)
lapply(libs,library,character.only=TRUE)

setwd("/Users/yuxiwu/git_svn/DV17/hw1_rgit")
acc2015 <- read_csv("accident.csv")
acc2014 <- read_sas("accident.sas7bdat")
class(acc2014)
class(acc2015)

# fill "" with NAs
acc2014 <- mutate(acc2014, TWAY_ID2=na_if(acc2014$TWAY_ID2, ""))
table(is.na(acc2014$TWAY_ID2))

dim(acc2014)
dim(acc2015)

# find intersecting column names
colnames(acc2014) %in% colnames(acc2015)
colnames(acc2015) %in% colnames(acc2014)
colnames(acc2014)[[19]] # missing from 2015: "ROAD_FNC"
colnames(acc2015)[c(19,20,21)] # missing from 2014 "RUR_URB", "FUNC_SYS", "RD_OWNER"

acc <- bind_rows(acc2014, acc2015)

count(acc, RUR_URB)
# RUR_URB was not a column in the 2014 data, which accounts for half of the rows in acc.
# Thus, half the rows in acc do not have a RUR_URB entry.

fips <- read_csv("fips.csv")
glimpse(fips)

acc$STATE <- as.character(acc$STATE)
acc$STATE <- str_pad(acc$STATE, pad="0", width=2, side="left")
acc$COUNTY <- as.character(acc$COUNTY)
acc$COUNTY <- str_pad(acc$COUNTY, pad="0", width=3, side="left")

acc <- rename(acc, "StateFIPSCode"="STATE")
acc <- rename(acc, "CountyFIPSCode"="COUNTY")

acc <- left_join(acc, fips, by=c("StateFIPSCode","CountyFIPSCode"))

f <- acc %>% group_by(StateName, YEAR)
agg <- summarise(f, TOTAL = sum(FATALS))
agg_wide <- spread(agg, YEAR, TOTAL)
agg_wide <- mutate(agg_wide, Diff_Percent = (`2015`-`2014`)/`2014`) # percent difference
agg_wide <- arrange(agg_wide, desc(Diff_Percent))
agg_wide <- filter(agg_wide, Diff_Percent > 0.15 & !is.na(StateName))

# chain operator 
agg_wide <- acc %>%
            group_by(StateName, YEAR) %>%
            summarise(TOTAL = sum(FATALS)) %>% 
            spread(YEAR, TOTAL) %>%
            mutate(Diff_Percent = (`2015`-`2014`)/`2014`) %>%
            arrange(desc(Diff_Percent)) %>%
            filter(Diff_Percent > 0.15 & !is.na(StateName))

glimpse(agg_wide)
