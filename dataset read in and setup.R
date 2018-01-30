library(readr)
library(lme4)
library(dplyr)

HBSC_data <- read_csv("C:/Users/Student/Desktop/Data/HBSC Sport and Substance BMI/HBSC data.csv")

d <- as.data.frame(HBSC_data)

d$weed <- as.integer(d$weed)
d$sports <- as.integer(d$sports)
d$team <- as.integer(d$team)
d$ind <- d$sports - 2*d$team

d$bmifactor <- as.factor(d$`bmi classification`)
m <- dummy(d$bmifactor)
mdf <- as.data.frame(m)
mutate(mdf, any_thin = xor(thin, `severely thin`))
d <- cbind(d,mdf)

dg <- d[d$Gender == "Girl",]

db <- na.omit(d[d$Gender == "Boy",])

db2 <- db[,c("team","ind","binge","obese","overweight","any_thin","School")]
dg2 <- dg[,c("team","ind","binge","obese","overweight","any_thin","School")]

#Adds a unique conseq. numeric ID variable for each school in the data set 

db2$SchoolID <- as.numeric(factor(db2$School, levels = unique(db$School)))
dg2$SchoolID <- as.numeric(factor(dg2$School, levels = unique(db$School)))
