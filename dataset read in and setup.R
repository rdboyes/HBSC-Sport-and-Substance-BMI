library(readr)

HBSC_data <- read_csv("C:/Users/Student/Desktop/Data/HBSC Sport and Substance BMI/HBSC data.csv")

d <- as.data.frame(na.omit(HBSC_data))

d$weed <- as.integer(d$weed)
d$sports <- as.integer(d$sports)
d$team <- as.integer(d$team)
d$ind <- d$sports - 2*d$team

d$bmifactor <- as.factor(d$`bmi classification`)
m <- dummy(d$bmifactor)
mdf <- as.data.frame(m)
mdf$any_thin <- mdf$`severly thin` | mdf$thin
d <- cbind(d,m)

dg <- d[d$Gender == "Girl",]

db <- na.omit(d[d$Gender == "Boy",c("team","ind","binge","obese","overweight","any_thin","School")])