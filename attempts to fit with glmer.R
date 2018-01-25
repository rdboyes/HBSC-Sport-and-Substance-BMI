library(lme4)
library(readr)

HBSC_data <- read_csv("C:/Users/Student/Desktop/Data/HBSC Sport and Substance BMI/HBSC data.csv")

d <- as.data.frame(na.omit(HBSC_data))

d$weed <- as.integer(d$weed)
d$sports <- as.integer(d$sports)
d$team <- as.integer(d$team)
d$ind <- d$sports - 2*d$team

d$bmifactor <- as.factor(d$`bmi classification`)
m <- dummy(d$bmifactor)
d <- cbind(d,m)

db <- d[d$Gender == "Boy",]
dg <- d[d$Gender == "Girl",]


glmer(binge ~ team + ind + team*ind + thin + overweight + obese + `severely thin`
      + team*obese + ind*obese + team*ind*obese
      + team*overweight + ind*overweight + team*ind*overweight
      #+ team*thin + ind*thin + team*ind*thin
      #+ team*`severely thin` + ind*`severely thin` + team*ind*`severely thin`
      + (1 | School), 
     data = db, 
     family = binomial(link = "logit"),
     nAGQ = 10,
     na.action = na.exclude,
     )

glmer(binge ~ team + ind + team*ind + overweight + obese
      + team*obese + ind*obese + team*ind*obese
      + team*overweight + ind*overweight + team*ind*overweight
      + (1 | School), 
      data = db, 
      family = binomial(link = "logit"),
      nAGQ = 20,
      na.action = na.exclude,
      verbose = 1
)

