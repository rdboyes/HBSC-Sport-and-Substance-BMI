library(readr)
library(devtools)

devtools::install_github("rmcelreath/rethinking", force = TRUE)

HBSC_data <- read_csv("C:/Users/Student/Desktop/Data/HBSC Sport and Substance BMI/HBSC data.csv")

d <- as.data.frame(na.omit(HBSC_data))

d$weed <- as.integer(d$weed)
d$sports <- as.integer(d$sports)
d$team <- as.integer(d$team)
d$ind <- d$sports - 2*d$team

d$bmifactor <- as.factor(d$`bmi classification`)
m <- dummy(d$bmifactor)
d <- cbind(d,m)

db <- na.omit(d[d$Gender == "Boy",c("team","ind","binge","obese","overweight","School")])

m1.2 <- map2stan(
  alist(
    binge ~ dbinom(1,p),
    logit(p) <- a + bTeam*team + bInd*ind + bSportInt*team*ind
    + (bTeam*team + bInd*ind + bSportInt*team*ind)*overweight*bOverInt + overweight*bOver
    + (bTeam*team + bInd*ind + bSportInt*team*ind)*obese*bObeseInt + obese*bObese,
    c(a,bTeam,bInd,bSportInt,bObeseInt,bObese,bOverInt,bOver) ~ dnorm(0,10)
  ), 
  data = db)

precis(m1.2)

m1.3 <- map2stan(
  alist(
    binge ~ dbinom(1,p),
    logit(p) <- a + bTeam*team +
    + team*overweight*bOverInt + overweight*bOver
    + team*obese*bObeseInt + obese*bObese,
    c(a,bTeam,bObeseInt,bObese,bOverInt,bOver) ~ dnorm(0,10)
  ), 
  data = db)

precis(m1.3)

db$SchoolID <- as.numeric(factor(db$School, levels = unique(db$School)))

m1.4 <- map2stan(
  alist(
    binge ~ dbinom(1,p),
    logit(p) <- a + a_school[SchoolID] + bTeam*team +
    + team*overweight*bOverInt + overweight*bOver
    + team*obese*bObeseInt + obese*bObese,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    c(a,bTeam,bObeseInt,bObese,bOverInt,bOver) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ), 
  data = db)

precis(m1.4)
plot(m1.4)
plot(precis(m1.4))

post <- extract.samples(m1.4)