library(lme4)
library(readr)
library(rethinking)

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

m1.4 <- map2stan(
  alist(
    binge ~ dbinom( 1 , p ),
    logit(p) <- Intercept +
      b_team*team +
      b_ind*ind +
      b_overweight*overweight +
      b_obese*obese +
      b_team_X_ind*team_X_ind +
      b_team_X_obese*team_X_obese +
      b_ind_X_obese*ind_X_obese +
      b_team_X_overweight*team_X_overweight +
      b_ind_X_overweight*ind_X_overweight +
      b_team_X_ind_X_obese*team_X_ind_X_obese +
      b_team_X_ind_X_overweight*team_X_ind_X_overweight,
    Intercept ~ dnorm(0,10),
    b_team ~ dnorm(0,10),
    b_ind ~ dnorm(0,10),
    b_overweight ~ dnorm(0,10),
    b_obese ~ dnorm(0,10),
    b_team_X_ind ~ dnorm(0,10),
    b_team_X_obese ~ dnorm(0,10),
    b_ind_X_obese ~ dnorm(0,10),
    b_team_X_overweight ~ dnorm(0,10),
    b_ind_X_overweight ~ dnorm(0,10),
    b_team_X_ind_X_obese ~ dnorm(0,10),
    b_team_X_ind_X_overweight ~ dnorm(0,10)
  )
)