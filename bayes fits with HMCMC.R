library(devtools)

devtools::install_github("rmcelreath/rethinking", force = TRUE)

#Bad model. Doesn't work at all

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

#Split model, works much better

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

#Adds a unique conseq. numeric ID variable for each school in the data set 

db$SchoolID <- as.numeric(factor(db$School, levels = unique(db$School)))

#Adaption to multilevel model with random intercept vector for schools (length = 192)

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

#Adds thin people back in, convergence questionable

m1.5 <- map2stan(
  alist(
    binge ~ dbinom(1,p),
    logit(p) <- a + a_school[SchoolID] + bTeam*team +
      + team*overweight*bOverInt + overweight*bOver
    + team*obese*bObeseInt + obese*bObese,
    + team*any_thin*bThinInt + bThin*any_thin,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    c(a,bTeam,bObeseInt,bObese,bOverInt,bOver,bThin,bThinInt) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ), 
  data = db)