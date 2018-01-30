library(devtools)
install.packages("rstan")

devtools::install_github("rmcelreath/rethinking", force = TRUE)

library(rethinking)

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

#Model 1.4
#Adaption to multilevel model with random intercept vector for schools (length = 192)
#Priors for all betas set with normal distributions with mean 0 and stdev 10
#Prior for standard deviation of normal distribution of random school intercepts set with a regularizing cauchy distribution

m1.4 <- map2stan(
  alist(
    binge ~ dbinom(1,p),
    logit(p) <- a 
    + a_school[SchoolID] 
    + bTeam*team
    + team*overweight*bOverInt 
    + overweight*bOver
    + team*obese*bObeseInt 
    + obese*bObese,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    c(a,bTeam,bObeseInt,bObese,bOverInt,bOver) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ), 
  data = db2)

precis(m1.4)
plot(m1.4)
plot(precis(m1.4))

post <- extract.samples(m1.4)

#Model 1.5
#Starts with 1.4, adds interaction with individual sports. Increases time for MCMC convergence by approximately 1.5x

m1.5 <- map2stan(
  alist(
    binge ~ dbinom(1,p),
    logit(p) <- a 
    + a_school[SchoolID] 
    + bTeam*team
    + bInd*ind
    + bSportInt*team*ind
    + bObese*obese
    + bTeamObeseInt*team*obese 
    + bIndObeseInt*ind*obese
    + bSportObeseInt*team*ind*obese
    + bOver*overweight
    + bTeamOverInt*team*overweight 
    + bIndOverInt*ind*overweight
    + bSportOverInt*team*ind*overweight,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    c(a,bTeam,bInd,bSportInt,bObese,bTeamObeseInt,bIndObeseInt,bSportObeseInt,bOver,bTeamOverInt,bIndOverInt,bSportOverInt) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ), 
  data = db2)

precis(m1.5)

#Model 2.0
#Starts with 1.5, adds confounding variables

m2.0 <- map2stan(
  alist(
    binge ~ dbinom(1,p),
    logit(p) <- a 
    + a_school[SchoolID] 
    + bTeam*team
    + bInd*ind
    + bSportInt*team*ind
    + bObese*obese
    + bTeamObeseInt*team*obese 
    + bIndObeseInt*ind*obese
    + bSportObeseInt*team*ind*obese
    + bOver*overweight
    + bTeamOverInt*team*overweight 
    + bIndOverInt*ind*overweight
    + bSportOverInt*team*ind*overweight,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    c(a,bTeam,bInd,bSportInt,bObese,bTeamObeseInt,bIndObeseInt,bSportObeseInt,bOver,bTeamOverInt,bIndOverInt,bSportOverInt) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ), 
  data = db2)

precis(m2.0)