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
#Starts with 1.5, adds confounding variables. Convergence time now at >10.0x compared to model 1.4. (40 minutes on my PC)

m2.0 <- map2stan(
  alist(
    harddrug ~ dbinom(1,p),
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
    + bSportOverInt*team*ind*overweight
    + bSESgood*SESgood
    + bSESgreat*SESgreat
    + bSESfair*SESfair
    + bSESbad*SESbad
    + bfam2*fam2
    + bfam3*fam3
    + bfam4*fam4
    + bWhite*white
    + bSiblings*siblings
    + bRURmed*RURmed
    + bRURlarge*RURlarge
    + bRURvsmall*RURvsmall
    + bRURsmall*RURsmall
    + bGrade_cat*grade_cat,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    c(a,bTeam,bInd,bSportInt,bObese,bTeamObeseInt,bIndObeseInt,bSportObeseInt,bOver,bTeamOverInt,bIndOverInt,bSportOverInt) ~ dnorm(0,10),
    c(bSESgreat,bSESgood,bSESfair,bSESbad,bfam2,bfam3,bfam4,bWhite,bSiblings,bRURmed,bRURlarge,bRURvsmall,bRURsmall,bGrade_cat) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1)
  ), 
  data = db2)

#Generate parameter estimates

plot(precis(m2.0))

#Check Markov Chain convergence (Currently some unhealthy behavior in sigma_school)

plot(m2.0, pars = c("bTeam","bInd","bSiblings","bObese","bOver",
                    "bSESbad","bSESfair","bSESgood","bSESgreat","bfam2","bfam3","bfam4",
                    "bRURmed", "bRURlarge","bRURvsmall","bRURsmall","bGrade_cat","sigma_school"),max_rows = 6)

#Sample 1000 points from the posterior distribution

post <- extract.samples(m2.0)

#Combine these 1000 points to form predictions for all types of people 
#(not perfect - this code maintains the clusters from the sample rather than generalizing over average expected clusters)

post$pred_norm_ind <- post$bInd
post$pred_norm_team <- post$bTeam
post$pred_norm_both <- post$bInd + post$bTeam + post$bSportInt
post$pred_over_ind <- post$bInd + post$bIndOverInt + post$bOver
post$pred_over_team <- post$bTeam + post$bTeamOverInt + post$bOver
post$pred_over_both <- post$bInd + post$bTeam + post$bOver + post$bSportInt + post$bTeamOverInt  + post$bIndOverInt + post$bSportOverInt 
post$pred_obese_ind <- post$bInd + post$bIndObeseInt + post$bOver
post$pred_obese_team <- post$bTeam + post$bTeamObeseInt + post$bOver
post$pred_obese_both <- post$bInd + post$bTeam + + post$bObese + post$bSportInt + post$bTeamObeseInt  + post$bIndObeseInt + post$bSportObeseInt

#Graph relationships for overweight/obese

dens(post$bObese, xlim = c(-1.5,1.5),ylim=c(0,3),col="#00FF00", xlab = "Beta")
dens(post$bOver, add=TRUE,col="#FF0000")
abline(add=TRUE,v=0,col="#0000FF")

#Graph changes in association over BMI in athletes who play both

dens(post$pred_norm_both, xlim = c(-1.5,1.5),col=rangi2, xlab = "Beta")
dens(post$pred_over_both, add=TRUE,col=0x00FF00)
dens(post$pred_obese_both, add=TRUE)

#Graph changes in association over BMI in athletes who play only team

dens(post$pred_norm_team, xlim = c(-1.5,1.0),col=rangi2, xlab = "Beta")
dens(post$pred_over_team, add=TRUE,col=0x00FF00)
dens(post$pred_obese_team, add=TRUE)

#Graph changes in association over BMI in athletes who play only individual

dens(post$pred_norm_ind, xlim = c(-1.5,1.0),col=rangi2, xlab = "Beta")
dens(post$pred_over_ind, add=TRUE,col=0x00FF00)
dens(post$pred_obese_ind, add=TRUE)

#Graph all calculated CIs on one graph

dens(post$pred_obese_ind, xlim = c(-1.5,1.5),col="#00FF00",ylim=c(0,4), xlab = "Beta")
dens(post$pred_obese_team, add=TRUE,col="#00FF00")
dens(post$pred_obese_both, add=TRUE,col="#00FF00")
dens(post$pred_over_ind, add=TRUE,col="#FF0000")
dens(post$pred_over_team, add=TRUE,col="#FF0000")
dens(post$pred_over_both, add=TRUE,col="#FF0000")
dens(post$pred_norm_ind, add=TRUE,col="#0000FF")
dens(post$pred_norm_team, add=TRUE,col="#0000FF")
dens(post$pred_norm_both, add=TRUE,col="#0000FF")





