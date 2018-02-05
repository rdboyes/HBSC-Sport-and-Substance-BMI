#m2.1 adds a third level (Region) and collapses rurality to allow hopefully faster convergence (~30 min). 
#Hopefully better Markov Chain behavior as well?

m2.1 <- map2stan(
  alist(
    harddrug ~ dbinom(1,p),
    logit(p) <- a 
    + a_school[SchoolID] 
    + a_region[region]
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
    + bGrade_cat*grade_cat
    + bRURlarge*RURlarge
    + bRURsmall*RURanysmall,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    a_region[region] ~ dnorm(0,sigma_region),
    c(a,bTeam,bInd,bSportInt,bObese,bTeamObeseInt,bIndObeseInt,bSportObeseInt,bOver,bTeamOverInt,bIndOverInt,bSportOverInt) ~ dnorm(0,10),
    c(bSESgreat,bSESgood,bSESfair,bSESbad,bfam2,bfam3,bfam4,bWhite,bSiblings,bRURlarge,bRURsmall,bGrade_cat) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,1),
    sigma_region ~ dcauchy(0,1)
  ), 
  data = db2)

plot(precis(m2.1))

#Check Markov Chain convergence (Currently some unhealthy behavior in sigma_school)

plot(m2.1, pars = c("bTeam","bInd","bSiblings","bObese","bOver",
                    "bSESbad","bSESfair","bSESgood","bSESgreat","bfam2","bfam3","bfam4",
                    "bRURlarge","bRURsmall","bGrade_cat","sigma_school"),max_rows = 6)


post <- extract.samples(m2.1)

post$pred_norm_ind <- post$bInd
post$pred_norm_team <- post$bTeam
post$pred_norm_both <- post$bInd + post$bTeam + post$bSportInt
post$pred_over_ind <- post$bInd + post$bIndOverInt + post$bOver
post$pred_over_team <- post$bTeam + post$bTeamOverInt + post$bOver
post$pred_over_both <- post$bInd + post$bTeam + post$bOver + post$bSportInt + post$bTeamOverInt  + post$bIndOverInt + post$bSportOverInt 
post$pred_obese_ind <- post$bInd + post$bIndObeseInt + post$bOver
post$pred_obese_team <- post$bTeam + post$bTeamObeseInt + post$bOver
post$pred_obese_both <- post$bInd + post$bTeam + + post$bObese + post$bSportInt + post$bTeamObeseInt  + post$bIndObeseInt + post$bSportObeseInt


dens(post$sigma_school, xlim = c(-1.5,1.5),ylim=c(0,8),col="#000000", xlab = "sigma")
dens(post$sigma_region, add=TRUE,col="#FF0000")

#M2.2 removes individual sport (as effect is almost nil) and includes thin people

m2.2 <- map2stan(
  alist(
    harddrug ~ dbinom(1,p),
    logit(p) <- a 
    + a_school[SchoolID] 
    + a_region[region]
    + bTeam*team
    + bObese*obese
    + bTeamObeseInt*team*obese 
    + bOver*overweight
    + bTeamOverInt*team*overweight 
    + bThin*any_thin
    + bTeamThin*any_thin*team
    + bSESgood*SESgood
    + bSESgreat*SESgreat
    + bSESfair*SESfair
    + bSESbad*SESbad
    + bfam2*fam2
    + bfam3*fam3
    + bfam4*fam4
    + bWhite*white
    + bSiblings*siblings
    + bGrade_cat*grade_cat
    + bRURlarge*RURlarge
    + bRURsmall*RURanysmall,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    a_region[region] ~ dnorm(0,sigma_region),
    c(a,bTeam,bObese,bTeamObeseInt,bOver,bTeamOverInt,bThin,bTeamThin) ~ dnorm(0,10),
    c(bSESgreat,bSESgood,bSESfair,bSESbad,bfam2,bfam3,bfam4,bWhite,bSiblings,bRURlarge,bRURsmall,bGrade_cat) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,3),
    sigma_region ~ dcauchy(0,3)
  ), 
  data = db2)

plot(precis(m2.2))

#Check Markov Chain convergence (Currently some unhealthy behavior in sigma_school)

plot(m2.2, pars = c("bTeam","bSiblings","bObese","bOver",
                    "bSESbad","bSESfair","bSESgood","bSESgreat","bfam2","bfam3","bfam4",
                    "bRURlarge","bRURsmall","bGrade_cat","sigma_school"),max_rows = 6)


post <- extract.samples(m2.2)

post$pred_norm_team <- post$bTeam
post$pred_over_team <- post$bTeam + post$bTeamOverInt + post$bOver
post$pred_obese_team <- post$bTeam + post$bTeamObeseInt + post$bObese
post$pred_thin_team <- post$bTeam + post$bTeamThin + post$bThin
post$pred_over_none <- post$bOver
post$pred_obese_none <- post$bObese
post$pred_thin_none <- post$bThin


dens(post$sigma_school, xlim = c(-1.5,1.5),ylim=c(0,8),col="#000000", xlab = "sigma")
dens(post$sigma_region, add=TRUE,col="#FF0000")

dens(post$pred_norm_team, xlim = c(-1.5,1.5),ylim=c(0,8),col="#000000", xlab = "beta")
dens(post$pred_thin_team, add=TRUE,col="#FF0000")
dens(post$pred_over_team, add=TRUE,col="#FF0000")
dens(post$pred_obese_team, add=TRUE,col="#FF0000")

dens(post$pred_thin_none, xlim = c(-1.5,1.5),ylim=c(0,8),col="#FF0000", xlab = "beta")
dens(post$pred_over_none, add=TRUE,col="#FF0000")
dens(post$pred_obese_none, add=TRUE,col="#FF0000")
abline(v=0,col="#000000", add=TRUE)

rows <- c("Thin BMI","Normal BMI","Overweight BMI","Obese BMI")
columns <- c("No Sport","Low","High","Team Sport","TLow","THigh")

results <- as.data.frame(exp(rbind(
  c(mean(post$pred_thin_none),HPDI(post$pred_thin_none)),
  c(0,0,0),
  c(mean(post$pred_over_none),HPDI(post$pred_over_none)),
  c(mean(post$pred_obese_none),HPDI(post$pred_obese_none)),
  c(mean(post$pred_thin_team),HPDI(post$pred_thin_team)),
  c(mean(post$pred_norm_team),HPDI(post$pred_norm_team)), 
  c(mean(post$pred_over_team),HPDI(post$pred_over_team)), 
  c(mean(post$pred_obese_team),HPDI(post$pred_obese_team))
  )))

#M2.3 is 2.2 but for girls instead of boys

m2.3 <- map2stan(
  alist(
    harddrug ~ dbinom(1,p),
    logit(p) <- a 
    + a_school[SchoolID] 
    + a_region[region]
    + bTeam*team
    + bObese*obese
    + bTeamObeseInt*team*obese 
    + bOver*overweight
    + bTeamOverInt*team*overweight 
    + bThin*any_thin
    + bTeamThin*any_thin*team
    + bSESgood*SESgood
    + bSESgreat*SESgreat
    + bSESfair*SESfair
    + bSESbad*SESbad
    + bfam2*fam2
    + bfam3*fam3
    + bfam4*fam4
    + bWhite*white
    + bSiblings*siblings
    + bGrade_cat*grade_cat
    + bRURlarge*RURlarge
    + bRURsmall*RURanysmall,
    a_school[SchoolID] ~ dnorm(0,sigma_school),
    a_region[region] ~ dnorm(0,sigma_region),
    c(a,bTeam,bObese,bTeamObeseInt,bOver,bTeamOverInt,bThin,bTeamThin) ~ dnorm(0,10),
    c(bSESgreat,bSESgood,bSESfair,bSESbad,bfam2,bfam3,bfam4,bWhite,bSiblings,bRURlarge,bRURsmall,bGrade_cat) ~ dnorm(0,10),
    sigma_school ~ dcauchy(0,3),
    sigma_region ~ dcauchy(0,3)
  ), 
  data = dg2,cores = 4)

plot(precis(m2.3))

#Check Markov Chain convergence (Currently some unhealthy behavior in sigma_school)

plot(m2.3, pars = c("bTeam","bSiblings","bObese","bOver",
                    "bSESbad","bSESfair","bSESgood","bSESgreat","bfam2","bfam3","bfam4",
                    "bRURlarge","bRURsmall","bGrade_cat","sigma_school","sigma_region"),max_rows = 6)


post <- extract.samples(m2.3)

post$pred_norm_team <- post$bTeam
post$pred_over_team <- post$bTeam + post$bTeamOverInt + post$bOver
post$pred_obese_team <- post$bTeam + post$bTeamObeseInt + post$bObese
post$pred_thin_team <- post$bTeam + post$bTeamThin + post$bThin
post$pred_over_none <- post$bOver
post$pred_obese_none <- post$bObese
post$pred_thin_none <- post$bThin


dens(post$sigma_school, xlim = c(-1.5,1.5),ylim=c(0,8),col="#000000", xlab = "sigma")
dens(post$sigma_region, add=TRUE,col="#FF0000")

dens(post$pred_norm_team, xlim = c(-1.5,1.5),ylim=c(0,8),col="#000000", xlab = "beta")
dens(post$pred_thin_team, add=TRUE,col="#FF0000")
dens(post$pred_over_team, add=TRUE,col="#FF0000")
dens(post$pred_obese_team, add=TRUE,col="#FF0000")

dens(post$pred_thin_none, xlim = c(-1.5,1.5),ylim=c(0,8),col="#FF0000", xlab = "beta")
dens(post$pred_over_none, add=TRUE,col="#FF0000")
dens(post$pred_obese_none, add=TRUE,col="#FF0000")
abline(v=0,col="#000000", add=TRUE)

rows <- c("Thin BMI","Normal BMI","Overweight BMI","Obese BMI")
columns <- c("No Sport","Low","High","Team Sport","TLow","THigh")

results <- as.data.frame(exp(rbind(
  c(mean(post$pred_thin_none),HPDI(post$pred_thin_none)),
  c(0,0,0),
  c(mean(post$pred_over_none),HPDI(post$pred_over_none)),
  c(mean(post$pred_obese_none),HPDI(post$pred_obese_none)),
  c(mean(post$pred_thin_team),HPDI(post$pred_thin_team)),
  c(mean(post$pred_norm_team),HPDI(post$pred_norm_team)), 
  c(mean(post$pred_over_team),HPDI(post$pred_over_team)), 
  c(mean(post$pred_obese_team),HPDI(post$pred_obese_team))
)))

