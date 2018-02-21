#m2.1 adds a third level (Region) and collapses rurality to allow hopefully faster convergence (~30 min). 
#Hopefully better Markov Chain behavior as well?

library(rethinking)

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
    a ~ dnorm(0,10),
    bTeam ~ dnorm(0,1),
    bInd ~ dnorm(0,1),
    bSportInt ~ dnorm(0,1),
    bObese ~ dnorm(0,1),
    bTeamObeseInt ~ dnorm(0,1),
    bIndObeseInt ~ dnorm(0,1),
    bSportObeseInt ~ dnorm(0,1),
    bOver ~ dnorm(0,1),
    bTeamOverInt ~ dnorm(0,1),
    bIndOverInt ~ dnorm(0,1),
    bSportOverInt ~ dnorm(0,1),
    bSESgreat ~ dnorm(0,1),
    bSESgood ~ dnorm(0,1),
    bSESfair ~ dnorm(0,1),
    bSESbad ~ dnorm(0,1),
    bfam2 ~ dnorm(0,1),
    bfam3 ~ dnorm(0,1),
    bfam4 ~ dnorm(0,1),
    bWhite ~ dnorm(0,1),
    bSiblings ~ dnorm(0,1),
    bRURlarge ~ dnorm(0,1),
    bRURsmall ~ dnorm(0,1),
    bGrade_cat ~ dnorm(0,1),
    sigma_school ~ dnorm(0.5,0.2),
    sigma_region ~ dnorm(0.5,0.2)
  ), 
  data = db2)

precis(m3.0)

post <- extract.samples(m2.1)

dens(post$sigma_school)
dens(post$sigma_region, add = TRUE, col = "#00FF00")




#glmer(harddrug ~ team*ind*obese + team*ind*overweight + SESgood 
 #  + SESgreat + SESfair + SESbad + fam2 + fam3 + fam4 + white + siblings + RURlarge + RURsmall + grade_cat + (1|SchoolID) + (1|region),
  # family = binomial(link = logit),data = db2)
