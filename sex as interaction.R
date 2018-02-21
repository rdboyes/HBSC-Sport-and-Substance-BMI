#Combined Model. Runtime 10885 seconds

m3.1 <- map2stan(
  alist(
    harddrug ~ dbinom(1,p),
    logit(p) <- a 
    + bSex*sex
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
    + bTeamSex*team*sex
    + bIndSex*ind*sex
    + bSportSex*team*ind*sex
    + bObeseSex*obese*sex
    + bTeamObeseSex*team*obese*sex 
    + bIndObeseSex*ind*obese*sex
    + bSportObeseSex*team*ind*obese*sex
    + bOverSex*overweight*sex
    + bTeamOverSex*team*overweight*sex 
    + bIndOverSex*ind*overweight*sex
    + bSportOverSex*team*ind*overweight*sex
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
    + bRURsmall*RURanysmall
    + bSESgoodSex*SESgood*sex
    + bSESgreatSex*SESgreat*sex
    + bSESfairSex*SESfair*sex
    + bSESbadSex*SESbad*sex
    + bfam2Sex*fam2*sex
    + bfam3Sex*fam3*sex
    + bfam4Sex*fam4*sex
    + bWhiteSex*white*sex
    + bSiblingsSex*siblings*sex
    + bGrade_catSex*grade_cat*sex
    + bRURlargeSex*RURlarge*sex
    + bRURsmallSex*RURanysmall*sex,
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
    sigma_region ~ dnorm(0.5,0.2),
    bTeamSex ~ dnorm(0,1),
    bIndSex ~ dnorm(0,1),
    bSportSex ~ dnorm(0,1),
    bObeseSex ~ dnorm(0,1),
    bTeamObeseSex ~ dnorm(0,1),
    bIndObeseSex ~ dnorm(0,1),
    bSportObeseSex ~ dnorm(0,1),
    bOverSex ~ dnorm(0,1),
    bTeamOverSex ~ dnorm(0,1),
    bIndOverSex ~ dnorm(0,1),
    bSportOverSex ~ dnorm(0,1),
    bSESgreatSex ~ dnorm(0,1),
    bSESgoodSex ~ dnorm(0,1),
    bSESfairSex ~ dnorm(0,1),
    bSESbadSex ~ dnorm(0,1),
    bfam2Sex ~ dnorm(0,1),
    bfam3Sex ~ dnorm(0,1),
    bfam4Sex ~ dnorm(0,1),
    bWhiteSex ~ dnorm(0,1),
    bSiblingsSex ~ dnorm(0,1),
    bRURlargeSex ~ dnorm(0,1),
    bRURsmallSex ~ dnorm(0,1),
    bGrade_catSex ~ dnorm(0,1),
    bSex ~ dnorm(0,1)
  ), 
  data = d2)

precis(m3.0)

plot(m3.0)

plot(precis(m3.0))

post <- extract.samples(m3.0)

dens(post$sigma_school)
dens(post$sigma_region, add = TRUE, col = "#00FF00")

report_ci <- function(team = 0,ind = 0,overweight = 0,obese = 0,sex = 0){
  sex <- 1 - sex
  m <- post$bSex*sex + post$bTeam*team+ post$bInd*ind+ post$bSportInt*team*ind+ post$bObese*obese  + post$bTeamObeseInt*team*obese + post$bIndObeseInt*ind*obese + post$bSportObeseInt*team*ind*obese+ post$bOver*overweight + post$bTeamOverInt*team*overweight  + post$bIndOverInt*ind*overweight  + post$bSportOverInt*team*ind*overweight + post$bTeamSex*team*sex  + post$bIndSex*ind*sex+ post$bSportSex*team*ind*sex+ post$bObeseSex*obese*sex+ post$bTeamObeseSex*team*obese*sex  + post$bIndObeseSex*ind*obese*sex+ post$bSportObeseSex*team*ind*obese*sex + post$bOverSex*overweight*sex  + post$bTeamOverSex*team*overweight*sex  + post$bIndOverSex*ind*overweight*sex + post$bSportOverSex*team*ind*overweight*sex
  return(m)
}
  
dens(report_ci(team = 1, ind = 0, sex = 0))
dens(report_ci(team = 1, ind = 0, sex = 0, overweight = 1),add = TRUE,col="#FF0000")
dens(report_ci(team = 1, ind = 0, sex = 0, obese = 1),add = TRUE,col="#0000FF")

exp(mean(report_ci(team = 1, ind = 0, sex = 1)))
exp(mean(report_ci(team = 1, ind = 0, sex = 1, overweight = 1)))
exp(mean(report_ci(team = 1, ind = 0, sex = 1, obese = 1)))

exp(HPDI(report_ci(team = 1, ind = 0, sex = 1)))
exp(HPDI(report_ci(team = 1, ind = 0, sex = 1, overweight = 1)))
exp(HPDI(report_ci(team = 1, ind = 0, sex = 1, obese = 1)))
