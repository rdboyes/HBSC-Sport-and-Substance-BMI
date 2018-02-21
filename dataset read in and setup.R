#0.1. Required libraries

library(readr)
library(lme4)
library(dplyr)
library(gmodels)

#1.0. Read data file from CSV and convert to dataframe

HBSC_data <- read_csv("C:/Users/Student/Desktop/Data/HBSC Sport and Substance BMI/HBSC data.csv")

d <- as.data.frame(HBSC_data)
d[d == "."] <- NA

#2.0. Create 0/1 exposure variables as numeric

d$weed <- as.integer(d$weed)
d$team <- ifelse(d$`Are you involved in any of these kinds of activities or groups? | A sports team (e.g., volleyball, hockey, soccer)` == "Yes",1,0)
d$ind <- ifelse(d$`Are you involved in any of these kinds of activities or groups? | An individual sport (e.g., running, cycling, skating)` == "Yes",1,0)
d$church <- ifelse(d$`Are you involved in any of these kinds of activities or groups? | Church or other religious/spiritual group`=="Yes",1,0)
d$arts <- ifelse(d$`Are you involved in any of these kinds of activities or groups? | Arts groups (e.g., music, dance, drama)`=="Yes",1,0)
d$other <- ifelse(d$`Are you involved in any of these kinds of activities or groups? | Other activity or group (e.g., chess, math, debate)`=="Yes",1,0)
d$community <- ifelse(d$`Are you involved in any of these kinds of activities or groups? | Community groups (e.g., scouts, girl guide, 4-H, cadet`=="Yes",1,0)
d$amphetamines <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Amphetamines (speed)`=="Never",0,1)
d$mdma <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Ecstasy, MDMA (E, Xtc, Adam, X)`=="Never",0,1)
d$cocaine <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Cocaine (coke, crack, snow, rock)`=="Never",0,1)
d$heroin <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Heroin (horse, tar, junk)`=="Never",0,1)
d$glue <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Glue or solvents (e.g., gasoline, butane, mod`=="Never",0,1)
d$meth <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Methamphetamines/Crystal methamphetamine (ice`=="Never",0,1)
d$lsd <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | LSD and other hallucinogens (e.g., PCP, magic`=="Never",0,1)
d$painkillers <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Pain Relievers (e.g., Percodon, Demerol, Oxyc`=="Never",0,1)
d$sedatives <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Sedatives/tranquillizers (e.g., Valium, Ativa`=="Never",0,1)
d$stim <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Stimulants (e.g., Ritalin, Concerta, Adderall`=="Never",0,1)
d$coldmed <- ifelse(d$`Have you ever taken one or several of these drugs in the last 12 months? | Cough and cold medicines`=="Never",0,1)

d$chew <- as.integer(d$chew)
d$siblings <- as.integer(d$siblings)
d$region <- as.integer(d$`Region/Municipality`)
d$grade_cat <- if_else(d$Grade <= 9,1,0) #ref = gr. 10 or higher
d$sex <- ifelse(d$Gender=="Boy",1,0)

#3.0. Create Dummy variables for factor variables

#3.1 BMI

d$bmifactor <- as.factor(d$`bmi classification`)
d$overweight <- ifelse(d$bmifactor == "overweight",1,0)
d$obese <- ifelse(d$bmifactor == "obese",1,0)

#3.2 Family Structure

d$fam1 <- ifelse(d$family == 1,1,0)
d$fam2 <- ifelse(d$family == 2,1,0)
d$fam3 <- ifelse(d$family == 3,1,0)

cbind(d$fam1,d$fam2,d$fam3)

#3.3 Socioeconomic Status

d$SESfair <- ifelse(d$`Family well off` == "Not very well off",1,0)
d$SESav <- ifelse(d$`Family well off` == "Average",1,0)
d$SESgood <- ifelse(d$`Family well off` == "Quite well off",1,0)
d$SESgreat <- ifelse(d$`Family well off` == "Very well off",1,0)

#3.4 Ethnicity

d<-mutate(d, white <- ifelse(eth == "Caucasian",1,0))
d<-rename(d, "white" = "white <- ifelse(eth == \"Caucasian\", 1, 0)")

#3.5 Statscan Rurality

m4 <- as.data.frame(dummy(as.factor(d$`StatsCan Code`)))
colnames(m4) <- c("RURmed", "RURlarge","RURvsmall","RURsmall")
m4$RURanysmall <- 0
m4$RURanysmall <- xor(m4$RURmed,m4$RURsmall)
m4$RURanysmall <- xor(m4$RURanysmall,d$RURvsmall)
d <- cbind(d, m4)


#4.0 Drop unnecessary variables

d2 <- d[,c("team","ind","siblings","obese","overweight",
           "School","white","SESav","SESfair","SESgood","SESgreat","fam2","fam3","fam1",
           "RURmed", "RURlarge","RURvsmall","RURsmall","grade_cat","region","RURanysmall","sex",
           "church","arts","community","other","binge",
           "amphetamines","mdma","cocaine","heroin","glue","meth","lsd",
           "painkillers","sedatives","stim","coldmed")]

#5.0 Stratify by gender

dg2 <- d2[d2$Gender == "Girl",]
db2 <- d2[d2$Gender == "Boy",]

#6.0 Adds a unique conseq. numeric ID variable for each school in the data set 

db2$SchoolID <- as.numeric(factor(db2$School, levels = unique(db2$School)))
dg2$SchoolID <- as.numeric(factor(dg2$School, levels = unique(dg2$School)))
d2$SchoolID <- as.numeric(factor(d2$School, levels = unique(d2$School)))


