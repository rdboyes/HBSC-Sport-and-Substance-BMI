#0.1. Required libraries

library(readr)
library(lme4)
library(dplyr)

#1.0. Read data file from CSV and convert to dataframe

HBSC_data <- read_csv("C:/Users/Student/Desktop/Data/HBSC Sport and Substance BMI/HBSC data.csv")

d <- as.data.frame(HBSC_data)

#2.0. Create 0/1 exposure variables as numeric

d$weed <- as.integer(d$weed)
d$sports <- as.integer(d$sports)
d$team <- as.integer(d$team)
d$ind <- d$sports - 2*d$team
d$chew <- as.integer(d$chew)
d$siblings <- as.integer(d$siblings)
d$region <- as.integer(d$`Region/Municipality`)
d$grade_cat <- if_else(d$Grade <= 9,9,10)

#3.0. Create Dummy variables for factor variables (there are missing data leaks in this code, needs fixing)

#3.1 BMI

d$bmifactor <- as.factor(d$`bmi classification`)
m <- dummy(d$bmifactor)
mdf <- as.data.frame(m)
mdf <- mutate(mdf, any_thin = xor(thin, `severely thin`))
d <- cbind(d,mdf)

#3.2 Family Structure

m2 <- as.data.frame(dummy(as.factor(d$family)))
colnames(m2)<-c("fam2","fam3","fam4")
d <- cbind(d,m2)

#3.3 Socioeconomic Status

m3 <- as.data.frame(dummy(as.factor(d$`Family well off`)))
colnames(m3)<-c("SESav","SESbad","SESfair","SESgood","SESgreat")
d <- cbind(d,m3)

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

d2 <- d[,c("Gender","team","ind","harddrug","siblings","obese","overweight","any_thin",
           "School","white","SESav","SESbad","SESfair","SESgood","SESgreat","fam2","fam3","fam4",
           "RURmed", "RURlarge","RURvsmall","RURsmall","grade_cat","region","RURanysmall")]

#5.0 Stratify by gender

dg2 <- d2[d2$Gender == "Girl",]
db2 <- d2[d2$Gender == "Boy",]

#6.0 Adds a unique conseq. numeric ID variable for each school in the data set 

db2$SchoolID <- as.numeric(factor(db2$School, levels = unique(db$School)))
dg2$SchoolID <- as.numeric(factor(dg2$School, levels = unique(db$School)))

#7.0 Missing data handling - currently complete case is implemented

db2 <- na.omit(db2)
dg2 <- na.omit(dg2)


