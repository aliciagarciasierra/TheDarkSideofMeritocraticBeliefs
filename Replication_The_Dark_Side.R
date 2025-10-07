

######################################################################
######## The Dark Side of Meritocratic Beliefs: ######################
######## Is Believing in Meritocracy Detrimental to Individuals ######
######## from Low Socioeconomic Backgrounds? #########################
######################################################################

# Authors: Alicia García-Sierra 

####################################################
########### PREPARE ENVIRONMENT ##################
#################################################

rm( list=ls( all= TRUE )) 

library(haven) # uploading needed packages
library(car)
library(foreign)
library(MASS)
library(tidyverse)
library(ordinal)
library(ggeffects)
library(effects)
library(pastecs)
library(ggplot2)
library(stargazer)
library(survival)
library(fixest)
library(prediction)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/PAPER MERITOCRATIC/2022/Data")

################################################################################
################################## GENERATING THE DATASET ######################
################################################################################

#We have to merge several data sets from different modules of the SOEP-Core data base. 

#1) Youth questionnaire: data of the teenagers at age 17.

jugendl <- read_dta("jugendl.dta")
jugendl$ID<-jugendl$pid

#2) We merge the youth questionnaire with the parental information ("biopare").

bioparen <- read_sav("bioparen.sav")
bioparen$ID<-bioparen$persnr # generate a new "ID" variable to avoid changing the original "pid" or "persnr". 
dat<- merge(bioparen, jugendl, by = "ID")

# Siblings

biosib <- read_sav("biosib.sav")
biosib$ID<-biosib$persnr

dat<- merge(dat, biosib, by = "ID")

#4) We extract the information about the educational outcomes of the individuals when they are adults (t+1) from "pgen". 

pgen<-read_sav("pgen.sav")
bjpgen<-read_sav("bjpgen.sav")

#table(pgen$pgbilzeit) # educational variable
#df.agg<-aggregate( pgbilzeit~ ID, pgen, max)
#dat<-merge(dat, df.agg, by="ID")
#table(pgen$pgemplst) # employment variable

pgen$ID<-pgen$pid

myvars<-c("ID","pgbilzeit", "pgfield", "pgpsbil",
"pgisced11", "pgisced97", "pgdegree",
 "pgisco88", "pgisco08","pgausb",
 "pgegp88", "pgegp08", "pgerljob", "pgemplst", "pglfs") 
pgen<-pgen[myvars]


# 5) Merge all the databases

data<-merge(pgen, dat, by="ID")
ID<-data$ID

library(dplyr)
date<-data %>%   # selecting the year with last syear
  group_by(ID) %>%
  slice(which.max(syear))
data<-date


###############################################################  
###################### DEPENDENT VARIABLE ##################### 
############################################################### 

myvars<-c("ID","pgbilzeit", "pgfield", "pgerljob", "pgpsbil",
          "pgisced11", "pgisced97", "pgdegree",
          "pgisco88", "pgisco08","pgausb",
          "pgegp08", "pgegp88", "pgegp08") 
#only operationalized hasta pgpsbil

# Years of education #
table(data$pgbilzeit)
data$yearsedu<-data$pgbilzeit
data$yearsedu[data$yearsedu<= 0]<-NA
table(data$yearsedu)

# Field of study #
table(data$pgfield)
data$stem<-data$pgfield
data$stem[data$stem<=0]<-NA
data$stem[data$stem==1]<-78
data$stem[data$stem>=36 & data$stem<=48 | data$stem==57 | data$stem==58 | data$stem>=63 & data$stem<=68]<-1 # stem
data$stem[data$stem!=1]<-0 # non-stem
table(data$stem)

# School leaving degree #
table(data$pgpsbil)
data$abitur<-data$pgpsbil
data$abitur[data$abitur==1 | data$abitur==2 | data$abitur==3 | data$abitur==5 |  data$abitur==6 |  data$abitur==7 |  data$abitur==8 ]<-0
data$abitur[data$abitur==4]<-1 #abitur
data$abitur[data$abitur==-2 | data$abitur==-1]<-NA #abitur
table(data$abitur)

data$nodegree<-data$pgpsbil
data$nodegree[data$nodegree==1 | data$nodegree==2 | data$nodegree==3 | data$nodegree==4 | data$nodegree==5 ]<-0
data$nodegree[data$nodegree==6 | data$nodegree==7 | data$nodegree==8]<-1
data$nodegree[data$nodegree==-2 | data$nodegree==-1 ]<-NA
table(data$nodegree)

#Trained for#
table(data$pgerljob)
data$trained<-data$pgerljob
data$trained[data$trained==3 | data$trained==4 | data$trained==-1 | data$trained==-2]<-NA
data$trained[data$trained==2]<-0 # not for what you're trained
table(data$trained)

#Precarious#
table(data$pgemplst)
data$precar<-data$pgemplst
data$precar[data$precar<=0| data$precar==6]<-NA
data$precar[data$precar==1 | data$precar==3]<-0
data$precar[data$precar==2 | data$precar==4  | data$precar==5]<-1
table(data$precar)

table(data$pgemplst)
data$full<-data$pgemplst
data$full[data$full==1]<-1
data$full[data$full==2 | data$full==3 | data$full==4 |data$full==5 | data$full==6]<-NA
data$full[data$full==-1 | data$full==-2 | data$full==-3]<-NA

     
table(data$pgemplst)
data$partime<-data$pgemplst
data$partime[data$partime==1 | data$partime==3 | data$partime==5 | data$partime==6 ]<-0
data$partime[data$partime==2 | data$partime==4]<-1
data$partime[data$partime==-1 | data$partime==-2 | data$partime==-5]<-NA
table(data$partime)

table(data$pglfs)
data$pglfs[data$pglfs==-2]<-NA
data$fullyworking<-data$pglfs
data$fullyworking[data$fullyworking!=11]<-0
data$fullyworking[data$fullyworking==11]<-1
table(data$fullyworking)

data$fullystudy<-data$pglfs
data$fullystudy[data$fullystudy!=3]<-0
data$fullystudy[data$fullystudy==3]<-1
table(data$fullystudy)

data$secjob<-data$pglfs
data$secjob[data$secjob!=8 | data$secjob!=10 | data$secjob!=13]<-0
data$secjob[data$secjob==8 | data$secjob==10 | data$secjob==13]<-1
table(data$secjob)


###############################################################  
###################### INDEPENDENT VARIABLES ################## 
############################################################### 


############ MERITOCRATIC BELIEFS: "One Has To Work Had To Be Successful" ########

#Dichotomous variable, period before 2006. 1 is very strong agree, 2 agree, 3 disagree, 4 very much disagree.
table(data$jl0354_v1) 
data$meri1<-data$jl0354_v1
data$meri1[data$meri1==-8 | data$meri1==-1]<-NA
data$meri1[data$meri1==3 | data$meri1==4]<-0 # Non-meritocratic belief, disagree with the statement that one has to work to be succesful.
data$meri1[data$meri1==1 | data$meri1==2]<-1 # Meritocratic belief.
table(data$meri1)

#Scale variable, period from 2006 onward. 1 disagree to 7 maximum agree
data$meri2<-data$jl0354_v2
data$meri2[data$meri2==-8 | data$meri2==-1]<-NA
table(data$meri2)
summary(data$meri2)#26,3% missingness
sd(data$meri2, na.rm=T)

#Combined variable - two periods
data$merit<-0
data$merit[data$jl0354_v1==1 | data$jl0354_v1==2 | data$jl0354_v2>=5.819 ]<-1 # if strongly agree or agree in the 4 categories variable, before 2006, and if bigger than the mean value (5.8 post deleting negative missing values) in the scale one, from 2006 to 2019.
table(data$merit)
summary(data$merit)

##################### PARENTAL EDUCATION ############################

# FATHERS

table(data$fsedu)
data$fedu<-data$fsedu

data$fedu[data$fedu==-5]<-NA
data$fedu[data$fedu==-1]<-NA
data$fedu[data$fedu==-2]<-NA
data$fedu[data$fsedu==0]<-NA

data$fedu[data$fsedu==6]<-0 #no school
data$fedu[data$fsedu==2 | data$fsedu==1 ]<-1 #general secondary
data$fedu[data$fsedu==3]<-2 #vocational
data$fedu[data$fsedu==4 | data$fsedu==5 ]<-3 #tertiary education

# MOTHERS 

data$medu<-data$msedu
data$medu[data$medu==-5]<-NA
data$medu[data$medu==-1]<-NA
data$medu[data$msedu==0]<-NA

data$medu[data$msedu==6]<-0 #no school 
data$medu[data$msedu==2 | data$msedu==1 ]<-1 # general secondary
data$medu[data$msedu==3]<-2 #vocational
data$medu[data$msedu==4 | data$msedu==5 ]<-3 # tertiary education


# DOMINANCE MODELS 

data$pedu <- ifelse(data$fedu>data$medu, data$fedu, data$medu) # the higher the number the better, since tertiary education is 3
table(data$pedu)


# DICHOTOMOUS VERSIONS

data$pedud<-data$pedu
data$pedud[data$pedu==0| data$pedu==1 | data$pedu==2]<-0
data$pedud[data$pedu==3]<-1 

data$medud<-data$medu
data$medud[data$medu==0 | data$medu==1| data$medu==2]<-0
data$medud[data$medu==3]<-1 

data$fedud<-data$fedu
data$fedud[data$fedu==0 | data$fedu==1| data$fedu==2]<-0
data$fedud[data$fedu==3]<-1 

# LABELLING

table(data$fedu) 
data$fedu<-as.factor(data$fedu)
data$fedu<-factor(data$fedu,
                  levels=c(0,1, 2, 3),
                  labels=c("No School", "Secondary",  "Vocational", "Tertiary"))
table(data$fedu)

data$fedud<-factor(data$fedud,
                   levels=c(0,1),
                   labels=c("Non-Tertiary", "Tertiary"))
table(data$fedud)

table(data$medu) 
data$medu<-factor(data$medu,
                  levels=c(0,1,2, 3),
                  labels=c("No School", "Secondary",  "Vocational", "Tertiary"))
table(data$medu)

data$medud<-factor(data$medud,
                   levels=c(0,1),
                   labels=c("Non-Tertiary", "Tertiary"))
table(data$medud)

table(data$pedu) 
data$pedu<-as.factor(data$pedu)
data$pedu<-factor(data$pedu,
                  levels=c(0,1,2, 3),
                  labels=c("No School", "Secondary",  "Vocational", "Tertiary"))
table(data$pedu)

data$pedud<-factor(data$pedud,
                  levels=c(0,1),
                  labels=c("Non-Tertiary", "Tertiary"))
table(data$pedud)


##################### PARENTAL OCCUPATION ############################

# FATHERS

table(data$fegp88)
data$egp<-data$fegp88
data$egp[data$fegp88==-2 | data$fegp88==-1]<-NA
data$egp[data$fegp88==1 | data$fegp88==2  ]<-0 #service
data$egp[data$fegp88== 3 | data$fegp88== 4 | data$fegp88== 5 | data$fegp88== 6 | data$fegp88== 11 | data$fegp88== 7] <- 1 #intermediate
data$egp[data$fegp88== 8 |data$fegp88== 9 | data$fegp88== 10 | data$fegp88== 11] <-2 #working class
table(data$egp)

#Dichot: service vs non service
data$egpd <- data$egp
data$egpd [data$egpd==0]<-3
data$egpd[data$egp==2 |data$egp==1 ]<-0 #no service
data$egpd [data$egpd==3]<-1 # service
table(data$egpd)

# MOTHERS

table(data$megp88)
data$megp<-data$megp88
data$megp[data$megp88==-2 | data$megp88==-1]<-NA
data$megp[data$megp88==1 | data$megp88==2]<-0 #service
data$megp[data$megp88== 3 | data$megp88== 4 | data$megp88== 5 | data$megp88== 6 | data$megp88== 11 | data$megp88== 7] <- 1 #intermediate
data$megp[data$megp88== 8 |data$megp88== 9 | data$megp88== 10 | data$megp88==11] <-2 #working class
table(data$megp)

#Dichot
data$megpd <- data$megp
data$megpd [data$megpd==0]<-3
data$megpd[data$megp==2 |data$megp==1 ]<-0 #no service
data$megpd [data$megpd==3]<-1 # service
table(data$megpd)

# DOMINANCE MODEL

data$pegp<- ifelse(data$egp<data$megp, data$egp, data$megp) # the lowest the number the better, since service class is 0
table(data$pegp)

#Dichot: service vs non service
data$pegpd <- data$pegp
data$pegpd[data$pegpd==0]<-3
data$pegpd[data$pegpd==2 |data$pegpd==1 ]<-0 #no service
data$pegpd[data$pegpd==3]<-1 # service
table(data$pegpd)

# LABELLING

data$egp<-factor(data$egp,
                 levels=c(0, 1, 2),
                 labels=c("Service", "Intermmediate", "Working"))
table(data$egp)
data$egpd<-factor(data$egpd,
                   levels=c(0, 1),
                   labels=c( "Non Service", "Service"))
table(data$egpd)

data$megp<-factor(data$megp,
                  levels=c(0, 1, 2),
                  labels=c("Service", "Intermmediate", "Working"))
table(data$megp)

data$megpd<-factor(data$megpd,
                  levels=c(0, 1),
                  labels=c( "Non Service", "Service"))
table(data$megpd)

data$pegp<-factor(data$pegp,
                  levels=c(0, 1, 2),
                  labels=c("Service", "Intermmediate", "Working"))
table(data$pegp)

data$pegpd<-factor(data$pegpd,
                   levels=c(0, 1),
                   labels=c( "Non Service", "Service"))
table(data$pegpd)

###############################################################  
###################### CONTROL VARIABLES ###################### 
############################################################### 

# EDUCATIONAL EXPECTATIONS: "Probability in % of being favoured by an apprenticeship or university place in the future". Higher values more trust on education.

table(data$jl0221)
data$edexp<-data$jl0221
data$edexp[data$jl0221==-5 | data$jl0221==-1]<-NA
data$edexp<-as.numeric(data$edexp)
summary(data$edexp) 
sd(data$edexp, na.rm=T)
# LOCUS OF CONTROL: Combined variable - two periods.

data$locus<-0
data$locus[data$jl0353_v1==3 | data$jl0353_v1==4 | data$jl0353_v2<=2.865 ]<-1 # higher control over one's life (those who disagree the most with the statement of other people having control over their lives)
table(data$locus)

#SELF-EFFICACY: carryout duties efficiently

table(data$jl0375) 
data$effi<-data$jl0375
data$effi[data$jl0375==-8 | data$jl0375==-1] <-NA
summary(data$effi)
sd(data$effi, na.rm=T)
data$effi<-as.numeric(data$effi) 

# GERMAN GRADES 

table(data$jl0152)
data$jl0152[data$jl0152==-5]<-NA
data$jl0152[data$jl0152==-2]<-NA
data$jl0152[data$jl0152==-1]<-NA
data$german<-(data$jl0152-7)*-1 # reverse the original variable: now, higher values better grades
data$german<-as.numeric(data$german) 
table(data$german)
sd(data$german, na.rm=T)

#MATHS GRADES 

table(data$jl0156)
data$jl0156[data$jl0156==-5]<-NA
data$jl0156[data$jl0156==-1]<-NA
data$jl0156[data$jl0156==-2]<-NA
data$maths<-(data$jl0156-7)*-1
data$maths<-as.numeric(data$maths)
summary(data$maths)
sd(data$maths, na.rm=T)

means<-data.frame(data$german, data$maths)
data$grades<-rowMeans(means, na.rm = T)
summary(data$grades)
sd(data$grades, na.rm=T)

data$highgrades<-3
data$highgrades[data$jl0152<=4 | data$jl0156<=4]<-1 #high grades
data$highgrades[data$jl0152>=4 | data$jl0156>=4]<-0 #low grades
data$highgrades[data$highgrades==3]<-NA
table(data$highgrades)


#SATISFRACTION WITH GRADES#

table(data$jl0147)
data$satisfaction<-data$jl0147
data$satisfaction[data$satisfaction==-5 | data$satisfaction==-1]<-NA
table(data$satisfaction)

#ABILITY IN GERMAN#

table(data$jl0248)
data$oral<-data$jl0248
data$oral[data$oral<=0]<-NA
table(data$oral)

table(data$jl0249)
data$written<-data$jl0249
data$written[data$written<=0]<-NA
table(data$written)

means<-data.frame(data$written, data$oral)
data$ability<-rowMeans(means, na.rm = T)
table(data$ability)

# AGE CONTINOUS 

table(data$jl0233)
data$age<-(2022-data$jl0233)
table(data$age)
summary(data$age)
sd(data$age, na.rm=T)
# AGE CATEGORICAL

data$agec<-data$age
data$agec[data$age<=22]<-0 #still in college
data$agec[data$age>=23 & data$age<=26]<-1 # non compulsory education
data$agec[data$age>=27]<-2 # labor market
data$agec<-as.factor(data$agec)
data$agec<-factor(data$agec,
                  levels=c(0, 1, 2),
                  labels=c("Less than 22", "Between 22 and 26", "More than 26"))
table(data$agec)


# GENDER

data$female<-0
data$female[data$sex==1]<-0 #male
data$female[data$sex==2]<-1 #female
table(data$female)

# FAMILY IDENTIFIICATOR
summary(data$cid)

################################################################################
###################################  EXTRACTING THE SAMPLE #####################
################################################################################

save(data, file="dataanalyses.RData")

#################################################
############### ANALYSES ########################
#################################################

rm( list=ls( all= TRUE )) 
library(fixest)
library(ggiplot)

setwd("~/Documents/PAPER MERITOCRATIC/2022/Data")

load("dataanalyses.RData")

data$

#EXPLORE MISSING PATTERNS#
myvars<-c( "meri2", "merit", 
           "precar","fullyworking",
         "egpd",
           "edexp", "locus", "effi", 
           "cid", "pid")
ver<-data[myvars] 
(colMeans(is.na(ver)))*100 # to check missings in dataframe

#RESTRICT THE DATA TO THOSE WITH ONLY SIBLINGS#
siblings<-data[ data$cid %in%  names(table(data$cid))[table(data$cid) >=2] , ]

# Seeing how many siblings per family
library(plyr)
y<-count(siblings, 'cid')
table(y$freq)

########################### DESCRIPTIVES #####################

#### GRAPH WITH DISTRIBUTIONS ###

data$egpd<-factor(data$egpd,
                  levels=c("Non Service", "Service"),
                  labels=c("Low SES", "High SES"))

p <- ggplot(data=subset(data, !is.na(data$egpd)), aes(x=meri2, group=egpd, colour=egpd)) +
  geom_density(adjust=1.5, alpha=.4) +
  xlab("Distribution") +
  ylab("Kernel Density") + labs(color = "SES")
p<-p+theme_bw()
p <- p + ggtitle("Meritocratic Beliefs by SES")
p<- p + theme(plot.title = element_text(hjust=0.5,size=13, face="bold"))
p <- p + theme(axis.title.y = element_text(size = 8, angle = 90))
p <- p + theme(axis.title.x = element_text(size = 8, angle = 00))
p<-p +  xlim(1,7) 
p<- p + theme(legend.position="bottom")
p<- p + scale_color_manual(values=c("darkorange3", "skyblue2"))
p

##### Mean by groups #####

aggregate(data$meri2, list(data$egpd), FUN=mean, na.rm=T) 
summary(data$meri2)

table1<-table(data$precar, data$egpd)
prop.table(table1, 2)

table2<-table(data$fullyworking, data$egpd)
prop.table(table2,2)

########  ICCs #########

# Reshaping Data #
#siblings$sib <- ave(siblings$pid,                 # Create identificator for each sibling within family
                #  siblings$cid,
               #    FUN = seq_along)

# write_dta(siblings, "siblings.dta") 

split<-split(siblings, f=siblings$egpd)
high<-split$Service
low<-split$`Non Service`

wide1<-reshape(as.data.frame(high),direction="wide", idvar="cid", timevar="sib")
wide2<-reshape(as.data.frame(low),direction="wide", idvar="cid", timevar="sib")


library(haven)
wide<- read_dta("~/Dropbox/PAPER MERITOCRATIC/2022/Data/wide.dta")
library("irr")

myvars<-c("fullyworking.1", "fullyworking.2")
date<-wide1[myvars]
icc(
  date, model = "oneway", 
  type= "consistency", unit = "single"
)

myvars<-c("fullyworking.1", "fullyworking.2")
date<-wide2[myvars]
icc(
  date, model = "oneway", 
  type= "consistency", unit = "single"
)
############################# FIXED EFFECTS MODELS ##############

######### PRECARIOUS WORK ###########
myvars<-c( "meri2", "merit", 
           "precar",
            "egpd",
           "edexp", "locus", "effi", 
           "age", "agec", "female",
           "cid", "pid")
sib<-siblings[myvars] 
sib<-na.omit(sib)


fest1<- feglm(precar~meri2*egpd| cid,sib)
summary(fest1)
fest2<- feglm(precar~meri2*egpd+ age + agec + female | cid  ,sib)
summary(fest2)
fest3<- feglm(precar~meri2*egpd+ age + agec + female + locus | cid  ,sib)
summary(fest3)
fest4<- feglm(precar~meri2*egpd+ age + agec + female + locus+ effi| cid  ,sib)
summary(fest4)

#Without fixed effects
fest4<- glm(precar~meri2,sib, family="binomial")
summary(fest4)
fest4<- glm(precar~meri2*egpd+ age + agec + female + locus+ effi,sib, family="binomial")
summary(fest4)

coefplot(list(fest1, fest2, fest3, fest4),
               drop = "!meri2",
               main="Precarious Work Situation")



###### FULLY WORKING STATUS ##########

myvars<-c( "meri2", "merit", 
           "fullyworking",
           "egpd",
           "edexp", "locus", "effi", 
           "age", "agec", "female",
           "cid", "pid")
sib<-siblings[myvars] 
sib<-na.omit(sib)

fest0<- feglm(fullyworking~meri2| cid,sib)
summary(fest0)
fest0<- feglm(fullyworking~egpd| cid,sib)
summary(fest0)


fest6<- feglm(fullyworking~meri2*egpd| cid , sib)
summary(fest6)
fest7<- feglm(fullyworking~meri2*egpd+ age + agec+ female| cid , sib)
summary(fest7, se="cluster")
fest8<- feglm(fullyworking~meri2*egpd+ age + agec+ female + locus | cid , sib)
summary(fest8, se="cluster")
fest9<- feglm(fullyworking~meri2*egpd+ age + agec+ female + locus + effi | cid , sib)
summary(fest9, se="cluster")

#Withoutfixedeffects

fest9<- glm(fullyworking~meri2*egpd+ age + agec + female + locus+ effi,sib, family="binomial")
summary(fest9)

fest9<- feglm(fullyworking~meri2*egpd+ age + agec+ female + locus + effi , sib)
summary(fest9)
etable(fest9)

coefplot(list(fest6, fest7, fest8, fest9),
         drop = "!meri2",
         main="Fully Working Status")

##################  ROBUSTNESS ################


# 1) CONTROLLING FOR ABILITY
myvars<-c( "meri2", "merit", 
           "precar",
           "egpd",
           "edexp", "locus", "effi", 
           "age", "agec", "female",
           "maths", "german",
           "cid", "pid")
sib<-siblings[myvars] 
sib<-na.omit(sib)
fest10<- feglm(precar~meri2*egpd+ age + agec + female + locus+ effi+maths+german| cid, sib)
summary(fest10)

myvars<-c( "meri2", "merit", 
           "fullyworking", 
           "egpd",
           "edexp", "locus", "effi", 
           "age", "agec", "female",
           "maths", "german",
           "cid", "pid")
sib<-siblings[myvars] 
sib<-na.omit(sib)
fest11<- feglm(fullyworking~meri2*egpd+ age + agec+ female + locus + effi + maths + german| cid , sib)
summary(fest11, se="cluster")

cogdj <- read_sav("cogdj.sav")
cogdj$ID<-cogdj$persnr # generate a new "ID" variable to avoid changing the original "pid" or "persnr". 
date<- merge(siblings, cogdj, by = "ID")
date$sumindex[date$sumindex<0]<-NA
fest12<- feglm(precar~meri2*egpd+ age + agec+ female + locus + effi + sumindex| cid.x , date)
summary(fest12, se="cluster")
fest13<- feglm(fullyworking~meri2*egpd+ age + agec+ female + locus + effi + sumindex| cid.x , date)
summary(fest13, se="cluster")

# 2) DICHOTOMOUS MERITOCRATIC BELIEFS

fest1<- feglm(precar~merit*egpd| cid,siblings)
summary(fest1)
fest2<- feglm(precar~merit*egpd+ age + agec + female | cid  ,siblings)
summary(fest2)
fest3<- feglm(precar~merit*egpd+ age + agec + female + locus | cid  ,siblings)
summary(fest3)
fest4<- feglm(precar~merit*egpd+ age + agec + female + locus+ effi| cid  ,siblings)
summary(fest4)
fest5<- feglm(precar~merit*egpd+ age + agec + female + locus+ effi+grades| cid  ,siblings)
summary(fest5)
etable(fest1, se="cluster", digits="r3")

fest6<- feglm(fullyworking~merit*egpd| cid , siblings)
summary(fest6)
fest7<- feglm(fullyworking~merit*egpd+ age + agec+ female| cid , siblings)
summary(fest7, se="cluster")
fest8<- feglm(fullyworking~merit*egpd+ age + agec+ female + locus | cid , siblings)
summary(fest8, se="cluster")
fest9<- feglm(fullyworking~merit*egpd+ age + agec+ female + locus + effi | cid , siblings)
summary(fest9, se="cluster")
fest10<- feglm(fullyworking~merit*egpd+ age + agec+ female + locus + effi + grades | cid , siblings)
summary(fest10, se="cluster")

# 3) DOMINANCE MODEL

######### PRECARIOUS WORK ###########
myvars<-c( "meri2", "merit", 
           "precar",
           "pegpd",
           "edexp", "locus", "effi", 
           "age", "agec", "female",
           "cid", "pid")
sib<-siblings[myvars] 
sib<-na.omit(sib)

fest12<- feglm(precar~meri2*pegpd+ age + agec + female + locus+ effi| cid  ,sib)
summary(fest12)

###### FULLY WORKING STATUS ##########

myvars<-c( "meri2", "merit", 
           "fullyworking",
           "pegpd",
           "edexp", "locus", "effi", 
           "age", "agec", "female",
           "cid", "pid", "syear")
sib<-siblings[myvars] 
sib<-na.omit(sib)

fest13<- feglm(fullyworking~meri2*pegpd+ age + agec+ female + locus + effi| cid , sib)
summary(fest13, se="cluster")

coefplot(list(fest12,fest13),
         drop = "!meri2",
         main="Fully Working Status")


# 4) PARENTAL EDUCATION 

######### PRECARIOUS WORK ###########
myvars<-c( "meri2", "merit", 
           "precar",
           "fedud",
           "edexp", "locus", "effi", 
           "age", "agec", "female",
           "cid", "pid")
sib<-siblings[myvars] 
sib<-na.omit(sib)

fest12<- feglm(precar~meri2*fedud+ age + agec + female + locus+ effi| cid  ,sib)
summary(fest12)

###### FULLY WORKING STATUS ##########

myvars<-c( "meri2", "merit", 
           "fullyworking",
           "fedud",
           "edexp", "locus", "effi", 
           "age", "agec", "female",
           "cid", "pid", "syear")
sib<-siblings[myvars] 
sib<-na.omit(sib)

fest13<- feglm(fullyworking~meri2*fedud+ age + agec+ female + locus + effi| cid , sib)
summary(fest13, se="cluster")

coefplot(list(fest12,fest13),
         drop = "!meri2",
         main="Fully Working Status")

############ AGE STRATIFICATION  ###########
table(data$age)
data$mean<-data$age
data$mean[data$mean<=28]<-0
data$mean[data$mean>28]<-1
table(data$mean)
split<-split(data, f=data$mean)
young<-split$`0`
old<-split$`1`

fest1<- feglm(precar~meri2*egpd| cid,young)
summary(fest1)

fest1a<- feglm(precar~meri2*egpd| cid,old)
summary(fest1a)

fest2<- feglm(fullyworking~meri2*egpd| cid,young)
summary(fest2)

fest2a<- feglm(fullyworking~meri2*egpd| cid,old)
summary(fest2a)

split<-split(data, f=data$fullyworking)
full<-split$`1`
nofull<-split$`0`

mean(full$age)
mean(nofull$age)
        
############ GRADES AT AGE 9 ###########
bioparen <- read_sav("bioagel.sav")
bioparen$ID<-bioparen$pid
date<- merge(bioparen, siblings, by = "ID")

table(date$lamark)
date$germ<-date$lamark
date$germ[date$germ<=0]<-NA
table(date$germ)

table(date$matmark)
date$mat<-date$matmark
date$mat[date$mat<=0]<-NA
table(date$mat)

fest14<- feols(meri2~mat+germ , date)
summary(fest14)
etable(fest14)
