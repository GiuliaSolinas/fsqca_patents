#### This script is used to analyze the data on patent grant achievement and fast grant achievement ####
#Sys.setenv(LANG = "en") #to avoid problems with the encoding of the text

#project QCA
#Performance measured as fastgrant achievement and patent grant achievement
#author: Giulia Solinas
#date created: 29 June 2018
# update:23 May 2019 
# update: 27-30 August 2019
# update: 1-5 September 2019
# update: 17 September 2019
# update: 24 September 2019
# update: 29 September 2019


#MAJOR NOTES
# We decide to eliminate the controlling conditions to avoid playing the game of the 
# reviewers and be subject to attacks for the types of controls that we use. 

#We decided to drop the variable based on Fast Grant Achievement based on 
#Reitzig and Puranam (2009). For this analysis, look at the old file from 2016

#The previous analysis in 2016 used _fz1 WITHOUT adjusting for 0.5 crossovers --> some cases dropped

# A1c: FHG <- org_fz+ citF
# A1cQ: QUI <-  "       "
# B1c: FHC <- org_fz1 + citF
# B1cQ : QUI <- "         "
# A1l: lags
# A1p: performance
# A1f: families

#Analysis on citations --> drop this analysis because there is no variations in citations (one of homogeneity sampling criteria)


##LIBRARY##

#library for QCA
library("QCA")#, lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
#library("QCA")#, lib.loc="~/Library/R/3.1/library")

#other useful packages
#library ("Hmisc")
library ("stats")
library ("psych")
library ("gdata")

#set the directory

#setwd ("~/your_directory_here")

#send results to a text file
#sink (file="FGA_GA_Org_29092019.txt", type=c("output", "message"), split = FALSE)

set_calibration_8 <- read.delim("set_calibration9.txt")
View(set_calibration_8)

names (set_calibration_8)
str (set_calibration_8)
attributes(set_calibration_8)

##DESCRIPTIVE STATISTICS##

#central tendencies and dispersion of study conditions
#display objects

sum2<-summary(set_calibration_8)
sum2

descriptive<-describe(set_calibration_8)
descriptive

#pair-wise correlations of study conditions
correlation8<-cor(set_calibration_8 [,c(2:21)], use ="pairwise.complete.obs")
correlation8

#CREATE VARIABLES FOR FAST PATENT GRANT

#For measures that use PatStat 2016
set_calibration_8$FAST1113 <- (1-set_calibration_8$GRANTLAG1113)
describe(set_calibration_8$FAST1113)
describe(set_calibration_8$GRANTLAG1113)

set_calibration_8$FAST1011 <- (1-set_calibration_8$GRANTLAG0110)
describe(set_calibration_8$FAST1011)

set_calibration_8$FASTAV <- (1- set_calibration_8$GRANTLAGAVE)

set_calibration_8$QUIFAST1113 <- paste(set_calibration_8$QUIGRANTLAG1113)
set_calibration_8$QUIFAST1113 <- recode(set_calibration_8$QUIFAST1113, "1=5 ; 2=4 ; 3=3 ; 4=2; 5=1")  

sum2<-summary(set_calibration_8)
sum2

descriptive2 <-describe(set_calibration_8)
descriptive2

##GRAPHS AND HISTOGRAMS
#histograms on grant achievement


hist(set_calibration_8$FHG1113_13, breaks=6) #for comparison only

hist(set_calibration_8$QUIFHG1113, breaks=5,
     main = "Histogram of quintiles of odds of obtaining a patent grant, 2011-2013",
     xlab = "quintiles of odds of patent grant", ylab = "density")

hist(set_calibration_8$TERFHG1113, breaks=3,
     main = "Histogram of terciles of odds of obtaining a patent grant, 2011-2013",
     xlab = "terciles of odds of patent grant", ylab = "density")

dFHGAVE <- density(set_calibration_8$FHGAVE)
plot(dFHGAVE, main= "Odds of obtaining a patent grant, 2001-2013",
     xlab= "patent grant", ylab="density")

dFHG1113 <- density(set_calibration_8$FHG1113,
                    main= "odds of obtaining a patent grant, 2011-2013",
                    xlab= "patent grant", ylab="density")
plot(dFHG1113)

#then the analysis should be on the data from PatStat16
#note that the distribution is not the same 

#Slow and fast achievement 
hist(set_calibration_8$QUIGRANTLAG1113, breaks=5,
     main = "Histogram of quintiles of lags in patent grant, 2011-2013",
     xlab = "Quintiles of patent grant lags", ylab = "density")

hist(set_calibration_8$TERGRANTLAG1113, breaks=3,
     main = "Histogram of terciles of lags in patent grant, 2011-2013",
     xlab = "Terciles of patent grant lags", ylab = "density")

dLAG1113 <- density(set_calibration_8$GRANTLAG1113)
plot(dLAG1113, main = "average grant lag 2011-2013",
     xlab = "grant lag", ylab = "density")

dLAGAV<-density(set_calibration_8$GRANTLAGAVE)
plot(dLAGAV, main = "Average grant lag 2001-2013",
     xlab = "grant lag", ylab = "density")

#for the paper you need to combine two graphs: dLAGAV and dFHGAVE
par(mfrow=c(2,1))

dLAGAV<-density(set_calibration_8$GRANTLAGAVE)
plot(dLAGAV, main = "Average patent grant lag, 2001-2013",
     xlab = "patent grant lag", ylab = "density")

dFHGAVE <- density(set_calibration_8$FHGAVE)
plot(dFHGAVE, main= "Odds of obtaining a patent grant, 2001-2013",
     xlab= "patent grant", ylab="density")


# Being fast in obtaining a patent grant
hist(set_calibration_8$QUIFAST1113, breaks = 5,
     main = "Histogram of quintiles of odds of fast patent grant",
     xlab = "quintiles (Fast patent grants)", ylab = "density") #(1-patent grant lag)

dFAST16 <- density(set_calibration_8$FAST1113)
plot(dFAST16, main = "Odds of fast patent grant 2011-2013" , 
     xlab = "Fast patent grant" , ylab = "density")

dFASTAV <- density(set_calibration_8$FASTAV)
plot(dFASTAV, main = "average likelihood of being fast in patent grant 2001-2013" , 
     xlab = "1-patent grant lag" , ylab = "density"  )

#for comparison only

hist(set_calibration_8$GRANTLAG1113_13, breaks=6) #for comparison only

hist(set_calibration_8$GRANTLAG0110, breaks=6) #for comparison only

#scatterplot between outcome (FGH1113 and grant lag16) and organizational conditions

plot(set_calibration_8$centralization, set_calibration_8$grantlag16,
     xlim=c(0, 25), ylim=c(0, 15),
     xlab = "centralization", ylab = "grant lag 2011-2013 (Patstat2016)"
)

plot(set_calibration_8$centralization, set_calibration_8$FHG1113,
     xlim=c(0, 1), ylim=c(0, 2),
     xlab = "centralization", ylab = "grant achievment 2011-2013 (PatStat 2016)"
)


plot(set_calibration_8$centralization, set_calibration_8$QUIFHG1113,
     xlim=c(0, 1), ylim=c(0, 6),
     xlab = "centralization", ylab = "grant achievment 2011-2013 (quintile distribution, Patstat 2016)"
)

plot(set_calibration_8$cross_functional, set_calibration_8$grantlag16,
     xlim=c(0, 25), ylim=c(0, 15),
     xlab = "cross-functionality", ylab = "grant lag  2011-2013 (PatStat 2016)"
)

plot(set_calibration_8$cross_functional, set_calibration_8$FHG1113,
     xlim=c(0, 3), ylim=c(0, 4),
     xlab = "cross-functionality", ylab = "grant achievment 2011-2013 (Patstat2016)"
)

plot(set_calibration_8$cross_functional, set_calibration_8$QUIFHG1113,
     xlim=c(0, 3), ylim=c(0, 8),
     xlab = "cross-functionality", ylab = "grant achievment 2011-2013 (quintile distribution, Patstat2016)"
)

plot(set_calibration_8$plan, set_calibration_8$grantlag16,
     xlim=c(0, 25), ylim=c(0, 15),
     xlab = "coordination by plan", ylab = "grant lag 2011-2013"
)

plot(set_calibration_8$plan, set_calibration_8$FHG1113,
     xlim=c(0, 1), ylim=c(0, 2),
     xlab = "coordination by plan", ylab = "grant achievment 2011-2013 (Patstat2016)"
)

plot(set_calibration_8$plan, set_calibration_8$QUIFHG1113,
     xlim=c(0, 1), ylim=c(0, 6),
     xlab = "coordination by plan", ylab = "grant achievment 2011-2013 (quintile distribution, Patstat2016)"
)

#look at this webpage if you want to plot two or more plots in one graph https://www.statmethods.net/advgraphs/layout.html

#create figure 1 
Raw_data_fig1 <- read.delim("~/Dropbox/IP Management/IP complementarity and qca/patent data fsQCA Giulia (1)/__26_28August2019/PaperAugust2019/Raw_data_fig1.txt")
View(Raw_data_fig1)

library(ggplot2)
library(lattice)

attach(Raw_data_fig1)

dotplot(SD_GRANT_LAG_USPTO~APPY|ID,
            main="Grant lag USPTO" ,
            xlab = "standard deviations grant lag USTPO"
            )
     
#---------------------------------------------------------
#--------------------------CALIBRATION---------------------
#----------------------------------------------------------

# NOTE: When calibrating conditions equal to 0.5 --> add a constant of 0.001 to the causal condition before calibrating (see Fiss, 2011)

################################
## ORGANIZATIONAL CONDITIONS ###
################################

## calibration based on the indirect method (Ragin)

#centralization: threshold on 3 points
th <-c(0.0, 0.5, 1)
#fuzzy-calibration positive end point concept, linear, method Dusa and Thiem (2013)
central_fz<-calibrate(set_calibration_8$centralization, type="fuzzy", 
                      thresholds =c(th[1], th[2], th[3]), 
                      include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
central_fz

#centralization: threshold on 6 points method Dusa and Thiem (2013)

#add first a constant of 0.001 to the causal conditions because the 6-point calibration generates many crossovers 
set_calibration_8$centralization <-(set_calibration_8$centralization + 0.001)

th <-c(0.0, 0.2, 0.4, 0.6, 0.9, 1)
#fuzzy-calibration positive end point concept, linear
central_fz1<-calibrate(set_calibration_8$centralization, type="fuzzy", 
                       thresholds =c(th[1], th[3], th[5], th[6], th[4], th[2]), 
                       include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
central_fz1

#cross_functional interaction: threshold on 3 points
                                       
th <-c(0.0, 0.5, 1)
#fuzzy-calibration positive end point concept, linear
cross_fz<-calibrate(set_calibration_8$cross_functional, type="fuzzy", 
                    thresholds =c(th[1], th[2], th[3]), 
                    include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
cross_fz

#cross_functional: threshold on 6 points
#add first a constant of 0.001 to the causal conditions because the 6-point calibration generates many crossovers 

set_calibration_8$cross_functional <- (set_calibration_8$cross_functional + 0.001)

th <-c(0.0, 0.2, 0.4, 0.6, 0.9, 1)
#fuzzy-calibration positive end point concept, linear
cross_fz1<-calibrate(set_calibration_8$cross_functional, type="fuzzy", 
                     thresholds = c(th[1], th[3], th[5], th[6], th[4], th[2]), 
                     include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p = 1, q = 1)
cross_fz1


#plan: threshold on 3 points
th <-c(0.0, 0.5, 1)
#fuzzy-calibration positive end point concept, linear
plan_fz<-calibrate(set_calibration_8$plan, type="fuzzy", 
                   thresholds =c(th[1], th[2], th[3]), 
                   include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
plan_fz


#plan: threshold on 6 points
#add first a constant of 0.001 to the causal conditions because the 6-point calibration generates many crossovers 

set_calibration_8$plan <- (set_calibration_8$plan + 0.001)

th <-c(0.0, 0.2, 0.4, 0.6, 0.9, 1)
#fuzzy-calibration positive end point concept, linear
plan_fz1<-calibrate(set_calibration_8$plan, type="fuzzy", 
                    thresholds =c(th[1], th[3], th[5], th[6], th[4], th[2]), 
                    include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
plan_fz1


##############################
##        CONTROLS         ###
##############################
#calibration based on the direct method, for the cross-overs use the median

#NOTES: the calibration proved to be very sensitive to the thresholds. In the  first place
#(27.08.2019) we tried to calibrate using the full values of  max, min and median for the cross
#These values do not lead to any consistent configuration. As a second try (28.09.2019) we tried to
#calibrate using the ter-1 as exclusion, median as cross and ter-3 as inclusion for increasing functions
#But not consistency in the configurations. Finally, we used max, median and min approximating by the 
#first two digits. 

#citations
citationsF<-calibrate(set_calibration_8$CITAVE, type="fuzzy", method = "direct",
                           thresholds = ("e=-0.21, c=-0.03, i=0.32"),
                           include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
citationsF

cit0110F<-calibrate(set_calibration_8$CIT0110, type="fuzzy",method = "direct",
                    thresholds = ("e=-0.20, c=-0.04, i=0.33"),
                    include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
cit0110F

cit1113F<-calibrate(set_calibration_8$CIT1113, type="fuzzy", method = "direct",
                    thresholds = ("e=-0.22, c=-0.05, i=0.42"),
                    include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
cit1113F

#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$QUICIT1113 <- (set_calibration_8$QUICIT1113 + 0.001)

th <-c(0, 1, 2, 3, 4)
quicitF<-calibrate(set_calibration_8$QUICIT1113, type="fuzzy",
                   thresholds =c(th[1], th[3], th[5]) ,
                   include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
quicitF

#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$TERCIT1113 <- (set_calibration_8$TERCIT1113 + 0.001)

th<-c (0, 1, 2)
tercitF<-calibrate(set_calibration_8$TERCIT1113, type="fuzzy",
                   thresholds =c(th[1], th[2], th[3]),
                   include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
tercitF



#families
fam_av <-calibrate(set_calibration_8$FAMAVE, type="fuzzy",method = "direct",
                   thresholds = ("e=-0.20, c=0.16, i=1.42"),
                   include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
fam_av

fam0110 <-calibrate(set_calibration_8$FAM0110, type="fuzzy",method = "direct",
                   thresholds = ("e=-0.20, c=0.20, i=1.54"),
                   include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
fam0110

fam1113 <-calibrate(set_calibration_8$FAM1113, type="fuzzy",method = "direct",
                     thresholds = ("e=-0.19, c=-0.003, i=0.97"),
                     include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
fam1113

#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$QUIFAM1113 <- (set_calibration_8$QUIFAM1113 + 0.001)

th <-c(0, 1, 2, 3, 4)
quifamF<-calibrate(set_calibration_8$QUIFAM1113, type="fuzzy",
                   thresholds =c(th[1], th[3], th[5]) ,
                   include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
quifamF

#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$TERFAM1113 <- (set_calibration_8$TERFAM1113 + 0.001)

th<-c (0, 1, 2)
terfamF<-calibrate(set_calibration_8$TERFAM1113, type="fuzzy",
                   thresholds =c(th[1], th[2], th[3]),
                   include=TRUE, logistic = TRUE, idm=0.95, ecdf = FALSE)
terfamF


##PERFORMANCE
#PATSTAT2016

#grant achievement based on quintiles, calibration based on a linear function 
#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$QUIFHG1113 <- (set_calibration_8$QUIFHG1113 + 0.001)

th <-c(0, 1, 2, 3, 4)
QUIFHG1113F<-calibrate(set_calibration_8$QUIFHG1113, type = "fuzzy",
                       thresholds =c(th[1], th[3], th[5]) , 
                       include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
QUIFHG1113F

#grant achievement based on continuous values, calibration based on a linear function
#for the cross-over use the median value

th<-c(0.71576595, 1.036590950, 1.245248)
FHG1113F_1<-calibrate(set_calibration_8$FHG1113, type = "fuzzy",
                    thresholds =c(th[1], th[2], th[3]) , 
                    include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
FHG1113F_1

#here I use the logistic function over three thresholds
FHG1113F <-calibrate(set_calibration_8$FHG1113, type = "fuzzy", method = "direct",
                       thresholds = "e=0.71, c=1.03, i=1.24", logistic= TRUE, idm=0.95)
FHG1113F


#here below I want to compare the two direct calibration methods with a graph
#the inspection shows mostly an overlap and there is just a minor difference
#between the linear and logit function 
#in this case, to be homogeneous with the other measures below, we opt for a logistic function
#for justification, see Dusa (2018) p. 80-82.

plot(set_calibration_8$FHG1113,FHG1113F, cex=0.6,
     main = "", xlab = "Raw data", ylab = "Calibrated data")
points(set_calibration_8$FHG1113, FHG1113F_1, cex=0.6, col="gray80")

#grant achievement based on terciles
#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$TERFHG1113 <- (set_calibration_8$TERFHG1113 + 0.001)

th<-c (0, 1, 2)
TERFHG1113F<-calibrate(set_calibration_8$TERFHG1113, type = "fuzzy",
                       thresholds =c(th[1], th[2], th[3]) , 
                       include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
TERFHG1113F

#past grant achievement (Patstat 2016)
FHG0111F<-calibrate(set_calibration_8$FHG0110, type = "fuzzy", method = "direct",
                    thresholds = "e=0.87, c=1.19, i=1.38", logistic= TRUE, idm=0.95)
FHG0111F

#grantlag (Patstat2016)
grantlag16<-calibrate(set_calibration_8$GRANTLAG1113, type = "fuzzy", method = "direct",
                      thresholds = "e=0.58, c=0.009, i= -1.02",
                      include = TRUE, logistic = TRUE, idm = 0.95)

grantlag16

plot(set_calibration_8$GRANTLAG1113,grantlag16, cex=1,
     main = "", xlab = "Raw data", ylab = "Grantlag16 calibrated data")
points(set_calibration_8$GRANTLAG1113, grantlag16, cex=1, col="gray80")


#past grantlag (Patstat2016)
grantlag0111<-calibrate(set_calibration_8$GRANTLAG0110, type = "fuzzy", method = "direct",
                        thresholds = "i=-0.17, c=0.11, e=1.11",
                        include= TRUE, logistic = TRUE, idm = 0.95, ecdf = FALSE)
grantlag0111

#grant averagege (Patstat2016)
grantlagav<-calibrate(set_calibration_8$GRANTLAGAVE, type = "fuzzy", method = "direct",
                        thresholds = "i=-0.16, c=0.06, e=1",
                        include= TRUE, logistic = TRUE, idm = 0.95, ecdf = FALSE)
grantlagav

#grant_mean (Patstat2016)
grantavF<-calibrate(set_calibration_8$FHGAVE, type="fuzzy", method = "direct",
                    thresholds = "e=0.87, c=1.168, i=1.36" , 
                    include = TRUE, logistic = TRUE, idm = 0.95)
grantavF

#quintiles grantlag (PatStat2016)
#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$QUIGRANTLAG1113 <- (set_calibration_8$QUIGRANTLAG1113 + 0.001)

th <-c(0, 1, 2, 3, 4)
QUILag16 <- calibrate(set_calibration_8$QUIGRANTLAG1113, type = "fuzzy", 
                      thresholds =c(th[1], th[3], th[5]) ,
                      include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
QUILag16

#terciles grantlag
#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$TERGRANTLAG1113 <- (set_calibration_8$TERGRANTLAG1113 + 0.001)

th<-c (0, 1, 2)
TERLag16 <- calibrate(set_calibration_8$TERGRANTLAG1113, type = "fuzzy", 
                      thresholds =c(th[1], th[2], th[3]) ,
                      include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)

# Fast grant achievement
Fast16 <-calibrate(set_calibration_8$FAST1113, type = "fuzzy", method = "direct",
                   thresholds = "e=0.41, c=0.99, i=2.02",
                   include= TRUE, logistic = TRUE, idm = 0.95, ecdf = FALSE)
Fast16

#Fast past grant achievement
Fast0110 <- calibrate(set_calibration_8$FAST1011, type = "fuzzy", method = "direct",
                      thresholds = "e=-0.11 , c=0.88 , i=.17",
                      include= TRUE, logistic = TRUE, idm = 0.95, ecdf = FALSE
)
Fast0110

#Average fast grant achievement
Fastav <- calibrate(set_calibration_8$FASTAV, type = "fuzzy", method = "direct",
                    thresholds = "e=-0.00, c=0.93, i=1.16",
                    include= TRUE, logistic = TRUE, idm = 0.95, ecdf = FALSE
)

Fastav

#Quintiles of fast grant achievement (2011-2013)
#add first a constant of 0.001 to the causal conditions because the calibration generates many crossovers 
set_calibration_8$QUIFAST1113 <- (set_calibration_8$QUIFAST1113 + 0.001)

th <-c(0, 1, 2, 3, 4)
QUIFast16 <- calibrate(set_calibration_8$QUIFAST1113, type = "fuzzy",
                       thresholds =c(th[1], th[3], th[5]) ,
                       include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
QUIFast16

#PATSTAT2013
#grant achievement (Patstat2013)
FHG1113_13F<-calibrate(set_calibration_8$FHG1113_13, type = "fuzzy", method = "direct" ,
                    thresholds ="e=0.04, c=1.030041, i=3.4", 
                    include = TRUE, logistic = TRUE, idm = 0.95, ecdf = FALSE)
FHG1113_13F

#past grant achievement (Patstat 2013)
FHG0111F_13<-calibrate(set_calibration_8$FHG1110_13, type = "fuzzy", method = "direct",
                    thresholds = "e=0.045, c=1.20, i=1.51", logistic= TRUE, idm=0.95)
FHG0111F_13

#grantlag (Patstat2013) threshold on three points
grantlag13<-calibrate(set_calibration_8$GRANTLAG1113_13, type = "fuzzy", method = "direct",
                      thresholds = "e=-0.32, c=-0.003, i=0.61",
                      include = TRUE, logistic = FALSE, idm = 0.95, ecdf = FALSE, p=1, q=1)
grantlag13

#past grantlag (Patstat2013)
grantlag0111_13<-calibrate(set_calibration_8$GRANTLAG0110_13, type = "fuzzy", method = "direct",
                        thresholds = "e=-0.07, c=0.17, i=0.62",
                        include= TRUE, logistic = TRUE, idm = 0.95, ecdf = FALSE)
grantlag0111_13

####################
## SET MEMBERSHIP ##
####################

IPorg_fuzzy3<-data.frame(central_fz, central_fz1, cross_fz, cross_fz1, plan_fz, plan_fz1,
                         FHG1113F, FHG0111F, QUIFHG1113F, TERFHG1113F, grantavF,
                         grantlag16, grantlagav,grantlag0111,QUILag16,
                         Fast16, Fast0110, Fastav, QUIFast16,
                         FHG1113_13F, FHG0111F_13, grantlag13, grantlag0111_13,
                         citationsF, cit0110F, cit1113F, quicitF, tercitF,
                         fam_av, fam0110, fam1113, quifamF, terfamF
                         )

head(IPorg_fuzzy3)

##creation of the negation of set for the negation of outcome

IPorg_fuzzy3$NQUIFHG1113F <- 1- IPorg_fuzzy3$QUIFHG1113F
IPorg_fuzzy3$NFHG1113F <- 1- IPorg_fuzzy3$FHG1113F
IPorg_fuzzy3$NTERFHG1113F <- 1- IPorg_fuzzy3$TERFHG1113F
IPorg_fuzzy3$NFHG1113_13F <- 1- IPorg_fuzzy3$FHG1113_13F
IPorg_fuzzy3$Ngrantlag16 <- 1- IPorg_fuzzy3$grantlag16
IPorg_fuzzy3$Ngrantlag13 <- 1- IPorg_fuzzy3$grantlag13
IPorg_fuzzy3$NFast16 <- 1 - IPorg_fuzzy3$Fast16
IPorg_fuzzy3$NQUIFast16 <- 1 - IPorg_fuzzy3$QUIFast16

#--------------------------------------------ANALYSIS ON FHG (PATSTAT 2016)--------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
##                                        ANALYSIS WITH ORG DIMENSIONS
#-------------------------------------------------------------------------------------------------------------------
#conditions are based on 6-point cross-overs
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fss1a<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE)

fss1a

pof(1-fss1a$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn1a<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssn1a

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1a <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE)

TT1a

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CS1a <-minimize (TT1a, outcome = "FHG1113F",
                 relation = "suf", incl.cut1 = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CS1a


#with inclusion of 0.9, there is just one configuration --> CENTRAL_FZ*CROSS_FZ*PLAN_FZ (9,17)

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT1a2<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1a2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1a <- minimize (TT1a2, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1a

PS1a$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS1a2 <- minimize (TT1a2, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

PS1a2
PS1a2$PIchart
PS1a2$SA$M1

factorize(PS1a2)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA1a <- findRows(obj = TT1a2, type = 2)
CSA1a

#enhanced parsimonious solution
PS1aE <- minimize (TT1a2, outcome = "FHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    include = c("?"), exclude = CSA1a, all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1aE

#INTERMEDIATE SOLUTION with directionality --> not possible including them, because the only 
# reminder is an implausible counterfactual --> only complex and parsimonious solutins

IS1a <- minimize (TT1a2, outcome = "FHG1113F", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"),
                  dir.exp= "1,1,-", # this is not very correct, because it might minimize also a CSA 
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
IS1a

#---------------------------------------ANALYSIS 1aQ ON QUIFGH1113F (PATSTAT 2016)--------------------------------------------

#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fss1aQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE)

fss1aQ

pof(1-fss1aQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn1aQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssn1aQ

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1aQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                  show.cases = TRUE)

TT1aQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CS1aQ <-minimize (TT1aQ, outcome = "QUIFHG1113F",
                 relation = "suf", incl.cut1 = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CS1aQ
#one more configuration respect to the outcome FHG1113F --> ~cent*~cross (3, 16, 19)

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT1a2Q<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1a2Q

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1aQ <- minimize (TT1a2Q, outcome = "QUIFHG1113F", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE)
PS1aQ

PS1aQ$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS1a2Q <- minimize (TT1a2Q, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

PS1a2Q
PS1a2Q$PIchart
PS1a2Q$SA$M1

factorize(PS1a2Q)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA1aQ <- findRows(obj = TT1a2Q, type = 2)
CSA1aQ
#there are no implausible counterfactuals --> no need to include them in the enhanced parsimonious solutions

# #enhanced parsimonious solution
# PS1aEQ <- minimize (TT1a2Q, outcome = "QUIFHG1113F", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz"),
#                    include = c("?"), exclude = CSA1aQ, all.sol = FALSE,
#                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE)
# PS1aEQ

#INTERMEDIATE SOLUTION with directionality

IS1aQ <- minimize (TT1a2Q, outcome = "QUIFHG1113F", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"),
                  dir.exp= "1,1,-", 
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
IS1aQ

#NEGATION OF OUTCOME
TT1a2QN<- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1a2QN

#there are no configurations with this threshold. I lower the incl.cut = c(0.8, 0.6)

#NEGATION OF OUTCOME
TT1a2QN<- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                     relation = "suf", incl.cut = c(0.8, 0.6),
                     conditions= c ("central_fz","cross_fz","plan_fz"),
                     complete= TRUE, show.case= TRUE,
                     sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1a2QN

#also with this lower threshold, there are no configurations resulting in the negatio of the outcome
#this should confirm the analysis for the positive outcome

#--------------------------- ANALYSIS 1a1 ON FHG (PATSTAT 2016) WITH LOWER INC.CUT -------------------------
#as the inclusion  is quite strict, we try to lower the inc.cut to 0.8 and coverage 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.8 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fss1a1<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                   relation="nec", incl.cut=0.8, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE)

fss1a1
#just disjuntive necessary configurations, with high thresholds. 

pof(1-fss1a1$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn1a1<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                    relation="nec", incl.cut=0.8, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssn1a1

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##
#by visual inspection of TT1a2, it is possible to see that there is quite a threshold
#in terms of inclusion scores and PRI --> from 0.876 (PRI0.546) to 0.837 (PRI: 0.268)
#such a low PRI (proportional reduction in inconsistency) could be problematic. It
#shows that configuration [2] is quite inconsitent with the output. Usually, you can 
#consider to include configurations that have an inclusion scopre > 0.5. 

TT1a1 <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  incl.cut =0.85,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                  show.cases = TRUE)

TT1a1

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CS1a1 <-minimize (TT1a1, outcome = "FHG1113F",
                 relation = "suf", incl.cut1 = 0.8,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CS1a1

factorize(CS1a1)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT1a12<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                   relation = "suf", incl.cut = 0.85,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1a12

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.85,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1a1 <- minimize (TT1a12, outcome = "FHG1113F", 
                  relation = "suf", incl.cut1 = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE)
PS1a1

PS1a1$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS1a12 <- minimize (TT1a12, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

PS1a12
PS1a12$PIchart
PS1a12$SA$M1

factorize(PS1a12)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA1a1 <- findRows(obj = TT1a12, type = 2)
CSA1a1

#enhanced parsimonious solution
PS1a1E <- minimize (TT1a12, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), exclude = CSA1a, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1a1E

#INTERMEDIATE SOLUTION with directionality --> does not make sense in this case
#there is just one reminder that is an implausible counterfactual and cannot be added

IS1a1 <- minimize (TT1a12, outcome = "FHG1113F",
                  relation = "suf", incl.cut=0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"),
                  dir.exp= "1,1,-",  #this cannot be added because it might include an implausible counterfactuals
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE)

IS1a1

#NEGATION OF OUTCOME

TT1a1N<- truthTable(IPorg_fuzzy3,outcome = "NFHG1113F",
                     relation = "suf", incl.cut1 = 0.9,
                     conditions= c ("central_fz","cross_fz","plan_fz"),
                     complete= TRUE, show.case= TRUE,
                     sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1a1N
#There is just one configuration (0,1,1) associated with case 19
#This configuration was not used to produce the positive outcome


#---------------------------------------ANALYSIS 1a1Q ON QUIFGH1113F (PATSTAT 2016)--------------------------------------------

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.88 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores


fss1a1Q<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                    relation="nec", incl.cut=0.88, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE)

fss1a1Q

pof(1-fss1a1Q$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn1a1Q<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                     relation="nec", incl.cut=0.88, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssn1a1Q
#there are no configurations using these cutoff values

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1a1Q <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   incl.cut =0.88,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE)

TT1a1Q

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CS1a1Q <-minimize (TT1a1Q, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut1 = 0.88,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CS1a1Q

factorize(CS1a1)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT1a12Q<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut = 0.88,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1a12Q

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.88,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1a1Q <- minimize (TT1a12Q, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.88,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1a1Q

PS1a1Q$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS1a12Q <- minimize (TT1a12Q, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.88,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)

PS1a12Q
PS1a12Q$PIchart
PS1a12Q$SA$M1

factorize(PS1a12Q)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA1a1Q <- findRows(obj = TT1a12, type = 2)
CSA1a1Q

#enhanced parsimonious solution
PS1a1QE <- minimize (TT1a12Q, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.88,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    include = c("?"), exclude = CSA1a1Q, all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1a1QE

#INTERMEDIATE SOLUTION with directionality --> they do not make sense in this analysis
#the only reminder is row [4] =(0,1,1)

IS1a1Q <- minimize (TT1a12, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut=0.88,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), 
                   dir.exp= "1,1,-", #this cannot be used because it might include an implausible counterfactual (CSA) 
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

IS1a1Q

#NEGATION OF OUTCOME
TT1a1QN<- truthTable(IPorg_fuzzy3,outcome = "NFHG1113F",
                    relation = "suf", incl.cut = 0.88,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1a1QN
#This TT is very similar to TT1a12QN.There is just one configuration (0,0,1) that
#was not part of the positive outcome. It is not possible lowering the threshold. 

#---------------------------------------ANALYSIS 1b ON FHG (PATSTAT 2016)--------------------------------------------
#use _fz1 in the calibration of the conditions
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fss1b<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                   neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE)

fss1b

pof(1-fss1b$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn1b<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssn1b

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##
#if we set the inclusion at 0.9  in the TT all outcomes values have been coded to zero
#Hence, we decide to lower the inclusion in the truth table to 0.85 --> it can be lowered up to 0.82

TT1b <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                  incl.cut1 =0.85,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                  show.cases = TRUE)

TT1b

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##
#We set the inclusion threshold to 0.82 to avoid cases with PRI=0.479. 

CS1b <-minimize (TT1b, outcome = "FHG1113F",
                 relation = "suf", incl.cut = 0.85,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CS1b

factorize(CS1b)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT1b2<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                   relation = "suf", incl.cut = 0.85,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1b2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.85,
           conditions= c ("central_fz1","cross_fz1","plan_fz1"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1b <- minimize (TT1b2, outcome = "FHG1113F", 
                  relation = "suf", incl.cut = 0.85,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE)
PS1b

PS1b$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS1b2 <- minimize (TT1b2, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.85,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

PS1b2
PS1b2$PIchart
PS1b2$SA$M1

factorize(PS1b2)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA1b <- findRows(obj = TT1b2, type = 2)
CSA1b
#only 1 implausible counterfactuals [4]

#enhanced parsimonious solution
PS1bE <- minimize (TT1b2, outcome = "FHG1113F", 
                   relation = "suf", incl.cut = 0.85,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   include = c("?"), exclude= CSA1b, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1bE

#INTERMEDIATE SOLUTION with directionality --> it does not make sense including (0,1,1), the only reminder,
#because it is a CSA. The intermediate will converge with the enhanced parsimonious solutions. 

IS1b <- minimize (TT1b2, outcome = "FHG1113F", 
                  relation = "suf", incl.cut = 0.85,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                  include = c("?"), exclude= CSA1b,
                  dir.exp= "1,1,1",  
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
IS1b

#NEGATION OF OUTCOME

TT1bN<- truthTable(IPorg_fuzzy3,outcome = "NFHG1113F",
                     relation = "suf", incl.cut = 0.85,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                     complete= TRUE, show.case= TRUE,
                     sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1bN
#There is just one configuration (0,0,1) that is not participating to the presence of outcome

#---------------------------------------ANALYSIS 1bQ ON QUIFHG1113F (PATSTAT 2016)--------------------------------------------

#use _fz1 in the calibration of the conditions
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fss1bQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                   neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE)

fss1bQ

pof(1-fss1bQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn1b<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssn1bQ
#There are no configurations using these cutoff values. 

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1bQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                  incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                  show.cases = TRUE)

TT1bQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CS1bQ <-minimize (TT1bQ, outcome = "QUIFHG1113F",
                 relation = "suf", incl.cut1 = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CS1bQ

factorize(CS1b)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT1b2Q<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1b2Q

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz1","cross_fz1","plan_fz1"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1bQ <- minimize (TT1b2Q, outcome = "QUIFHG1113F", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE)
PS1bQ

PS1b$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS1b2Q <- minimize (TT1b2Q, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

PS1b2Q
PS1b2$PIchart
PS1b2$SA$M1

factorize(PS1b2)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA1bQ <- findRows(obj = TT1b2Q, type = 2)
CSA1bQ


#enhanced parsimonious solution --> no need for this step
# PS1bEQ <- minimize (TT1b2Q, outcome = "QUIFHG1113F", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz1","cross_fz1","plan_fz1"),
#                    include = c("?"), exclude = CSA1bQ, all.sol = FALSE,
#                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE)
# PS1bEQ

#INTERMEDIATE SOLUTION with directionality --> collapse to the parsimonious

IS1bQ <- minimize (TT1b2Q, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                  include = c("?"),
                  dir.exp= "1,1,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
IS1bQ

#NEGATION OF OUTCOME
TT1bQN <- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                   relation = "suf", incl.cut = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1bQN

#There is no outcome with this threshold. 

TT1bQN <- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                     relation = "suf", incl.cut = c(0.8, 0.6),
                     conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                     complete= TRUE, show.case= TRUE,
                     sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1bQN

#There is just one configuration (0,0,1) represented by one case and this configuration
#does not enter into the positive outcome. The PRI is very low and the other configurations
#are all flagged as C. 

##NOTE ON THE INTERMEDIATE SOLUTION WITH 2^3 CONFIGURATIONS
#There is just one configuration that usually remains as logical reminder --> all the corner solutions
#are taken --> limited need for logical reminders --> if included, intermediate = complex solutions

#####################################################################################################
## Of all the configurations that I tried, only 1bQ set offers results that are consitent (>0.8) and
## with a high coverage (0.7). 
#####################################################################################################


####################################################################################################
##                                          CONTROLS
####################################################################################################

##----------- ANALYSIS WITH CITATIONS

#1A: outcome on FHG1113 grant achievement (Patstat 2016)
#First attempt is inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssA1c<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F",
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                 "quicitF"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                   PRI=TRUE
)

fssA1c

pof(1-fssA1c$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnA1c<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quicitF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnA1c

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1A1c <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1A1c

# There are just two configurations represented by two cases. It is a bit restrictive. 
# It makes sense lowering the threshold to 0.87, caputuring more cases that have
# in any case a PRI > 0.5

TT1A1c <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   incl.cut1 =0.87,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1A1c

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSA1c <-minimize (TT1A1c, outcome = "FHG1113F",
                 relation = "suf", incl.cut = 0.87,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CSA1c

factorize(CSA1c)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2A1c<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                   relation = "suf", incl.cut1 = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2A1c

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.87,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "quicitF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1A1c <- minimize (TT2A1c, outcome = "FHG1113F",
                   relation = "suf", incl.cut1 = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1A1c

PS1A1c$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2A1c <- minimize (TT2A1c, outcome = "FHG1113F",
                   relation = "suf", incl.cut1 = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
PS2A1c
PS2A1c$PIchart
PS2A1c$SA$M3

factorize(PS2A1c)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAA1c <- findRows(obj = TT2A1c, type = 2)
CSAA1c

#enhanced parsimonious solution
PS2A1cP <- minimize (TT2A1c, outcome = "FHG1113F",
                    relation = "suf", incl.cut1 = 0.85,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quicitF"),
                    include = c("?"), exclude = CSAA1c, all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2A1cP

#INTERMEDIATE SOLUTION with directionality

ISA1c <- minimize (TT2A1c, outcome = "FHG1113F",
                  relation = "suf", incl.cut1 = 0.87,
                  conditions= c ("central_fz","cross_fz","plan_fz",
                                 "quicitF"),
                  include = c("?"),
                  dir.exp= "1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
ISA1c

###############QUINTILES
#1AcQ: outcome on QUIFHG1113 grant achievement (Patstat 2016)
#First attempt is inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssA1cQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                  "quicitF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssA1cQ

pof(1-fssA1cQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnA1cQ2<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F",
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quicitF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnA1cQ2
#there are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1A1cQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quicitF"),
                    incl.cut =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1A1cQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSA1cQ <-minimize (TT1A1cQ, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut = 0.9,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSA1cQ

factorize(CSA1cQ)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2A1cQ<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quicitF"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2A1cQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "quicitF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1A1cQ <- minimize (TT2A1cQ, outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1A1cQ

PS1A1cQ$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2A1cQ <- minimize (TT2A1cQ, outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2A1cQ
PS2A1cQ$PIchart
PS2A1cQ$SA$M2

factorize(PS2A1cQ)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAA1cQ <- findRows(obj = TT2A1cQ, type = 2)
CSAA1cQ
#no CSA


#enhanced parsimonious solution --> no need for this
# PS2A1cQP <- minimize (TT2A1cQ, outcome = "QUIFHG1113F",
#                      relation = "suf", incl.cut1 = 0.9,
#                      conditions= c ("central_fz","cross_fz","plan_fz",
#                                     "quicitF"),
#                      include = c("?"), all.sol = FALSE,
#                      row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                      use.letters = FALSE
# )
# PS2A1cQP

#INTERMEDIATE SOLUTION with directionality

ISA1cQ <- minimize (TT2A1cQ, outcome = "QUIFHG1113F",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   include = c("?"),
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISA1cQ
factorize(ISA1cQ)
#checking for easy and difficult counterfactuals
#for model 1
ISA1cQ$i.sol$C1P1$EC

ISA1cQ$i.sol$C1P1$DC

#model 1 did not filter any easy counterfactual
#all the counterfactuals in the model are difficult



#NEGATION OF THE OUTCOME
TTA1cQN <- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                      relation = "suf", incl.cut = 0.9,
                      conditions= c ("central_fz","cross_fz","plan_fz",
                                     "quicitF"),
                      show.case= TRUE, ddc=TRUE,
                      sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TTA1cQN


#lower the inclusion score
TTA1cQN <- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                      relation = "suf", incl.cut = c(0.8, 0.6),
                      conditions= c ("central_fz","cross_fz","plan_fz",
                                     "quicitF"),
                      complete = TRUE, show.case= TRUE, 
                      sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TTA1cQN
#there are just lots of C with too low PRI to conclude anything --> do not go further

# #minimization with negation of outcome complex solutions
# CSA1cQN <- minimize(TTA1cQN, relation = "suf", incl.cut = 0.9,
#                     details = TRUE, show.cases = TRUE,explain ="1")
# CSA1cQN
# 
# 
# #minimization with negation of intermediate complex solutions without reminders
# ISA1cQN <- minimize(TTA1cQN, relation = "suf", incl.cut = 0.9,
#                     include = c("?"),
#                     dir.exp = "-,1,1,-",
#                     details = TRUE, show.cases = TRUE,explain ="1")
# ISA1cQN


#B1: outcome on FHG1113 grant achievement (Patstat 2016) and calibration _fz1 
#First attempt is inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssB1c<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F",
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                                  "quicitF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssB1c

pof(1-fssB1c$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnB1c<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1","quicitF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnB1c

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1B1c <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1c

# There are no configurations with the threshold 0.9.  
# It makes sense lowering the threshold to 0.82, caputuring more cases that have
# in any case a PRI > 0.5

TT1B1c <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    incl.cut1 =0.82,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1c

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSB1c <-minimize (TT1B1c, outcome = "FHG1113F",
                  relation = "suf", incl.cut = 0.82,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSB1c

factorize(CSB1c)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2B1c<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                    relation = "suf", incl.cut = 0.82,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2B1c

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.82,
           conditions= c ("central_fz1","cross_fz1","plan_fz1",
                          "quicitF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1B1c <- minimize (TT2B1c, outcome = "FHG1113F",
                    relation = "suf", incl.cut1 = 0.82,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1B1c

PS1B1c$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2B1c <- minimize (TT2B1c, outcome = "FHG1113F",
                    relation = "suf", incl.cut1 = 0.82,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2B1c
PS2B1c$PIchart
PS2B1c$SA$M1
PS2B1c$SA$M2
#M1 has a higher inclusion for sol 4 vs sol 5

factorize(PS2B1c)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAB1c <- findRows(obj = TT2B1c, type = 2)
CSAB1c

#enhanced parsimonious solution
PS2B1cP <- minimize (TT2B1c, outcome = "FHG1113F",
                     relation = "suf", incl.cut1 = 0.82,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                    "quicitF"),
                     include = c("?"),  all.sol = FALSE,
                     row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                     use.letters = FALSE
)
PS2B1cP

#INTERMEDIATE SOLUTION with directionality

ISB1c <- minimize (TT2B1c, outcome = "FHG1113F",
                   relation = "suf", incl.cut1 = 0.82,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                  "quicitF"),
                   include = c("?"),
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISB1c

###QUINTILES
#B1cQ: outcome on QUIFHG1113 grant achievement (Patstat 2016) and calibration _fz1 
#First attempt is inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssB1cQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                                  "quicitF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssB1cQ

pof(1-fssB1cQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnB1cQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1","quicitF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnB1cQ
#There are no configurations using these cutoff values

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1B1cQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1cQ


##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSB1cQ <-minimize (TT1B1cQ, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut = 0.9,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSB1cQ

factorize(CSB1cQ)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2B1cQ<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2B1cQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz1","cross_fz1","plan_fz1",
                          "quicitF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1B1cQ <- minimize (TT2B1cQ, outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1B1cQ

PS1B1cQ$SA
#there are two models, they do not differ for the use of one reminders 


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2B1cQ <- minimize (TT2B1cQ, outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2B1cQ
PS2B1cQ$PIchart
PS2B1cQ$SA$M1
#this model uses the reminders as model 2 above

factorize(PS2B1cQ)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAB1cQ <- findRows(obj = TT2B1cQ, type = 2)
CSAB1cQ
#There are no CSA

#enhanced parsimonious solution --> no need for this
# PS2B1cQP <- minimize (TT2B1cQ, outcome = "QUIFHG1113F",
#                      relation = "suf", incl.cut1 = 0.9,
#                      conditions= c ("central_fz1","cross_fz1","plan_fz1",
#                                     "quicitF"),
#                      include = c("?"),  all.sol = FALSE,
#                      row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                      use.letters = FALSE
# )
# PS2B1cQP

#INTERMEDIATE SOLUTION with directionality

ISB1cQ <- minimize (TT2B1cQ, outcome = "QUIFHG1113F",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                  "quicitF"),
                   include = c("?"),
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISB1cQ
factorize(ISB1cQ)

#NEGATION OF OUTCOME
TTB1cQN <- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                      relation = "suf", incl.cut = 0.9,
                      conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                     "quicitF"),
                      show.case= TRUE, ddc=TRUE,
                      sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TTB1cQN

#lower the inclusion rate
TTB1cQN <- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                      relation = "suf", incl.cut = c(0.8, 0.6),
                      conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                     "quicitF"),
                      show.case= TRUE, ddc=TRUE,
                      sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TTB1cQN
#There are just  Cs -->no sense to continue 

# #minimization of negation out outcome of complex solutions
# CSB1cQN <- minimize(TTB1cQN, relation = "suf", incl.cut = 0.8,
#                     details = TRUE, show.cases = TRUE,explain ="1")
# CSB1cQN
# 
# #minimization of negation of outcome parsimonius solutions with row.dominance
# PSB1cQN <- minimize(TTB1cQN, relation = "suf", incl.cut = 0.9,
#                     include = c("?"),
#                     all.sol=FALSE, row.dom = TRUE,
#                     details = TRUE, show.cases = TRUE,explain ="1")
# PSB1cQN
# #not consistent
# 
# #minimization of the intermediate solutions without reminders
# ISB1cQN <- minimize(TTB1cQN, relation = "suf", incl.cut = 0.9,
#                     include = c("?"),
#                     dir.exp = "-,1,1,-",
#                     details = TRUE, show.cases = TRUE,explain ="1")
# ISB1cQN
# #not consistent
# #want to check which easy counterfactuals have been used
# ISB1cQN$i.sol$C1P2$EC
# ISB1cQN$i.sol$C1P3$EC

#------------ANALYSIS ON PAST GRANT LAG OUTCOME GRANT ACHIEVEMENT FGH1113F (Patstat 2016)

#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssA1l<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                 "grantlag0111"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE
)

fssA1l

pof(1-fssA1l$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnA1l<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","grantlag0111"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnA1l

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1A1l <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1A1l

# The threshold 0.9 is limitative --> only two configurations with three companies in total (9,17,14)
# I lower the threshold to 0.85 so that I capture configurations with PRI>0.5

TT1A1l <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "grantlag0111"),
                    incl.cut1 =0.85,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1A1l

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSA1l <-minimize (TT1A1l, outcome = "FHG1113F",
                 relation = "suf", incl.cut = 0.85,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CSA1l

factorize(CSA1l)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2A1l<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                   relation = "suf", incl.cut1 = 0.85,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2A1l

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.85,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "grantlag0111"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1A1l <- minimize (TT2A1l, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.85,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1A1l

PS1A1l$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2A1l <- minimize (TT2A1l, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.85,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
PS2A1l
PS2A1l$PIchart
PS2A1l$SA$M1

factorize(PS2A1l)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAA1l <- findRows(obj = TT2A1l, type = 2)
CSAA1l

#INTERMEDIATE SOLUTION with directionality and enhanced solutions

ISA1l <- minimize (TT2A1l, outcome = "FHG1113F", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz",
                                 "grantlag0111"),
                  include = c("?"), 
                  dir.exp= "1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
ISA1l

#QUINTILES A1LQ
#outcome on quintiles of grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssA1lQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                  "grantlag0111"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssA1lQ

pof(1-fssA1lQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnA1lQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","grantlag0111"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnA1lQ

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1A1lQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "grantlag0111"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1A1lQ


##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSA1lQ <-minimize (TT1A1lQ, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut = 0.9,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSA1lQ

factorize(CSA1lQ)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2A1lQ<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "grantlag0111"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2A1lQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "grantlag0111"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1A1lQ <- minimize (TT2A1lQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "grantlag0111"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1A1lQ

PS1A1lQ$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2A1lQ <- minimize (TT2A1lQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "grantlag0111"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2A1lQ
PS2A1lQ$PIchart
PS2A1lQ$SA$M1

factorize(PS2A1lQ)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAA1lQ <- findRows(obj = TT2A1lQ, type = 2)
CSAA1lQ

#INTERMEDIATE SOLUTION with directionality and enhanced solutions

ISA1lQ <- minimize (TT2A1lQ, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISA1lQ

## B1l
#outcome on grant achievement (Patstat 2016) and coordination mechanisms _fz1
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssB1l<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                                  "grantlag0111"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssB1l

pof(1-fssB1l$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnB1l<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1","grantlag0111"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnB1l

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1B1l <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1l

# Since there are only three configurations represented by three companies (9, 11,13) that lead to the outcome
# I will lower the incl.cut to 0.85 and maintain PRI>0.5

TT1B1l <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    incl.cut1 =0.85,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1l


##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSB1l <-minimize (TT1B1l, outcome = "FHG1113F",
                  relation = "suf", incl.cut = 0.85,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSB1l

factorize(CSB1l)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2B1l<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                    relation = "suf", incl.cut1 = 0.85,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2B1l

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.85,
           conditions= c ("central_fz1","cross_fz1","plan_fz1",
                          "grantlag0111"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1B1l <- minimize (TT2B1l, outcome = "FHG1113F", 
                    relation = "suf", incl.cut1 = 0.85,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1B1l

PS1B1l$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2B1l <- minimize (TT2B1l, outcome = "FHG1113F", 
                    relation = "suf", incl.cut1 = 0.85,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2B1l
PS2B1l$PIchart
PS2B1l$SA$M1

factorize(PS2B1l)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAB1l <- findRows(obj = TT2B1l, type = 2)
CSAB1l

#INTERMEDIATE SOLUTION with directionality and enhanced solutions

ISB1l <- minimize (TT2B1l, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                  "grantlag0111"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISB1l

factorize(ISB1l)

#QUINTILES
##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssB1lQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                                  "grantlag0111"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssB1lQ

pof(1-fssB1lQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnB1lQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1","grantlag0111"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnB1lQ

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1B1lQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1lQ



##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSB1lQ <-minimize (TT1B1lQ, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut = 0.9,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSB1lQ

factorize(CSB1lQ)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2B1lQ<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2B1lQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz1","cross_fz1","plan_fz1",
                          "grantlag0111"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1B1lQ <- minimize (TT2B1lQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1B1lQ

PS1B1lQ$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2B1lQ <- minimize (TT2B1lQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "grantlag0111"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2B1lQ
PS2B1lQ$PIchart
PS2B1lQ$SA$M1

factorize(PS2B1lQ)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAB1lQ <- findRows(obj = TT2B1lQ, type = 2)
CSAB1lQ

#INTERMEDIATE SOLUTION with directionality and enhanced solutions

ISB1lQ <- minimize (TT2B1lQ, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                  "grantlag0111"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISB1lQ

factorize(ISB1lQ)

# The inclusion of previous lags as "controlling" condition changes the configurations
# leading to the outcome. --> LAG+cent*CROSS

#------------ANALYSIS  ON NO. OF AVERAGE FAMILIES  AND OUTCOME GRANT ACHIEVEMENT FGH1113F (Patstat 2016)

#conditions are based on 6-point cross-overs
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssA1f<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                 "quifamF"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE
)

fssA1f

pof(1-fssA1f$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnA1f<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quifamF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnA1f

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1A1f <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1A1f

# The are three configurations represented by three companies.
# I lower the inc.cut to 0.87 to avoid PRI<0.5

TT1A1f <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quifamF"),
                    incl.cut1 =0.87,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1A1f


##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSA1f <-minimize (TT1A1f, outcome = "FHG1113F",
                 relation = "suf", incl.cut1 = 0.87,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CSA1f

factorize(CSA1f)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2A1f<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                   relation = "suf", incl.cut = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2A1f

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.87,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1A1f <- minimize (TT2A1f, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1A1f

PS1A1f$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2A1f <- minimize (TT2A1f, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
PS2A1f
PS2A1f$PIchart
PS2A1f$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAA1f <- findRows(obj = TT2A1f, type = 2)
CSAA1f

#INTERMEDIATE SOLUTION with directionality

ISA1f <- minimize (TT2A1f, outcome = "FHG1113F", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz",
                                 "quifamF"),
                  include = c("?"), 
                  dir.exp= "1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
ISA1f

##QUINTILES
#conditions are based on 6-point cross-overs
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssA1fQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                  "quifamF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssA1fQ

pof(1-fssA1fQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnA1fQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quifamF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnA1fQ
#there are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1A1fQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quifamF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1A1fQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSA1fQ <-minimize (TT1A1fQ, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut1 = 0.9,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSA1fQ

factorize(CSA1fQ)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2A1fQ<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quifamF"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2A1fQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1A1fQ <- minimize (TT2A1fQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1A1fQ

PS1A1fQ$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2A1fQ <- minimize (TT2A1fQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2A1fQ
PS2A1fQ$PIchart
PS2A1fQ$SA$M1
PS2A1fQ$SA$M2
#M1 has a higher incl and cov

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAA1fQ <- findRows(obj = TT2A1fQ, type = 2)
CSAA1fQ
#No CSA

#INTERMEDIATE SOLUTION with directionality

ISA1fQ <- minimize (TT2A1fQ, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISA1fQ
#check which counterfactuals were used
ISA1fQ$i.sol$C1P1$EC

#NEGATION OF OUTCOME
TTA1fQN <- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                      relation = "suf", incl.cut = 0.9,
                      conditions= c ("central_fz","cross_fz","plan_fz",
                                     "quifamF"),
                      show.case= TRUE, ddc=TRUE,
                      sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TTA1fQN

#lower the inclusion cutoff
TTA1fQN <- truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                      relation = "suf", incl.cut = c(0.8, 0.6),
                      conditions= c ("central_fz","cross_fz","plan_fz",
                                     "quifamF"),
                      show.case= TRUE, complete = TRUE,
                      sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TTA1fQN

#very low PRI --> I guess it will be very inconsistent

#complex solutions negation of outcome
CSA1fQN <- minimize(TTA1fQN, relation = "suf", incl.cut = 0.75,
                    details = TRUE, show.cases = TRUE,explain ="1")
CSA1fQN
#None of the values in OUT is explained. Please check the truth table.

# #intermediate solutions with directions negation of outcome
# ISA1fQN <- minimize(TTA1fQN,relation = "suf", incl.cut = 0.9,
#                     include = c("?"),
#                     dir.exp = "-,1,1,-", #it does not work if "0, 1, 1, -"
#                     details = TRUE, show.cases = TRUE,explain ="1")
# ISA1fQN
  
  
## 1Bf
## configurations based on _fz1
#conditions are based on 6-point cross-overs
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssB1f<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                                  "quifamF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssB1f

pof(1-fssB1f$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnB1f<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1","quifamF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnB1f

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1B1f <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1f

#lower the inclusion threshold to 0.81
TT1B1f <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    incl.cut1 =0.81,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1f

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSB1f <-minimize (TT1B1f, outcome = "FHG1113F",
                  relation = "suf", incl.cut1 = 0.81,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSB1f

factorize(CSB1f)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2B1f<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                    relation = "suf", incl.cut = 0.81,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2B1f

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.81,
           conditions= c ("central_fz1","cross_fz1","plan_fz1",
                          "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1B1f <- minimize (TT2B1f, outcome = "FHG1113F", 
                    relation = "suf", incl.cut1 = 0.81,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1B1f

PS1B1f$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2B1f <- minimize (TT2B1f, outcome = "FHG1113F", 
                    relation = "suf", incl.cut1 = 0.81,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2B1f
PS2B1f$PIchart
PS2B1f$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAB1f <- findRows(obj = TT2B1f, type = 2)
CSAB1f

#INTERMEDIATE SOLUTION with directionality

ISB1f <- minimize (TT2B1f, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                  "quifamF"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISB1f

## QUINTILES
##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssB1fQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                                  "quifamF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssB1fQ

pof(1-fssB1fQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnB1fQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1","quifamF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnB1fQ
#There are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1B1fQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1fQ

#inrease the inc threshold to 0.93
TT1B1fQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                    "quifamF"),
                     incl.cut1 =0.93,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1B1fQ


##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSB1fQ <-minimize (TT1B1fQ, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut1 = 0.93,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSB1fQ

factorize(CSB1fQ)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2B1fQ<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut = 0.93,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2B1fQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.93,
           conditions= c ("central_fz1","cross_fz1","plan_fz1",
                          "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1B1fQ <- minimize (TT2B1fQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.93,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1B1fQ
#check for the reminders used in the analysis
PS1B1fQ$SA



#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2B1fQ <- minimize (TT2B1fQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.93,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2B1fQ
PS2B1fQ$PIchart
PS2B1fQ$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAB1fQ <- findRows(obj = TT2B1fQ, type = 2)
CSAB1fQ
#no CSA

#INTERMEDIATE SOLUTION with directionality

ISB1fQ <- minimize (TT2B1fQ, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.93,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                  "quifamF"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISB1fQ
ISB1fQ$SA$M1
#this solution uses reminder [7]

#NEGATION OF OUTCOME
TT1B1fQN <-truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                     relation = "suf", incl.cut = 0.9,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                    "quifamF"),
                     show.case= TRUE, complete =TRUE,
                     sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1B1fQN

#lower the inclusion cutoff
TT1B1fQN <-truthTable(IPorg_fuzzy3,outcome = "NQUIFHG1113F",
                     relation = "suf", incl.cut = c(0.8, 0.6),
                     conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                    "quifamF"),
                     show.case= TRUE, complete =TRUE,
                     sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1B1fQN
#there are two configurations for the negation of outcome and lots of Cs

#minimization of complex solutions
CSB1fQN <- minimize(TT1B1fQN, relation = "suf", incl.cut = 0.9,
                    details = TRUE, show.cases = TRUE,explain ="1")

CSB1fQN

#minimization of intermediate solutions with reminders and directionality
ISB1fQN <- minimize(TT1B1fQN, relation = "suf", #incl.cut=0.9,
                    include = c("?"),
                    dir.exp = "-,1,1,-",
                    details = TRUE, show.cases = TRUE,explain ="1")
ISB1fQN
 

#------------ANALYSIS A1p:CONDITIONS ON PREVIOUS SUCESS AND OUTCOME GRANT ACHIEVEMENT FGH1113F (Patstat 2016)

#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssA1p<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                 "FHG0111F"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE
)

fssA1p

pof(1-fssA1p$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnA1p2<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","FHG0111F"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnA1p2

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1A1p <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1A1p

# There are just two configurations that can be included. I lower the threshold to 0.87 to
# avoid cases with PRI<0.5

TT1A1p <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "FHG0111F"),
                    incl.cut1 =0.87,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1A1p

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSA1p <-minimize (TT1A1p, outcome = "FHG1113F",
                 relation = "suf", incl.cut1 = 0.87,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CSA1p

factorize(CSA1p)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2A1p<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                   relation = "suf", incl.cut1 = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2A1p

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.87,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "FHG0111F"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1A1p <- minimize (TT2A1p, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1A1p

PS1A1p$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2A1p <- minimize (TT2A1p, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
PS2A1p
PS2A1p$PIchart
PS2A1p$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAA1p <- findRows(obj = TT2A1p, type = 2)
CSAA1p

#INTERMEDIATE SOLUTION with directionality

ISA1p <- minimize (TT2A1p, outcome = "FHG1113F", 
                  relation = "suf", incl.cut1 = 0.87,
                  conditions= c ("central_fz","cross_fz","plan_fz",
                                 "FHG0111F"),
                  include = c("?"),
                  dir.exp="1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
ISA1p

## QUINTILES
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssA1pQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                  "FHG0111F"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssA1pQ

pof(1-fssA1pQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnA1pQ2<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                      neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","FHG0111F"),
                      relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                      PRI=TRUE)

fssnA1pQ2
#there are no configurations with these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1A1pQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "FHG0111F"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1A1pQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSA1pQ <-minimize (TT1A1pQ, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut1 = 0.9,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSA1pQ

factorize(CSA1pQ)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2A1pQ<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "FHG0111F"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2A1pQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "FHG0111F"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1A1pQ <- minimize (TT2A1pQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "FHG0111F"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1A1pQ

PS1A1pQ$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2A1pQ <- minimize (TT2A1pQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz","cross_fz","plan_fz",
                                   "FHG0111F"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2A1pQ
PS2A1pQ$PIchart
PS2A1pQ$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAA1pQ <- findRows(obj = TT2A1pQ, type = 2)
#no CSA


#INTERMEDIATE SOLUTION with directionality

ISA1pQ <- minimize (TT2A1pQ, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   include = c("?"),
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISA1pQ

#NEGATION OF OUTCOME
TT1A1pQN <- truthTable(IPorg_fuzzy3, outcome = "NQUIFHG1113F",
                       relation = "suf", incl.cut = 0.9,
                       conditions= c ("central_fz","cross_fz","plan_fz",
                                      "FHG0111F"),
                       show.case= TRUE, complete =TRUE,
                       sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1A1pQN

#lower the inc.cut
TT1A1pQN <- truthTable(IPorg_fuzzy3, outcome = "NQUIFHG1113F",
                       relation = "suf", incl.cut = c(0.8,0.6),
                       conditions= c ("central_fz","cross_fz","plan_fz",
                                      "FHG0111F"),
                       show.case= TRUE, complete =TRUE,
                       sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1A1pQN
#there is one configuration with three cases (10,15,18) and lots of Cs

#minimization with negation of outcome and complex solutions
CSA1pQN <- minimize(TT1A1pQN, relation = "suf", incl.cut = 0.9,
                    details = TRUE, show.cases = TRUE,explain ="1")
CSA1pQN

#minimization with negation of outcome and intermediate solutions with directionality
ISA1pQN <- minimize(TT1A1pQN,relation = "suf", incl.cut = 0.9,
                    include = c("?"),
                    dir.exp = "-,1,1,-",
                    details = TRUE, show.cases = TRUE,explain ="1")
ISA1pQN
#One configuration and three companies included. Yet, the PRI is very low. 

##B1P
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssB1p<-superSubset(IPorg_fuzzy3,outcome = "FHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                                  "FHG0111F"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssB1p

pof(1-fssB1p$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnB1p2<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113F", 
                      neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1","FHG0111F"),
                      relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                      PRI=TRUE)

fssnB1p2

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1B1p <-truthTable(IPorg_fuzzy3,outcome = "FHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "FHG0111F"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1p

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSB1p <-minimize (TT1B1p, outcome = "FHG1113F",
                  relation = "suf", incl.cut1 = 0.9,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSB1p

factorize(CSB1p)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2B1p<- truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "FHG0111F"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2B1p

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "FHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz1","cross_fz1","plan_fz1",
                          "FHG0111F"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1B1p <- minimize (TT2B1p, outcome = "FHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "FHG0111F"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1B1p

PS1B1p$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2B1p <- minimize (TT2B1p, outcome = "FHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "FHG0111F"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2B1p
PS2B1p$PIchart
PS2B1p$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAB1p <- findRows(obj = TT2B1p, type = 2)
CSAB1p

#INTERMEDIATE SOLUTION with directionality

ISB1p <- minimize (TT2B1p, outcome = "FHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                  "FHG0111F"),
                   include = c("?"),
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISB1p

##QUINTILES

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssB1pQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFHG1113F", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                                  "FHG0111F"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                    PRI=TRUE
)

fssB1pQ

pof(1-fssB1pQ$coms, outcome = QUIFHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnB1pQ2<-superSubset(IPorg_fuzzy3,outcome = "~QUIFHG1113F", 
                      neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1","FHG0111F"),
                      relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                      PRI=TRUE)

fssnB1pQ2
#There are no configurations for these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1B1pQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "FHG0111F"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1B1pQ

# to consider to increase the inc.cut to 0.97

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSB1pQ <-minimize (TT1B1pQ, outcome = "QUIFHG1113F",
                  relation = "suf", incl.cut1 = 0.9,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)

CSB1pQ

factorize(CSB1pQ)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2B1pQ<- truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "FHG0111F"),
                    complete= TRUE, show.case= TRUE,
                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2B1pQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUIFHG1113F",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz1","cross_fz1","plan_fz1",
                          "FHG0111F"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1B1pQ <- minimize (TT2B1pQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "FHG0111F"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE)
PS1B1pQ

PS1B1pQ$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2B1pQ <- minimize (TT2B1pQ, outcome = "QUIFHG1113F", 
                    relation = "suf", incl.cut1 = 0.9,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                   "FHG0111F"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)
PS2B1pQ
PS2B1pQ$PIchart
PS2B1pQ$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAB1pQ <- findRows(obj = TT2B1pQ, type = 2)
CSAB1pQ
#there is one CSA that should not enter into the intermediate solutions

#INTERMEDIATE SOLUTION with directionality

ISB1pQ <- minimize (TT2B1pQ, outcome = "QUIFHG1113F", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                  "FHG0111F"),
                   include = c("?"), exclude= CSAB1pQ, 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
ISB1pQ

#NEGATION OF OUTCOME
TT1B1pQN <- truthTable(IPorg_fuzzy3, outcome = "NQUIFHG1113F",
                       relation = "suf", incl.cut = 0.9,
                       conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                      "FHG0111F"),
                       show.case= TRUE, complete =TRUE,
                       sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1B1pQN
#there is one configuration (1,1,1,0) represented by cases 3 and 12. they did not contribute to the 
#presence of outcome

#minimization of complex solutions with negation of outcome
CSB1pQN <- minimize(TT1B1pQN, relation = "suf", incl.cut = 0.9,
                    details = TRUE, show.cases = TRUE,explain ="1")
CSB1pQN

#minimization of intermediate solutions with directionality
ISB1pQN <- minimize(TT1B1pQN,relation = "suf", incl.cut = 0.9,
                    include = c("?"),
                    dir.exp = "-,1,1,-",
                    details = TRUE, show.cases = TRUE,explain ="1")
ISB1pQN


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
##                                      ANALYSIS ON FAST GRANT ACHIEVEMENT (PATSTAT 2016)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#Fa: outcome Fast16
#First attempt is inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFa<-superSubset(IPorg_fuzzy3,outcome = "Fast16",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssFa

pof(1-fssFa$coms, outcome = FHG1113F,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFa<-superSubset(IPorg_fuzzy3,outcome = "~Fast16",
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnFa

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1Fa <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fa

#I lower the threshold to 0.8 and still avoid PRI<0.5
TT1Fa <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    incl.cut =0.8,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fa

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFa <-minimize (TT1Fa1, outcome = "Fast16",
                  relation = "suf", incl.cut = 0.8,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)
CSFa

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2Fa <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz"),
                    incl.cut =0.8,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT2Fa

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "Fast16", relation= "suff", incl.cut = 0.8,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1Fa <- minimize(TT2Fa, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
  
)

PS1Fa
PS1Fa$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2Fa <- minimize(TT2Fa, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

PS2Fa
PS2Fa$PIchart
PS2Fa$SA$MI

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAFa <- findRows(obj = TT2Fa , type = 2)
CSAFa

#enhanced parsimonious solutions
PSEFa <- minimize(TT2Fa, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), exclude= CSAFa, all.sol = FALSE,
                  row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

PSEFa

#intermediate solutions with directionality
#enhanced parsimonious solutions
ISFa <- minimize(TT2Fa, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), 
                  dir.exp= "1,1,1",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

ISFa

##  QUINTILES
#FaQ: outcome QUIFast16
#First attempt is inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFaQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFast16",
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                   PRI=TRUE
)

fssFaQ

pof(1-fssFaQ$coms, outcome = QUIFast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFaQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFast16",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnFaQ
#There are no configurations using these cutoffs values.

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1FaQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1FaQ


##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFaQ <-minimize (TT1FaQ, outcome = "QUIFast16",
                 relation = "suf", incl.cut = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)
CSFaQ

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2FaQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   incl.cut =0.9,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT2FaQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "QUIFast16", relation= "suff", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1FaQ <- minimize(TT2FaQ, outcome= "QUIFast16",
                  relation = "suf", incl.cut = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

PS1FaQ
PS1FaQ$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2FaQ <- minimize(TT2FaQ, outcome= "QUIFast16",
                  relation = "suf", incl.cut = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

PS2FaQ
PS2FaQ$PIchart
PS2FaQ$SA$MI

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAFaQ <- findRows(obj = TT2FaQ , type = 2)
CSAFaQ
#no CSA

# #enhanced parsimonious solutions
# PSEFa <- minimize(TT2Fa, outcome= "Fast16",
#                   relation = "suf", incl.cut = 0.8,
#                   conditions= c ("central_fz","cross_fz","plan_fz"),
#                   include = c("?"), exclude= CSAFa, all.sol = FALSE,
#                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                   use.letters = FALSE
#                   
# )
# 
# PSEFa

#intermediate solutions with directionality
#enhanced parsimonious solutions
ISFaQ <- minimize(TT2FaQ, outcome= "QUIFast16",
                 relation = "suf", incl.cut = 0.9,
                 conditions= c ("central_fz","cross_fz","plan_fz"),
                 include = c("?"), 
                 dir.exp= "1,1,-",
                 details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                 use.letters = FALSE
                 
)

ISFaQ

#check for the reminders used
ISFaQ$SA$M1
#it used the only reminder available
ISFaQ$SA$M2
#It did not use any reminder

#Negation of outcome
TT1FaQN <- truthTable(IPorg_fuzzy3, outcome = "NQUIFast16",
                      relation= "suf", incl.cut = 0.9,
                      conditions = c("central_fz","cross_fz","plan_fz"),
                      complete = TRUE, show.case= TRUE, 
                      sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1FaQN
#too low inclusion scores to proceed with the negation of outcome analysis. 

#Fb: ooutcome Fast16 and calibration based on _fz1
#First attempt is inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFb<-superSubset(IPorg_fuzzy3,outcome = "Fast16",
                   neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                   PRI=TRUE
)

fssFb

pof(1-fssFb$coms, outcome = Fast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFb<-superSubset(IPorg_fuzzy3,outcome = "~Fast16",
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnFb

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1Fb <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1Fb

#I cannot lower the threshold to 0.8 and avoid PRI<0.5 --> this analysis does not make sense. 

##  QUINTILES
#FbQ: outcome QUIFast16
#First attempt is inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFbQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFast16",
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssFbQ

pof(1-fssFbQ$coms, outcome = QUIFast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFbQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFast16",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnFbQ
#There are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1FbQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1FbQ

#fix the threshold to 0.96 --> if you fix to 0.99 it is very limited in terms of coverage
#and it can have limited meaning from an empirical point of view. 

TT1FbQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                    incl.cut1 =0.96,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1FbQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFbQ <-minimize (TT1FbQ, outcome = "QUIFast16",
                  relation = "suf", incl.cut = 0.96,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)
CSFbQ
#If you try to change the threshold to 0.954 --> just one configuration CENT

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2FbQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                    incl.cut =0.96,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT2FbQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "QUIFast16", relation= "suff", incl.cut = 0.96,
           conditions= c ("central_fz1","cross_fz1","plan_fz1"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1FbQ <- minimize(TT2FbQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 0.96,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS1FbQ
PS1FbQ$SA
#there is one reminder in the solutions. 

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2FbQ <- minimize(TT2FbQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 0.96,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS2FbQ
PS2FbQ$PIchart
PS2FbQ$SA$MI
#no reminders used

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAFbQ <- findRows(obj = TT2FbQ , type = 2)
CSAFbQ
#No CSA

# #enhanced parsimonious solutions
# PSEFa <- minimize(TT2Fa, outcome= "Fast16",
#                   relation = "suf", incl.cut = 0.8,
#                   conditions= c ("central_fz1","cross_fz1","plan_fz1"),
#                   include = c("?"), exclude= CSAFa, all.sol = FALSE,
#                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                   use.letters = FALSE
#                   
# )
# 
# PSEFa

#intermediate solutions with directionality
#Error message 
#Error in minimize(TT2FbQ, outcome = "QUIFast16", relation = "suf", incl.cut = 0.96,  : 
#INTEGER() can only be applied to a 'integer', not a 'NULL'
# I guess the reason is that in the minimization with the reminders it does not use actually the reminders
 

ISFbQ <- minimize(TT2FbQ, outcome= "QUIFast16",
                  relation = "suf", incl.cut = 0.96,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                  include = c("?"), 
                  dir.exp= "1,1,-", 
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

ISFbQ
#then I tried with the eqmcc 
ISFbQ <- eqmcc(TT2FbQ, outcome= "QUIFast16",
               relation = "suf", incl.cut = 0.96,
               conditions= c ("central_fz1","cross_fz1","plan_fz1"),
               include = c("?"), 
               dir.exp= "1,1,-", 
               details = TRUE, show.cases = TRUE, use.tilde = FALSE,
               use.letters = FALSE)
#It still returns an error and tells that directional expectations cancel each other out to an empty space


#--------------------CONTROLS
#Fac: Fast grant and citations

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFac<-superSubset(IPorg_fuzzy3,outcome = "Fast16",
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                   PRI=TRUE
)

fssFac

pof(1-fssFac$coms, outcome = Fast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFac<-superSubset(IPorg_fuzzy3,outcome = "~Fast16",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnFac

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1Fac <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1Fac

#I lower the threshold to 0.85 and still avoid PRI<0.5
TT1Fac <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                   incl.cut =0.85,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1Fac

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFac <-minimize (TT1Fac, outcome = "Fast16",
                 relation = "suf", incl.cut = 0.85,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)
CSFac

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2Fac <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                   incl.cut =0.85,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT2Fac

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "Fast16", relation= "suff", incl.cut = 0.85,
           conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1Fac <- minimize(TT2Fac, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.85,
                  conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

PS1Fac
PS1Fac$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2Fac <- minimize(TT2Fac, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.85,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

PS2Fac
PS2Fac$PIchart
PS2Fac$SA$M4
PS2Fac$SA$M1
#M4 uses less reminders

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAFac <- findRows(obj = TT2Fac , type = 2)
CSAFac

#enhanced parsimonious solutions
PSEFac <- minimize(TT2Fac, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.85,
                  conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                  include = c("?"), exclude= CSAFac, all.sol = FALSE,
                  row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

PSEFac

#intermediate solutions with directionality
#enhanced parsimonious solutions
ISFac <- minimize(TT2Fac, outcome= "Fast16",
                 relation = "suf", incl.cut = 0.85,
                 conditions= c ("central_fz","cross_fz","plan_fz"),
                 include = c("?"), 
                 dir.exp= "1,1,-,-",
                 details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                 use.letters = FALSE
                 
)

ISFac

## QUINTILES
##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFacQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFast16",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssFacQ

pof(1-fssFacQ$coms, outcome = QUIFast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFacQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFast16",
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnFacQ
#there are no configurations using these cutffs values

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1FacQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1FacQ

#IF I use an incl of 1 --> low coverage --> incl of 0.98
TT1FacQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                    incl.cut =0.98,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1FacQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFacQ <-minimize (TT1FacQ, outcome = "QUIFast16",
                  relation = "suf", incl.cut = 0.98,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)
CSFacQ

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2FacQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                    incl.cut =0.98,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT2FacQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "QUIFast16", relation= "suff", incl.cut = 0.98,
           conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1FacQ <- minimize(TT2FacQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 0.98,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS1FacQ
PS1FacQ$SA
#there is a difference between M2 and M4: M1 uses a SA that makes little sense (0,0,0,0)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2FacQ <- minimize(TT2FacQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 0.98,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS2FacQ
PS2FacQ$PIchart
PS2FacQ$SA$M1
PS2FacQ$SA$M4
#let's use M4 because it uses less SA and has high incl

# analysis of the counterFacQtuals: to exclude implausible counterfactuals
CSAFacQ <- findRows(obj = TT2FacQ , type = 2)
CSAFacQ
#no CSA

# #enhanced parsimonious solutions
# PSEFacQ <- minimize(TT2FacQ, outcome= "QUIFast16",
#                    relation = "suf", incl.cut = 1,
#                    conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
#                    include = c("?"), exclude= CSAFacQ, all.sol = FALSE,
#                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE
#                    
# )
# 
# PSEFacQ

#intermediate solutions with directionality
#enhanced parsimonious solutions
ISFacQ <- minimize(TT2FacQ, outcome= "QUIFast16",
                  relation = "suf", incl.cut = 1,
                  conditions= c ("central_fz","cross_fz","plan_fz", "quicitF"),
                  include = c("?"), 
                  dir.exp= "1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

ISFacQ
ISFacQ$SA$M1


#NEGATION OF THE OUTCOME
TT1FacQN <- truthTable(IPorg_fuzzy3, outcome = "NQUIFast16",
                       relation = "suf", incl.cut = 0.9,
                       conditions= c ("central_fz","cross_fz","plan_fz",
                                      "quicitF"),
                       complete = TRUE, show.case= TRUE, 
                       sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1FacQN

#lower the threshold 
TT1FacQN <- truthTable(IPorg_fuzzy3, outcome = "NQUIFast16",
                       relation = "suf", incl.cut = c(0.8, 0.6),
                       conditions= c ("central_fz","cross_fz","plan_fz",
                                      "quicitF"),
                       complete = TRUE, show.case= TRUE, 
                       sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT1FacQN
#it returns a lot of 0s and Cs --> in our analysis, if there is a C, we will treat it as a 0
#it makes sense because the Cs have very low PRI --> it would be inconsistent in any case.
#we are not progressing further.

#Fbc: fast grant and citations, calibration on _fz1


##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFbc<-superSubset(IPorg_fuzzy3,outcome = "Fast16",
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssFbc

pof(1-fssFbc$coms, outcome = Fast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFbc<-superSubset(IPorg_fuzzy3,outcome = "~Fast16",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnFbc

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1Fbc <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fbc

#XXX TO CONTINUE


## QUINTILES
##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFbcQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFast16",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                     PRI=TRUE
)

fssFbcQ

pof(1-fssFbcQ$coms, outcome = QUIFast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFbcQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFast16",
                      neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                      relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                      PRI=TRUE)

fssnFbcQ
#there are no configurations using these cutoffs values

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1FbcQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                     incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FbcQ

#I use an incl of 1 --> coverage around 0.4 --> lower to 0.99
TT1FbcQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                     incl.cut =0.99,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FbcQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFbcQ <-minimize (TT1FbcQ, outcome = "QUIFast16",
                   relation = "suf", incl.cut = 0.99,
                   explain = "1",include = "1",
                   row.dom = TRUE, all.sol= FALSE,
                   details = TRUE, show.cases = TRUE)
CSFbcQ

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2FbcQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                     incl.cut =0.99,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT2FbcQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "QUIFast16", relation= "suff", incl.cut = 0.99,
           conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1FbcQ <- minimize(TT2FbcQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.99,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PS1FbcQ
PS1FbcQ$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2FbcQ <- minimize(TT2FbcQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.99,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PS2FbcQ
PS2FbcQ$PIchart
PS2FbcQ$SA$MI

# analysis of the counterFactuals: to exclude implausible counterFbctuals
CSAFbcQ <- findRows(obj = TT2FbcQ , type = 2)
CSAFbcQ
#no CSA

#enhanced parsimonious solutions
PSEFbcQ <- minimize(TT2FbcQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.99,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PSEFbcQ


#intermediate solutions with directionality
ISFbcQ <- minimize(TT2FbcQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 0.99,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "quicitF"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

ISFbcQ

#NEGATION OF OUTCOME
TT2FbcQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                       relation = "suf", incl.cut = 0.9,
                       conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                      "quicitF"),
                       show.case= TRUE, complete =TRUE,
                       sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FbcQN
#There are lots of 0s and very low PRI --> possible problem of inconsistency
#lower inclusion score
TT2FbcQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                       relation = "suf", c(0.8, 0.6),
                       conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                      "quicitF"),
                       show.case= TRUE, complete =TRUE,
                       sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FbcQN
# Four 0s become C --> it does not make sense to continue, given the low PRI


###---------------------------------
#### FAMILIES
#Faf: Fast grant and families

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFaf<-superSubset(IPorg_fuzzy3,outcome = "Fast16",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssFaf

pof(1-fssFaf$coms, outcome = Fast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFaf<-superSubset(IPorg_fuzzy3,outcome = "~Fast16",
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnFaf
#there are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1Faf <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Faf

#I lower the threshold to 0.81 but there are configurations with  PRI<0.5
TT1Faf <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                    incl.cut =0.81,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Faf

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFaf <-minimize (TT1Faf, outcome = "Fast16",
                  relation = "suf", incl.cut = 0.81,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)
CSFaf

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2Faf <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                    incl.cut =0.81,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT2Faf

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "Fast16", relation= "suff", incl.cut = 0.81,
           conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1Faf <- minimize(TT2Faf, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.81,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS1Faf
PS1Faf$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2Faf <- minimize(TT2Faf, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.87,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS2Faf
PS2Faf$PIchart
PS2Faf$SA$MI

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAFaf <- findRows(obj = TT2Faf , type = 2)
CSAFaf

#enhanced parsimonious solutions
PSEFaf <- minimize(TT2Faf, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.81,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                   include = c("?"), exclude= CSAFaf, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PSEFaf

#intermediate solutions with directionality
ISFaf <- minimize(TT2Faf, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.81,
                  conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                  include = c("?"), 
                  dir.exp= "1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

ISFaf

## QUINTILES
##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFafQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFast16",
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                     PRI=TRUE
)

fssFafQ

pof(1-fssFafQ$coms, outcome = QUIFast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFafQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFast16",
                      neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                      relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                      PRI=TRUE)

fssnFafQ
#There are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1FafQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                     incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FafQ

#I use an incl of 0.96
TT1FafQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                     incl.cut =0.96,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FafQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFafQ <-minimize (TT1FafQ, outcome = "QUIFast16",
                   relation = "suf", incl.cut = 0.96,
                   explain = "1",include = "1",
                   row.dom = TRUE, all.sol= FALSE,
                   details = TRUE, show.cases = TRUE)
CSFafQ

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2FafQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                     incl.cut =0.96,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT2FafQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "QUIFast16", relation= "suff", incl.cut = 0.96,
           conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1FafQ <- minimize(TT2FafQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.96,
                    conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PS1FafQ
PS1FafQ$SA



#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2FafQ <- minimize(TT2FafQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.96,
                    conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PS2FafQ
PS2FafQ$PIchart
PS2FafQ$SA$MI

# analysis of the counterFafQtuals: to exclude implausible counterFaftuals
CSAFafQ <- findRows(obj = TT2FafQ , type = 2)
CSAFafQ
#No CSA

#enhanced parsimonious solutions --> there is no need for this
# PSEFafQ <- minimize(TT2FaQ, outcome= "QUIFast16",
#                     relation = "suf", incl.cut = 1,
#                     conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
#                     include = c("?"), exclude= CSAFafQ, all.sol = FALSE,
#                     row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                     use.letters = FALSE
#                     
# )
#PSEFafQ

#intermediate solutions with directionality
#enhanced parsimonious solutions
ISFafQ <- minimize(TT2FafQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 1,
                   conditions= c ("central_fz","cross_fz","plan_fz", "quifamF"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

ISFafQ

#NEGATION OF OUTCOME
#Truthtable
TT2FafQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                        relation = "suf", incl.cut = 0.9,
                        conditions= c ("central_fz","cross_fz","plan_fz",
                                       "quifamF"),
                        show.case= TRUE, complete =TRUE,
                        sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FafQN
#No 1s, just 0s
#lower inclusion score
TT2FafQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                        relation = "suf", incl.cut = c(0.8, 0.6),
                        conditions= c ("central_fz","cross_fz","plan_fz",
                                       "quifamF"),
                        show.case= TRUE, complete =TRUE,
                        sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FafQN
#Four 0s become Cs [cases: 19, 2, 14, 20, 17] --> the PRI is very low and 
#continuing with the minimization might cause inconsistent results


#Fbf: fast grant and families, calibration on _fz1


##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFbf<-superSubset(IPorg_fuzzy3,outcome = "Fast16",
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssFbf
#The inclusion score is sufficiently high and the coverage is not below 0.6

pof(1-fssFbf$coms, outcome = Fast16,IPorg_fuzzy3, relation = "necessity")
#very low inclusion score

#negation of outcome of all minimal sufficiency combinations

fssnFbf<-superSubset(IPorg_fuzzy3,outcome = "~Fast16",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnFbf
#there are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1Fbf <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fbf

#I lower the threshold to 0.79 but I cannot avoid PRI<0.5
TT1Fbf <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                    incl.cut =0.79,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fbf

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFbf <-minimize (TT1Fbf, outcome = "Fast16",
                  relation = "suf", incl.cut = 0.79,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)
CSFbf

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2Fbf <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                    incl.cut =0.79,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT2Fbf

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "Fast16", relation= "suff", incl.cut = 0.79,
           conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1Fbf <- minimize(TT2Fbf, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.79,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS1Fbf
PS1Fbf$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2Fbf <- minimize(TT2Fbf, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.79,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS2Fbf
PS2Fbf$PIchart
PS2Fbf$SA$MI

# analysis of the counterFbftuals: to exclude implausible counterFbftuals
CSAFbf <- findRows(obj = TT2Fbf , type = 2)
CSAFbf

#enhanced parsimonious solutions
PSEFbf <- minimize(TT2Fbf, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.79,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                   include = c("?"), exclude= CSAFbf, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PSEFbf

#intermediate solutions with directionality

ISFbf <- minimize(TT2Fbf, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.79,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                  include = c("?"),
                  dir.exp= "1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE

)

ISFbf

## QUINTILES
##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFbfQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFast16",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                     PRI=TRUE
)

fssFbfQ
#The inclusion is above 0.9 and the coverage above 0.8

pof(1-fssFbfQ$coms, outcome = QUIFast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFbfQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFast16",
                      neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                      relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                      PRI=TRUE)

fssnFbfQ
#There are no configurations using these cutoffs values

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1FbfQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                     incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FbfQ

#I use an incl of 0.99 to capture configurations with PRI>0.9
TT1FbfQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                     incl.cut =0.99,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FbfQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFbfQ <-minimize (TT1FbfQ, outcome = "QUIFast16",
                   relation = "suf", incl.cut = 0.99,
                   explain = "1",include = "1",
                   row.dom = TRUE, all.sol= FALSE,
                   details = TRUE, show.cases = TRUE)
CSFbfQ

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2FbfQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                     incl.cut =0.99,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT2FbfQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "QUIFast16", relation= "suff", incl.cut = 0.99,
           conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1FbfQ <- minimize(TT2FbfQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.99,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
)

PS1FbfQ
PS1FbfQ$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2FbfQ <- minimize(TT2FbfQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.99,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    )

PS2FbfQ
PS2FbfQ$PIchart
PS2FbfQ$SA$MI

# analysis of the counterfactuals: to exclude implausible counterFbftuals
CSAFbfQ <- findRows(obj = TT2FbfQ , type = 2)
CSAFbfQ
#No CSA

#enhanced parsimonious solutions
PSEFbfQ <- minimize(TT2FbfQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.99,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                    include = c("?"), 
                    #exclude= CSAFbfQ, 
                    all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PSEFbfQ

#intermediate solutions with directionality
ISFbfQ <- minimize(TT2FbfQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 0.99,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "quifamF"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   )

ISFbfQ

#NEGATION OF OUTCOME
#truthtable
TT2FbfQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                       relation = "suf", incl.cut = 0.9,
                       conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                      "quifamF"),
                       show.case= TRUE, complete =TRUE,
                       sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FbfQN
#very low inclusion scores and PRI=0 --> does not make sense to continue
sink()
#-----------------------
###### PAST VELOCITY
#Fav abd Fbv

#Fav: Fast grant and pas velocity

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFav<-superSubset(IPorg_fuzzy3,outcome = "Fast16",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssFav

pof(1-fssFav$coms, outcome = Fast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFav<-superSubset(IPorg_fuzzy3,outcome = "~Fast16",
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnFav

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1Fav <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fav

#If I lower the threshold to 0.85 I cannot avoid PRI<0.5
TT1Fav <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                    incl.cut =0.85,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fav

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFav <-minimize (TT1Fav, outcome = "Fast16",
                  relation = "suf", incl.cut = 0.85,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)
CSFav

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2Fav <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                    incl.cut =0.85,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT2Fav

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "Fast16", relation= "suff", incl.cut = 0.85,
           conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1Fav <- minimize(TT2Fav, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.85,
                   conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS1Fav
PS1Fav$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2Fav <- minimize(TT2Fav, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.85,
                   conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)

PS2Fav
PS2Fav$PIchart
PS2Fav$SA$MI

# analysis of the counterFavtuals: to exclude implausible counterFavtuals
CSAFav <- findRows(obj = TT2Fav , type = 2)
CSAFav

#enhanced parsimonious solutions
PSEFav <- minimize(TT2Fav, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.85,
                   conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                   include = c("?"), exclude= CSAFav, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PSEFav

#intermediate solutions with directionality

ISFav <- minimize(TT2Fav, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz","Fast0110"),
                  include = c("?"), 
                  dir.exp= "1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

ISFav

## QUINTILES
##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFavQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFast16",
                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                     PRI=TRUE
)

fssFavQ
#The inclusion score is above 0.9 and the coverage above 0.8

pof(1-fssFavQ$coms, outcome = QUIFast16,IPorg_fuzzy3, relation = "necessity")
#very low inclusion point

#negation of outcome of all minimal sufficiency combinations

fssnFavQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFast16",
                      neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                      relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                      PRI=TRUE)

fssnFavQ
#there are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1FavQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                     incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FavQ

#I use an incl of 0.97
TT1FavQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                     incl.cut =0.97,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FavQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFavQ <-minimize (TT1FavQ, outcome = "QUIFast16",
                   relation = "suf", incl.cut = 0.97,
                   explain = "1",include = "1",
                   row.dom = TRUE, all.sol= FALSE,
                   details = TRUE, show.cases = TRUE)
CSFavQ

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2FavQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                     incl.cut =0.97,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT2FavQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "QUIFast16", relation= "suff", incl.cut = 0.97,
           conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1FavQ <- minimize(TT2FavQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.97,
                    conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PS1FavQ
PS1FavQ$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2FavQ <- minimize(TT2FavQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 1,
                    conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PS2FavQ
PS2FavQ$PIchart
PS2FavQ$SA$MI

# analysis of the counterFavQtuals: to exclude implausible counterFavtuals
CSAFavQ <- findRows(obj = TT2FavQ , type = 2)
CSAFavQ
#No CSA

#enhanced parsimonious solutions
PSEFavQ <- minimize(TT2FavQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.8,
                    conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                    include = c("?"), 
                    #exclude= CSAFavQ, 
                    all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PSEFavQ

#intermediate solutions with directionality
ISFavQ <- minimize(TT2FavQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 0.97,
                   conditions= c ("central_fz","cross_fz","plan_fz", "Fast0110"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

ISFavQ
#NEGATION OF OUTCOME
TT2FavQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                        relation = "suf", incl.cut = 0.9,
                        conditions= c ("central_fz","cross_fz","plan_fz",
                                       "Fast0110"),
                        show.case= TRUE, complete =TRUE,
                        sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FavQN

#lower inclusion rate
TT2FavQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                        relation = "suf", incl.cut = c(0.8, 0.6),
                        conditions= c ("central_fz","cross_fz","plan_fz",
                                       "Fast0110"),
                        show.case= TRUE, complete =TRUE,
                        sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FavQN
#too low inclusion score (just about 0.6) and very low PRI (0.2)

#Fbv: fast grant and past performane, calibration on _fz1

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFbv<-superSubset(IPorg_fuzzy3,outcome = "Fast16",
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                    PRI=TRUE
)

fssFbv

pof(1-fssFbv$coms, outcome = Fast16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnFbv<-superSubset(IPorg_fuzzy3,outcome = "~Fast16",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                     PRI=TRUE)

fssnFbv

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1Fbv <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fbv

#I cannot lower the threshold to 0.79 and still avoid PRI<0.5
TT1Fbv <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                    incl.cut =0.79,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT1Fbv

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFbv <-minimize (TT1Fbv, outcome = "Fast16",
                  relation = "suf", incl.cut = 0.79,
                  explain = "1",include = "1",
                  row.dom = TRUE, all.sol= FALSE,
                  details = TRUE, show.cases = TRUE)
CSFbv

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2Fbv <-truthTable(IPorg_fuzzy3,outcome = "Fast16", neg.out = FALSE,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                    incl.cut =0.79,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                    show.cases = TRUE
)

TT2Fbv

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "Fast16", relation= "suff", incl.cut = 0.79,
           conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1Fbv <- minimize(TT2Fbv, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.79,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS1Fbv
PS1Fbv$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2Fbv <- minimize(TT2Fbv, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.79,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PS2Fbv
PS2Fbv$PIchart
PS2Fbv$SA$MI

# analysis of the counterFbvtuals: to exclude implausible counterFbvtuals
CSAFbv <- findRows(obj = TT2Fbv , type = 2)
CSAFbv

#enhanced parsimonious solutions
PSEFbv <- minimize(TT2Fbv, outcome= "Fast16",
                   relation = "suf", incl.cut = 0.85,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                   include = c("?"), exclude= CSAFbv, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

PSEFbv

#intermediate solutions with directionality
ISFbv <- minimize(TT2Fbv, outcome= "Fast16",
                  relation = "suf", incl.cut = 0.8,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                  include = c("?"), 
                  dir.exp= "1,1,-,-",
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
                  
)

ISFbv

## QUINTILES
##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssFbvQ<-superSubset(IPorg_fuzzy3,outcome = "QUIFast16",
                     neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                     PRI=TRUE
)

fssFbvQ
#the inclusion is above 0.9 and the coverage above 0.8

pof(1-fssFbvQ$coms, outcome = QUIFast16,IPorg_fuzzy3, relation = "necessity")
#the inclusion is around 0.2

#negation of outcome of all minimal sufficiency combinations

fssnFbvQ<-superSubset(IPorg_fuzzy3,outcome = "~QUIFast16",
                      neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                      relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                      PRI=TRUE)

fssnFbvQ
#There are no configurations using these cutoffs

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1FbvQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                     incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FbvQ

#I use an incl of 0.99
TT1FbvQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                     incl.cut =0.99,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT1FbvQ

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSFbvQ <-minimize (TT1FbvQ, outcome = "QUIFast16",
                   relation = "suf", incl.cut = 0.99,
                   explain = "1",include = "1",
                   row.dom = TRUE, all.sol= FALSE,
                   details = TRUE, show.cases = TRUE)
CSFbvQ

##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2FbvQ <-truthTable(IPorg_fuzzy3,outcome = "QUIFast16", neg.out = FALSE,
                     conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                     incl.cut =0.99,  complete = TRUE, sort.by = c("incl", "n"),decreasing = TRUE,
                     show.cases = TRUE
)

TT2FbvQ

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table
truthTable(IPorg_fuzzy3,outcome = "QUIFast16", relation= "suff", incl.cut = 0.99,
           conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE
)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1FbvQ <- minimize(TT2FbvQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.99,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PS1FbvQ
PS1FbvQ$SA

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2FbvQ <- minimize(TT2FbvQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 0.99,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                    include = c("?"), all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PS2FbvQ
PS2FbvQ$PIchart
PS2FbvQ$SA$MI

# analysis of the counterFbvQtuals: to exclude implausible counterFbvtuals
CSAFbvQ <- findRows(obj = TT2FbvQ , type = 2)
CSAFbvQ
#No CSA

#enhanced parsimonious solutions
PSEFbvQ <- minimize(TT2FaQ, outcome= "QUIFast16",
                    relation = "suf", incl.cut = 1,
                    conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                    include = c("?"), 
                    #exclude= CSAFbvQ, 
                    all.sol = FALSE,
                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                    use.letters = FALSE
                    
)

PSEFbvQ

#intermediate solutions with directionality
#enhanced parsimonious solutions
ISFbvQ <- minimize(TT2FbvQ, outcome= "QUIFast16",
                   relation = "suf", incl.cut = 0.99,
                   conditions= c ("central_fz1","cross_fz1","plan_fz1", "Fast0110"),
                   include = c("?"), 
                   dir.exp= "1,1,-,-",
                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
                   
)

ISFbvQ
#NEGATION OF OUTCOME
TT2FbvQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                        conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                       "Fast0110"),
                        complete = TRUE, show.case= TRUE, 
                        sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FbvQN
#Just 0s, in any case, the inc is very low and the PRI almost equal to 0 --> inconsistency?

#lower the threshold
TT2FbvQN <- truthTable (IPorg_fuzzy3, outcome = "NQUIFast16",
                        relation = "suf", incl.cut = c(0.8, 0.6),
                        conditions= c ("central_fz1","cross_fz1","plan_fz1",
                                       "Fast0110"),
                        complete = TRUE, show.case= TRUE, 
                        sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2FbvQN
sink()

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
##                                      ANALYSIS ON SLOW GRANT ACHIEVEMENT (PATSTAT 2016)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#---------------------------------------ANALYSIS 2a ON GRANTLAG (PATSTAT 2016)--------------------------------------------

#outcome on patentlag16 (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6: there are no configurations using these cutoffs values

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.8 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fss2a<-superSubset(IPorg_fuzzy3,outcome = "grantlag16", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                   relation="nec", incl.cut=0.8, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE)

fss2a

pof(1-fss1a$coms, outcome = grantlag16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn2a<-superSubset(IPorg_fuzzy3,outcome = "~grantlag16", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                    relation="nec", incl.cut=0.8, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssn2a

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT2a <-truthTable(IPorg_fuzzy3,outcome = "grantlag16", neg.out = FALSE,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  incl.cut1 =0.8,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                  show.cases = TRUE)

TT2a

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CS2a <-minimize (TT2a, outcome = "grantlag16",
                 relation = "suf", incl.cut1 = 0.8,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CS2a

factorize(CS2a)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2a2<- truthTable(IPorg_fuzzy3,outcome = "grantlag16",
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2a2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "grantlag16",
           relation = "suf", incl.cut = 0.8,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2a <- minimize (TT2a2, outcome = "grantlag16", 
                  relation = "suf", incl.cut1 = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE)
PS2a

PS2a$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2a2 <- minimize (TT2a2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

PS2a2
PS2a2$PIchart
PS2a2$SA$M1

factorize(PS2a2)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA2a <- findRows(obj = TT2a2, type = 2)
CSA2a

#enhanced parsimonious solution
PS2aE <- minimize (TT2a2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), exclude = CSA2a, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS2aE

#INTERMEDIATE SOLUTION with directionality

IS2a <- minimize (TT2a2, outcome = "grantlag16", 
                  relation = "suf", incl.cut1 = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"),
                  dir.exp= c(1,1,0),  
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
IS2a
#----------------------------------------------------------------------------------------------------------------------
#---------------------------------------ANALYSIS 2a ON GRANTLAG (PATSTAT 2016)--------------------------------------------

#conditions are based on 6-point cross-overs
#outcome on patentlag16 (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6: there are no configurations using these cutoffs values

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.8 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fss2a<-superSubset(IPorg_fuzzy3,outcome = "QUILag16", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                   relation="nec", incl.cut=0.8, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE)

fss2a

pof(1-fss1a$coms, outcome = QUILag16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn2a<-superSubset(IPorg_fuzzy3,outcome = "~QUILag16", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz"),
                    relation="nec", incl.cut=0.8, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssn2a

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT2a <-truthTable(IPorg_fuzzy3,outcome = "QUILag16", neg.out = FALSE,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                  show.cases = TRUE)

TT2a

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CS2a <-minimize (TT2a, outcome = "QUILag16",
                 relation = "suf", incl.cut1 = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CS2a

factorize(CS2a)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2a2<- truthTable(IPorg_fuzzy3,outcome = "QUILag16",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2a2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "QUILag16",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2a <- minimize (TT2a2, outcome = "QUILag16", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE)
PS2a

PS2a$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2a2 <- minimize (TT2a2, outcome = "QUILag16", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

PS2a2
PS2a2$PIchart
PS2a2$SA$M1

factorize(PS2a2)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA2a <- findRows(obj = TT2a2, type = 2)
CSA2a

#enhanced parsimonious solution
PS2aE <- minimize (TT2a2, outcome = "QUILag16", 
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), exclude = CSA2a, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS2aE

#INTERMEDIATE SOLUTION with directionality

IS2a <- minimize (TT2a2, outcome = "QUILag16", 
                  relation = "suf", incl.cut1 = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"),
                  dir.exp= c(1,1,0),  
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
IS2a
#---------------------------------------ANALYSIS 2b ON GRANTLAG (PATSTAT 2016)--------------------------------------------

#conditions are based on 6-point cross-overs
#outcome on patentlag16 (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6: there are no configurations using these cutoffs values

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.8 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fss2b<-superSubset(IPorg_fuzzy3,outcome = "grantlag16", 
                   neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                   relation="nec", incl.cut=0.8, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE)

fss2b

pof(1-fss2b$coms, outcome = grantlag16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssn2b<-superSubset(IPorg_fuzzy3,outcome = "~grantlag16", 
                    neg.out=FALSE, conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                    relation="nec", incl.cut=0.8, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssn2b

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT2b <-truthTable(IPorg_fuzzy3,outcome = "grantlag16", neg.out = FALSE,
                  conditions= c ("central_fz1","cross_fz1","plan_fz1"),
                  incl.cut1 =0.8,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                  show.cases = TRUE)

TT2a

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CS2a <-minimize (TT2a, outcome = "grantlag16",
                 relation = "suf", incl.cut1 = 0.8,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CS2a

factorize(CS2a)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2a2<- truthTable(IPorg_fuzzy3,outcome = "grantlag16",
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2a2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "grantlag16",
           relation = "suf", incl.cut = 0.8,
           conditions= c ("central_fz","cross_fz","plan_fz"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS2a <- minimize (TT2a2, outcome = "grantlag16", 
                  relation = "suf", incl.cut1 = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"), all.sol = FALSE,
                  row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE)
PS2a

PS2a$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2a2 <- minimize (TT2a2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)

PS2a2
PS2a2$PIchart
PS2a2$SA$M1

factorize(PS2a2)

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSA2a <- findRows(obj = TT2a2, type = 2)
CSA2a

#enhanced parsimonious solution
PS2aE <- minimize (TT2a2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.8,
                   conditions= c ("central_fz","cross_fz","plan_fz"),
                   include = c("?"), exclude = CSA2a, all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS2aE

#INTERMEDIATE SOLUTION with directionality

IS2a <- minimize (TT2a2, outcome = "grantlag16", 
                  relation = "suf", incl.cut1 = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz"),
                  include = c("?"),
                  dir.exp= c(1,1,0),  
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
IS2a





#------------ANALYSIS C2:CONDITIONS ON 6 POINT CROSS-OVER AND OUTCOME grantlag16 (Patstat 2016)

#conditions are based on 6-point cross-overs
#outcome on grantlag16 (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssC2<-superSubset(IPorg_fuzzy3,outcome = "grantlag16", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                 "grantlag0111"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE
)

fssC2

pof(1-fssB2$coms, outcome = grantlag16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnC2<-superSubset(IPorg_fuzzy3,outcome = "~grantlag16", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","grantlag0111"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnC2

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1C2 <-truthTable(IPorg_fuzzy3,outcome = "grantlag16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1C2

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSC2 <-minimize (TT1C2, outcome = "grantlag16",
                 relation = "suf", incl.cut1 = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CSC2

factorize(CSC2)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##
#with lower inclusion score

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2C2<- truthTable(IPorg_fuzzy3,outcome = "grantlag16",
                   relation = "suf", incl.cut =c(0.8, 0.6), 
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2C2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "grantlag16",
           relation = "suf", incl.cut =c(0.8, 0.6), 
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "grantlag0111"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1C2 <- minimize (TT2C2, outcome = "grantlag16", 
                   relation = "suf", incl.cut = 0.80,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1C2

PS1C2$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2C2 <- minimize (TT2C2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "grantlag0111"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
PS2C2
PS2C2$PIchart
PS2C2$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAC2 <- findRows(obj = TT2C2, type = 2)
CSAC2

#INTERMEDIATE SOLUTION with directionality

ISC2 <- minimize (TT2C2, outcome = "grantlag16", 
                  relation = "suf", incl.cut = 0.8,
                  conditions= c ("central_fz","cross_fz","plan_fz",
                                 "grantlag0111"),
                  include = c("?"),
                  dir.exp= c(1,1,0,0),
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
ISC2

## STEP5: BOOLEAN MINIMIZATION WITH NEGATION OF OUTCOMES ## ? this is not correctly done?
#negation of outcome with exclusion of reminders 
PSnC2 <- minimize (IPorg_fuzzy3, outcome = "~grantlag16",neg.out = FALSE,
                   relation = "suf", incl.cut1 = 0.9,
                   conditions = c ("central_fz","cross_fz","plan_fz",
                                   "grantlag0111"),
                   row.dom= TRUE, all.sol= TRUE, details = TRUE, show.cases = TRUE)
PSnC2

PSnC2$PIchart

PSnC2$SA$M1


##ANALYSIS OF CONTRADDICTORY SIMPLIFYING ASSUMPTIONS

SCm2 <- row.names(PSnC2$SA$M1)
SCm2
SCm2n <- row.names(PSnC2$SA$M1)
SCm2n

CSC2 <- intersect(SCm2, SCm2n)
CSC2

#minimization with eqmcc of complex solution without logical reminders
neg_CPC2 <- minimize (IPorg_fuzzy3, outcome = "~grantlag16",neg.out = FALSE,
                      relation = "suf", incl.cut1 = 0.9,
                      conditions = c ("central_fz","cross_fz","plan_fz",
                                      "grantlag0111"),
                      details = TRUE, show.cases = TRUE
)
neg_CPC2

#minimization with eqmcc of intermediate solutions (0,1,1,0) with logical reminders
neg_IPC2<- minimize (IPorg_fuzzy3, outcome = "~grantlag16",neg.out = FALSE,
                     relation = "suf", incl.cut1 = 0.9,
                     conditions= c ("central_fz","cross_fz","plan_fz",
                                    "grantlag0111"),
                     include = c("?"),
                     dir.exp= c(0,1,1,0),
                     details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                     use.letters = FALSE
)
neg_IPC2



#------------ANALYSIS D2:CONDITIONS ON 6 POINT CROSS-OVER AND OUTCOME grantlag16 (Patstat 2016)

#conditions are based on 6-point cross-overs
#outcome on grantlag16 (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6
#also return PRI (proportional reduction in inconsistency) scores

fssD2<-superSubset(IPorg_fuzzy3,outcome = "grantlag16",
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                 "quicitF"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE,
                   PRI=TRUE
)

fssD2

pof(1-fssD2$coms, outcome = grantlag16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations

fssnD2<-superSubset(IPorg_fuzzy3,outcome = "~grantlag16",
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quicitF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnD2

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1D2 <-truthTable(IPorg_fuzzy3,outcome = "grantlag16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1D2

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSD2 <-minimize (TT1D2, outcome = "grantlag16",
                 relation = "suf", incl.cut1 = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CSD2

factorize(CSD2)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by
#inclusion scores and then by number of cases
TT2D2<- truthTable(IPorg_fuzzy3,outcome = "grantlag16",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2D2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "grantlag16",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "quicitF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1D2 <- minimize (TT2D2, outcome = "grantlag16",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1D2

PS1D2$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2D2 <- minimize (TT2D2, outcome = "grantlag16",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quicitF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
PS2D2
PS2D2$PIchart
PS2D2$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
findRows(obj = TT2D2, type = 2)


#INTERMEDIATE SOLUTION with directionality

ISD2 <- minimize (TT2D2, outcome = "grantlag16",
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz",
                                 "quicitF"),
                  include = c("?"),
                  dir.exp= c(1,1,0,0),
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
ISD2

## STEP5: BOOLEAN MINIMIZATION WITH NEGATION OF OUTCOMES ## ? this is not correctly done?
#negation of outcome with exclusion of reminders
PSnD2 <- minimize (IPorg_fuzzy3, outcome = "~grantlag16",neg.out = FALSE,
                   relation = "suf", incl.cut1 = 0.9,
                   conditions = c ("central_fz","cross_fz","plan_fz",
                                   "quicitF"),
                   row.dom= TRUE, all.sol= TRUE, details = TRUE, show.cases = TRUE)
PSnD2

PSnD2$PIchart

PSnD2$SA$M1


##ANALYSIS OF CONTRADDICTORY SIMPLIFYING ASSUMPTIONS

SDm2 <- row.names(PSnD2$SA$M1)
SDm2
SDm2n <- row.names(PSnD2$SA$M1)
SDm2n

CSD2 <- intersect(SDm2, SDm2n)
CSD2

#minimization with eqmcc of complex solution without logical reminders
neg_CPD2 <- minimize (IPorg_fuzzy3, outcome = "~grantlag16",neg.out = FALSE,
                      relation = "suf", incl.cut1 = 0.9,
                      conditions = c ("central_fz","cross_fz","plan_fz",
                                      "quicitF"),
                      details = TRUE, show.cases = TRUE
)
neg_CPD2

#minimization with eqmcc of intermediate solutions (0,1,1,0) with logical reminders
neg_IPD2<- minimize (IPorg_fuzzy3, outcome = "~grantlag16",neg.out = FALSE,
                     relation = "suf", incl.cut1 = 0.9,
                     conditions= c ("central_fz","cross_fz","plan_fz",
                                    "quicitF"),
                     include = c("?"),
                     dir.exp= c(0,1,1,0),
                     details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                     use.letters = FALSE
)
neg_IPD2



#------------ANALYSIS E2:CONDITIONS ON FAMILIES AND OUTCOME GRANT LAG (Patstat 2016)

#conditions are based on 6-point cross-overs
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssE3<-superSubset(IPorg_fuzzy3,outcome = "grantlag16", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                 "quifamF"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE
)

fssE3

pof(1-fssE3$coms, outcome = grantlag16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnE4<-superSubset(IPorg_fuzzy3,outcome = "~grantlag16", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quifamF"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnE4

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1E2 <-truthTable(IPorg_fuzzy3,outcome = "grantlag16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1E2

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSE2 <-minimize (TT1E2, outcome = "grantlag16",
                 relation = "suf", incl.cut1 = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CSE2

factorize(CSE2)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2E2<- truthTable(IPorg_fuzzy3,outcome = "grantlag16",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2E2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "grantlag16",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "quifamF"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1E2 <- minimize (TT2E2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1E2

PS1E1$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2E2 <- minimize (TT2E2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "quifamF"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
PS2E2
PS2E2$PIchart
PS2E2$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAE2 <- findRows(obj = TT2E2, type = 2)


#INTERMEDIATE SOLUTION with directionality

ISE2 <- minimize (TT2E2, outcome = "grantlag16", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz",
                                 "quifamF"),
                  include = c("?"),
                  dir.exp= c(1,1,0,0),
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
ISE2




#------------ANALYSIS E2:CONDITIONS ON FAMILIES AND OUTCOME GRANT LAG (Patstat 2016)

#conditions are based on 6-point cross-overs
#outcome on grant achievement (Patstat 2016)
#inclusion score of 0.9 and coverage of 0.6

##################
## NECESSITY ####
#################
#find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
#also return PRI (proportional reduction in inconsistency) scores

fssE3<-superSubset(IPorg_fuzzy3,outcome = "grantlag16", 
                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
                                                 "FHG0111F"),
                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
                   PRI=TRUE
)

fssE3

pof(1-fssE3$coms, outcome = grantlag16,IPorg_fuzzy3, relation = "necessity")

#negation of outcome of all minimal sufficiency combinations 

fssnE4<-superSubset(IPorg_fuzzy3,outcome = "~grantlag16", 
                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","FHG0111F"),
                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
                    PRI=TRUE)

fssnE4

#################
## SUFFICIENCY ##
#################

##COMPLEX SOLUTIONS##


##STEP 1: TRUTH TABLE##

TT1E2 <-truthTable(IPorg_fuzzy3,outcome = "grantlag16", neg.out = FALSE,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
                   show.cases = TRUE
)

TT1E2

##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##

CSE2 <-minimize (TT1E2, outcome = "grantlag16",
                 relation = "suf", incl.cut1 = 0.9,
                 explain = "1",include = "1",
                 row.dom = TRUE, all.sol= FALSE,
                 details = TRUE, show.cases = TRUE)

CSE2

factorize(CSE2)


##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##

#complete truthtable with logical reminders, show cases and first sort by 
#inclusion scores and then by number of cases
TT2E2<- truthTable(IPorg_fuzzy3,outcome = "grantlag16",
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   complete= TRUE, show.case= TRUE,
                   sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
TT2E2

##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##

#check for deviant cases in the truth table

truthTable(IPorg_fuzzy3,outcome = "grantlag16",
           relation = "suf", incl.cut = 0.9,
           conditions= c ("central_fz","cross_fz","plan_fz",
                          "FHG0111F"),
           show.case= TRUE, ddc=TRUE,
           sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)

#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are retained

PS1E2 <- minimize (TT2E2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE)
PS1E2

PS1E1$SA


#Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
#dominated PI are not retained

PS2E2 <- minimize (TT2E2, outcome = "grantlag16", 
                   relation = "suf", incl.cut1 = 0.9,
                   conditions= c ("central_fz","cross_fz","plan_fz",
                                  "FHG0111F"),
                   include = c("?"), all.sol = FALSE,
                   row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                   use.letters = FALSE
)
PS2E2
PS2E2$PIchart
PS2E2$SA$M1

# analysis of the counterfactuals: to exclude implausible counterfactuals
CSAE2 <- findRows(obj = TT2E2, type = 2)


#INTERMEDIATE SOLUTION with directionality

ISE2 <- minimize (TT2E2, outcome = "grantlag16", 
                  relation = "suf", incl.cut1 = 0.9,
                  conditions= c ("central_fz","cross_fz","plan_fz",
                                 "FHG0111F"),
                  include = c("?"),
                  dir.exp= c(1,1,0,0),
                  details = TRUE, show.cases = TRUE, use.tilde = FALSE,
                  use.letters = FALSE
)
ISE2

# #-----------------------------ANALYSIS A ON ORG CONDITIONS AND CITATIONS--------------------------------------------
# #-----ANALYSIS A1: CONDITIONS ON 6 POINT CROSS-OVER AND OUTCOME FGH1113_13F (Patstat 2013)------
# 
# 
# #conditions are based on 6-point cross-overs
# #outcome on grant achievement (Patstat 2013)
# #inclusion score of 0.9 and coverage of 0.6
# #Pre TIME seminar preparations: just on _fz conditions, you should try also on the _fz1 -> no, too many cross-overs
# #repeat the old analysis with _fz extension
# 
# ##################
# ## NECESSITY ####
# #################
# #find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
# #also return PRI (proportional reduction in inconsistency) scores
# 
# fssA1<-superSubset(IPorg_fuzzy3,outcome = "FHG1113_13F", 
#                         neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
#                                                       "quicitF"),
#                         relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
#                         PRI=TRUE
# )
# 
# fssA1
# 
# pof(1-fssA1$coms, outcome = FHG1113_13F,IPorg_fuzzy3, relation = "necessity")
# 
# #negation of outcome of all minimal sufficiency combinations 
# 
# fssnA1<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113_13F", 
#                          neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quicitF"),
#                          relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
#                          PRI=TRUE)
# 
# fssnA1
# 
# #prova<-superSubset(IPorg_fuzzy3,outcome = "NFHG1113_13F", 
# #                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quicitF"),
# #                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
# #                   PRI=TRUE)
# #prova --> it is the same as fssnA1
# 
# 
# #################
# ## SUFFICIENCY ##
# #################
# 
# ##COMPLEX SOLUTIONS##
# 
# 
# ##STEP 1: TRUTH TABLE##
# 
# TT1A1 <-truthTable(IPorg_fuzzy3,outcome = "FHG1113_13F", neg.out = FALSE,
#                       conditions= c ("central_fz","cross_fz","plan_fz",
#                                      "quicitF"),
#                       incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
#                       show.cases = TRUE
# )
# 
# TT1A1
# 
# #make a test on the truthtable --> incl.cut=c(0.8,06) to see if there are 
# #C cases that participate both to the positive and negative outcome
# 
# TT1A1bis <-truthTable(IPorg_fuzzy3,outcome = "FHG1113_13F", neg.out = FALSE,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    incl.cut = c(0.8 , 0.6),  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
#                    show.cases = TRUE
# )
# 
# TT1A1bis
# #what we observe is that cases 14 and 20 become C and all the other cases are recorded as 1
# 
# 
# ##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##
# 
# CSA1 <-minimize (TT1A1, outcome = "FHG1113F_13",
#                      relation = "suf", incl.cut1 = 0.9,
#                      explain = "1",include = "1",
#                      row.dom = TRUE, all.sol= FALSE,
#                      details = TRUE, show.cases = TRUE)
# 
# CSA1
# 
# factorize(CSA1)
# 
# 
# ##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##
# 
# #complete truthtable with logical reminders, show cases and first sort by 
# #inclusion scores and then by number of cases
# TT2A1<- truthTable(IPorg_fuzzy3,outcome = "FHG1113_13F",
#                        relation = "suf", incl.cut1 = 0.9,
#                        conditions= c ("central_fz","cross_fz","plan_fz",
#                                       "quicitF"),
#                        complete= TRUE, show.case= TRUE,
#                        sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# TT2A1
# 
# ##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##
# 
# #check for deviant cases in the truth table
# 
# truthTable(IPorg_fuzzy3,outcome = "FHG1113_13F",
#            relation = "suf", incl.cut = 0.9,
#            conditions= c ("central_fz","cross_fz","plan_fz",
#                           "quicitF"),
#            show.case= TRUE, ddc=TRUE,
#            sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# 
# #Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
# #dominated PI are retained
# 
# PS1A1 <- minimize (TT2A1, outcome = "FHG1113_13F", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    include = c("?"), all.sol = FALSE,
#                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE)
# PS1A1
# 
# PS1A1$SA
# 
# 
# #Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
# #dominated PI are not retained
# 
# PS2A1 <- minimize (TT2A1, outcome = "FHG1113_13F", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    include = c("?"), all.sol = FALSE,
#                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE
# )
# PS2A1
# PS2A1$PIchart
# PS2A1$SA$M1
# 
# # analysis of the counterfactuals: to exclude implausible counterfactuals
# CSA <- findRows(obj = TT2A1, type = 2)
# CSA
# 
# #Enhanced Parsimonious Analysis
# 
# PS2A1bis <- minimize (TT2A1, outcome = "FHG1113_13F", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    include = c("?"), exclude = CSA, all.sol = FALSE,
#                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE
# )
# PS2A1bis
# 
# 
# #INTERMEDIATE SOLUTION with directionality 
# #I tried the Enhanced Intermediate Analysis, but does not work --> PES and IS overlap
# 
# ISA1 <- minimize (TT2A1, outcome = "FHG1113_13F", 
#                    relation = "suf", 
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                   include = c("?"),
#                   dir.exp= c(1,1,0,0),
#                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                   use.letters = FALSE)
# ISA1
# 
# ## STEP5: BOOLEAN MINIMIZATION WITH NEGATION OF OUTCOMES (try to set c=0.9, 0.6)##
# #negation of outcome  
# PSnA1 <- minimize (IPorg_fuzzy3, outcome = "NFHG1113_13F",
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions = c ("central_fz","cross_fz","plan_fz",
#                                    "quicitF"),
#                    row.dom= TRUE, all.sol= TRUE, details = TRUE, show.cases = TRUE)
# PSnA1
# 
# PSnA1$PIchart
# 
# PSnA1$SA$M1
# 
# 
# ##ANALYSIS OF CONTRADDICTORY SIMPLIFYING ASSUMPTIONS
# 
# SAm1 <- row.names(PS1A1$SA$M1)
# SAm1
# SAm1n <- row.names(PSnA1$SA$M1)
# SAm1n
# 
# CSA1 <- intersect(SAm1, SAm1n)
# CSA1
# 
# #Negation of outcome: Truthtable
# TT2A1n<- truthTable(IPorg_fuzzy3,outcome = "NFHG1113_13F",
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    complete= TRUE, show.case= TRUE,
#                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# TT2A1n
# 
# #minimization with eqmcc of complex solution without logical reminders
# neg_CPA1 <- minimize (TT2A1n, outcome = "NFHG1113_13F",
#                       relation = "suf", incl.cut1 = 0.9,
#                       conditions = c ("central_fz","cross_fz","plan_fz",
#                                       "quicitF"),
#                       details = TRUE, show.cases = TRUE
# )
# neg_CPA1
# 
# #minimization with eqmcc of intermediate solutions (0,1,1,0) with logical reminders
# neg_IPA1<- minimize (TT2A1n, outcome = "NFHG1113_13F", neg.out = FALSE,
#                      relation = "suf", incl.cut1 = 0.9,
#                      conditions= c ("central_fz","cross_fz","plan_fz",
#                                     "quicitF"),
#                      dir.exp= c(0,1,1,0), include = "?",
#                      row.dom = TRUE,
#                      details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                      use.letters = FALSE
# )
# neg_IPA1
# 
# # notes on the negation of outcome: there is a strong overlap between cases that fall into 
# # the positive outcome  and cases that fall into the negative outcome
# # yet, when looking for contraddicting cases, there is limited evidence 
# 

#---------------------------------------------------------------------------------------------------------------------
#-----------------------------ANALYSIS B ON ORG CONDITIONS AND PATENT LAGS--------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

#-----ANALYSIS B1: CONDITIONS ON 6 POINT CROSS-OVER AND OUTCOME FGH1113_13F (Patstat 2013)------


#conditions are based on 6-point cross-overs
#outcome on grant achievement (Patstat 2013)
#inclusion score of 0.9 and coverage of 0.6
#Pre TIME seminar preparations: just on _fz conditions, you should try also on the _fz1 -> no, too many cross-overs
#repeat the old analysis with _fz extension

# ##################
# ## NECESSITY ####
# #################
# #find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
# #also return PRI (proportional reduction in inconsistency) scores
# 
# fssB1<-superSubset(IPorg_fuzzy3,outcome = "FHG1113_13F", 
#                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
#                                                  "grantlag0111_13"),
#                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
#                    PRI=TRUE
# )
# 
# fssB1
# 
# pof(1-fssB1$coms, outcome = FHG1113_13F,IPorg_fuzzy3, relation = "necessity")
# 
# #negation of outcome of all minimal sufficiency combinations 
# 
# fssnB1<-superSubset(IPorg_fuzzy3,outcome = "~FHG1113_13F", 
#                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","grantlag0111_13"),
#                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
#                     PRI=TRUE)
# 
# fssnB1
# 
# #prova<-superSubset(IPorg_fuzzy3,outcome = "NFHG1113_13F", 
# #                   neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","quicitF"),
# #                   relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
# #                   PRI=TRUE)
# #prova --> it is the same as fssnA1
# 
# 
# #################
# ## SUFFICIENCY ##
# #################
# 
# ##COMPLEX SOLUTIONS##
# 
# 
# ##STEP 1: TRUTH TABLE##
# 
# TT1B1 <-truthTable(IPorg_fuzzy3,outcome = "FHG1113_13F", neg.out = FALSE,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "grantlag0111_13"),
#                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
#                    show.cases = TRUE
# )
# 
# TT1B1
# 
# ##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##
# 
# CSB1 <-minimize (TT1B1, outcome = "FHG1113F_13",
#                  relation = "suf", incl.cut1 = 0.9,
#                  explain = "1",include = "1",
#                  row.dom = TRUE, all.sol= FALSE,
#                  details = TRUE, show.cases = TRUE)
# 
# CSB1
# 
# factorize(CSB1)
# 
# 
# ##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##
# 
# #complete truthtable with logical reminders, show cases and first sort by 
# #inclusion scores and then by number of cases
# TT2B1<- truthTable(IPorg_fuzzy3,outcome = "FHG1113_13F",
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "grantlag0111_13"),
#                    complete= TRUE, show.case= TRUE,
#                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# TT2B1
# 
# ##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##
# 
# #check for deviant cases in the truth table
# 
# truthTable(IPorg_fuzzy3,outcome = "FHG1113_13F",
#            relation = "suf", incl.cut = 0.9,
#            conditions= c ("central_fz","cross_fz","plan_fz",
#                           "grantlag0111_13"),
#            show.case= TRUE, ddc=TRUE,
#            sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# 
# #Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
# #dominated PI are retained
# 
# PS1B1 <- minimize (TT2B1, outcome = "FHG1113_13F", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "grantlag0111_13"),
#                    include = c("?"), all.sol = FALSE,
#                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE)
# PS1B1
# 
# PS1B1$SA
# 
# 
# #Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
# #dominated PI are not retained
# 
# PS2B1 <- minimize (TT2B1, outcome = "FHG1113_13F", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "grantlag0111_13"),
#                    include = c("?"), all.sol = FALSE,
#                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE
# )
# PS2B1
# PS2B1$PIchart
# PS2B1$SA$M1
# factorize(PS2B1)
# 
# # analysis of the counterfactuals: to exclude implausible counterfactuals
# findRows(obj = TT2B1, type = 2)
# 
# 
# #INTERMEDIATE SOLUTION with directionality
# 
# ISB1 <- minimize (TT2B1, outcome = "FHG1113_13F", 
#                   relation = "suf", incl.cut1 = 0.9,
#                   conditions= c ("central_fz","cross_fz","plan_fz",
#                                  "grantlag0111_13"),
#                   include = c("?"),
#                   dir.exp= c(1,1,0,0),
#                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                   use.letters = FALSE
# )
# ISB1
# 
# factorize(ISB1)
# 
# ## STEP5: BOOLEAN MINIMIZATION WITH NEGATION OF OUTCOMES## this is not correctly done
# #negation of outcome with exclusion of reminders 
# PSnB1 <- minimize (IPorg_fuzzy3, outcome = "~FHG1113_13F",neg.out = FALSE,
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions = c ("central_fz","cross_fz","plan_fz",
#                                    "grantlag0111_13"),
#                    row.dom= TRUE, all.sol= TRUE, details = TRUE, show.cases = TRUE)
# PSnB1
# 
# PSnB1$PIchart
# 
# PSnB1$SA$M1
# 
# 
# ##ANALYSIS OF CONTRADDICTORY SIMPLIFYING ASSUMPTIONS
# 
# SBm1 <- row.names(PSnB1$SA$M1)
# SBm1
# SBm1n <- row.names(PSnB1$SA$M1)
# SBm1n
# 
# CSB1 <- intersect(SBm1, SBm1n)
# CSB1
# 
# #minimization with eqmcc of complex solution without logical reminders
# neg_CPB1 <- minimize (IPorg_fuzzy3, outcome = "~FHG1113_13F",neg.out = FALSE,
#                       relation = "suf", incl.cut1 = 0.9,
#                       conditions = c ("central_fz","cross_fz","plan_fz",
#                                       "grantlag0111_13"),
#                       details = TRUE, show.cases = TRUE
# )
# neg_CPB1
# 
# #minimization with eqmcc of intermediate solutions (0,1,1,0) without logical reminders
# neg_IPB1<- minimize (IPorg_fuzzy3, outcome = "~FHG1113_13F",neg.out = FALSE,
#                      relation = "suf", incl.cut1 = 0.9,
#                      conditions= c ("central_fz","cross_fz","plan_fz",
#                                     "grantlag0111_13"),
#                      dir.exp= c(0,1,1,0),
#                      details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                      use.letters = FALSE
# )
# neg_IPB1


#----------------------------------------------------------------------------------------
#-------------- ORG CONDITIONS  AND OUTCOME CURRENT GRANT LAG ---------------------------
#use grantlag16 and grantlag13 as dependent variables

# #-----ANALYSIS C1: CONDITIONS ON 6 POINT CROSS-OVER AND OUTCOME GRANTLAT13 (Patstat 2013)------
# 
# #conditions are based on 6-point cross-overs
# #outcome on grant achievement (Patstat 2013)
# #inclusion score of 0.9 and coverage of 0.6
# #Pre TIME seminar preparations: just on _fz conditions, you should try also on the _fz1 -> no, too many cross-overs
# #repeat the old analysis with _fz extension
# 
# ##################
# ## NECESSITY ####
# #################
# #find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
# #also return PRI (proportional reduction in inconsistency) scores
# 
# fssC1<-superSubset(IPorg_fuzzy3,outcome = "grantlag13", 
#                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
#                                                  "grantlag0111_13"),
#                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
#                    PRI=TRUE
# )
# 
# fssC1
# 
# pof(1-fssC1$coms, outcome = grantlag13,IPorg_fuzzy3, relation = "necessity")
# 
# #negation of outcome of all minimal sufficiency combinations 
# 
# fssnC1<-superSubset(IPorg_fuzzy3,outcome = "~grantlag13", 
#                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","grantlag0111_13"),
#                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
#                     PRI=TRUE)
# 
# fssnC1
# 
# #################
# ## SUFFICIENCY ##
# #################
# 
# ##COMPLEX SOLUTIONS##
# 
# 
# ##STEP 1: TRUTH TABLE##
# 
# TT1C1 <-truthTable(IPorg_fuzzy3,outcome = "grantlag13", neg.out = FALSE,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "grantlag0111_13"),
#                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
#                    show.cases = TRUE
# )
# 
# TT1C1
# 
# ##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##
# 
# CSC1 <-minimize (TT1C1, outcome = "grantlag13",
#                  relation = "suf", incl.cut1 = 0.9,
#                  explain = "1",include = "1",
#                  row.dom = TRUE, all.sol= FALSE,
#                  details = TRUE, show.cases = TRUE)
# 
# CSC1
# 
# factorize(CSC1)
# 
# 
# ##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##
# 
# #complete truthtable with logical reminders, show cases and first sort by 
# #inclusion scores and then by number of cases
# TT2C1<- truthTable(IPorg_fuzzy3,outcome = "grantlag13",
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "grantlag0111_13"),
#                    complete= TRUE, show.case= TRUE,
#                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# TT2C1
# 
# ##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##
# 
# #check for deviant cases in the truth table
# 
# truthTable(IPorg_fuzzy3,outcome = "grantlag13",
#            relation = "suf", incl.cut = 0.9,
#            conditions= c ("central_fz","cross_fz","plan_fz",
#                           "grantlag0111_13"),
#            show.case= TRUE, ddc=TRUE,
#            sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# 
# #Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
# #dominated PI are retained
# 
# PS1C1 <- minimize (TT2C1, outcome = "grantlag13", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "grantlag0111_13"),
#                    include = c("?"), all.sol = FALSE,
#                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE)
# PS1C1
# 
# PS1C1$SA
# 
# 
# #Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
# #dominated PI are not retained
# 
# PS2C1 <- minimize (TT2C1, outcome = "grantlag13", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "grantlag0111_13"),
#                    include = c("?"), all.sol = FALSE,
#                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE
# )
# PS2C1
# PS2C1$PIchart
# PS2C1$SA$M1
# 
# # analysis of the counterfactuals: to exclude implausible counterfactuals
# findRows(obj = TT2C1, type = 2)
# 
# 
# #INTERMEDIATE SOLUTION with directionality
# 
# ISC1 <- minimize (TT2C1, outcome = "grantlag13", 
#                   relation = "suf", incl.cut1 = 0.9,
#                   conditions= c ("central_fz","cross_fz","plan_fz",
#                                  "grantlag0111_13"),
#                   include = c("?"),
#                   dir.exp= c(1,1,0,0),
#                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                   use.letters = FALSE
# )
# ISC1
# 
# ## STEP5: BOOLEAN MINIMIZATION WITH NEGATION OF OUTCOMES ## this is not correctly done
# #negation of outcome with exclusion of reminders 
# PSnC1 <- minimize (IPorg_fuzzy3, outcome = "~grantlag13",neg.out = FALSE,
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions = c ("central_fz","cross_fz","plan_fz",
#                                    "grantlag0111_13"),
#                    row.dom= TRUE, all.sol= TRUE, details = TRUE, show.cases = TRUE)
# PSnC1
# 
# PSnC1$PIchart
# 
# PSnC1$SA$M1
# 
# 
# ##ANALYSIS OF CONTRADDICTORY SIMPLIFYING ASSUMPTIONS
# 
# SCm1 <- row.names(PSnC1$SA$M1)
# SCm1
# SCm1n <- row.names(PSnC1$SA$M1)
# SCm1n
# 
# CSC1 <- intersect(SCm1, SCm1n)
# CSC1
# 
# #minimization with eqmcc of complex solution without logical reminders
# neg_CPC1 <- minimize (IPorg_fuzzy3, outcome = "~grantlag13",neg.out = FALSE,
#                       relation = "suf", incl.cut1 = 0.9,
#                       conditions = c ("central_fz","cross_fz","plan_fz",
#                                       "grantlag0111_13"),
#                       details = TRUE, show.cases = TRUE
# )
# neg_CPC1
# 
# #minimization with eqmcc of intermediate solutions (0,1,1,0) without logical reminders
# neg_IPC1<- minimize (IPorg_fuzzy3, outcome = "~grantlag13",neg.out = FALSE,
#                      relation = "suf", incl.cut1 = 0.9,
#                      conditions= c ("central_fz","cross_fz","plan_fz",
#                                     "grantlag0111_13"),
#                      dir.exp= c(0,1,1,0),
#                      details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                      use.letters = FALSE
# )
# neg_IPC1
# #-----ANALYSIS D1: CONDITIONS ON 6 POINT CROSS-OVER AND GRANTLAG (PatStat13)------
# 
# #conditions are based on 6-point cross-overs
# #outcome on grant achievement (Patstat 2013)
# #inclusion score of 0.9 and coverage of 0.6
# #Pre TIME seminar preparations: just on _fz conditions, you should try also on the _fz1 -> no, too many cross-overs
# #repeat the old analysis with _fz extension
# 
# ##################
# ## NECESSITY ####
# #################
# #find all minimal necessary combinations with inclusion score of 0.9 and coverage of 0.6  
# #also return PRI (proportional reduction in inconsistency) scores
# 
# fssD1<-superSubset(IPorg_fuzzy3,outcome = "grantlag13", 
#                    neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz",
#                                                  "quicitF"),
#                    relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde=FALSE, use.letter=FALSE, 
#                    PRI=TRUE
# )
# 
# fssD1
# 
# pof(1-fssD1$coms, outcome = grantlag13,IPorg_fuzzy3, relation = "necessity")
# 
# #negation of outcome of all minimal sufficiency combinations 
# 
# fssnD1<-superSubset(IPorg_fuzzy3,outcome = "~grantlag13", 
#                     neg.out=FALSE, conditions= c ("central_fz","cross_fz","plan_fz","grantlag0111_13"),
#                     relation="nec", incl.cut=0.9, cov.cut = 0.6, use.tilde= FALSE, use.letter=FALSE,
#                     PRI=TRUE)
# 
# fssnD1
# 
# #################
# ## SUFFICIENCY ##
# #################
# 
# ##COMPLEX SOLUTIONS##
# 
# 
# ##STEP 1: TRUTH TABLE##
# 
# TT1D1 <-truthTable(IPorg_fuzzy3,outcome = "grantlag13", neg.out = FALSE,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    incl.cut1 =0.9,  complete = FALSE, sort.by = c("incl", "n"),decreasing = TRUE,
#                    show.cases = TRUE
# )
# 
# TT1D1
# 
# ##STEP 2: BOOLEAN MINIMIZATION OF COMPLEX SOLUTIONS##
# 
# CSD1 <-minimize (TT1D1, outcome = "grantlag13",
#                  relation = "suf", incl.cut1 = 0.9,
#                  explain = "1",include = "1",
#                  row.dom = TRUE, all.sol= FALSE,
#                  details = TRUE, show.cases = TRUE)
# 
# CSD1
# 
# factorize(CSD1)
# 
# 
# ##STEP 3: TRUTH TABLE WITH LOGICAL REMINDERS AND POSITIVE OUTCOME##
# 
# #complete truthtable with logical reminders, show cases and first sort by 
# #inclusion scores and then by number of cases
# TT2D1<- truthTable(IPorg_fuzzy3,outcome = "grantlag13",
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    complete= TRUE, show.case= TRUE,
#                    sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# TT2D1
# 
# ##STEP 4: BOOLEAN MINIMIZATION WITH PARSIMONIOUS SOLUTIONS##
# 
# #check for deviant cases in the truth table
# 
# truthTable(IPorg_fuzzy3,outcome = "grantlag13",
#            relation = "suf", incl.cut = 0.9,
#            conditions= c ("central_fz","cross_fz","plan_fz",
#                           "quicitF"),
#            show.case= TRUE, ddc=TRUE,
#            sort.by=c("incl", "n"), decreasing= TRUE, use.letters= FALSE)
# 
# #Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
# #dominated PI are retained
# 
# PS1D1 <- minimize (TT2D1, outcome = "grantlag13", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    include = c("?"), all.sol = FALSE,
#                    row.dom = FALSE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE)
# PS1D1
# 
# PS1D1$SA
# 
# 
# #Boolean minization of truthtable with the maximum numbers of boundaries and with logical reminders
# #dominated PI are not retained
# 
# PS2D1 <- minimize (TT2D1, outcome = "grantlag13", 
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions= c ("central_fz","cross_fz","plan_fz",
#                                   "quicitF"),
#                    include = c("?"), all.sol = FALSE,
#                    row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                    use.letters = FALSE
# )
# PS2D1
# PS2D1$PIchart
# PS2D1$SA$M1
# 
# # analysis of the counterfactuals: to exclude implausible counterfactuals
# findRows(obj = TT2D1, type = 2)
# 
# 
# #INTERMEDIATE SOLUTION with directionality
# 
# ISD1 <- minimize (TT2D1, outcome = "grantlag13", 
#                   relation = "suf", incl.cut1 = 0.9,
#                   conditions= c ("central_fz","cross_fz","plan_fz",
#                                  "quicitF"),
#                   include = c("?"),
#                   dir.exp= c(1,1,0,0),
#                   details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                   use.letters = FALSE
# )
# ISD1
# 
# ## STEP5: BOOLEAN MINIMIZATION WITH NEGATION OF OUTCOMES ## this is not correctly done
# #negation of outcome with exclusion of reminders 
# PSnD1 <- minimize (IPorg_fuzzy3, outcome = "~grantlag13",neg.out = FALSE,
#                    relation = "suf", incl.cut1 = 0.9,
#                    conditions = c ("central_fz","cross_fz","plan_fz",
#                                    "quicitF"),
#                    row.dom= TRUE, all.sol= TRUE, details = TRUE, show.cases = TRUE)
# PSnD1
# 
# PSnD1$PIchart
# 
# PSnD1$SA$M1
# 
# 
# ##ANALYSIS OF CONTRADDICTORY SIMPLIFYING ASSUMPTIONS
# 
# SDm1 <- row.names(PSnD1$SA$M1)
# SDm1
# SDm1n <- row.names(PSnD1$SA$M1)
# SDm1n
# 
# CSD1 <- intersect(SDm1, SDm1n)
# CSD1
# 
# #minimization with eqmcc of complex solution without logical reminders
# neg_CPD1 <- minimize (IPorg_fuzzy3, outcome = "~grantlag13",neg.out = FALSE,
#                       relation = "suf", incl.cut1 = 0.9,
#                       conditions = c ("central_fz","cross_fz","plan_fz",
#                                       "quicitF"),
#                       details = TRUE, show.cases = TRUE
# )
# neg_CPD1
# 
# #minimization with eqmcc of intermediate solutions (0,1,1,0) without logical reminders
# neg_IPD1<- minimize (IPorg_fuzzy3, outcome = "~grantlag13",neg.out = FALSE,
#                      relation = "suf", incl.cut1 = 0.9,
#                      conditions= c ("central_fz","cross_fz","plan_fz",
#                                     "quicitF"),
#                      dir.exp= c(0,1,1,0),
#                      details = TRUE, show.cases = TRUE, use.tilde = FALSE,
#                      use.letters = FALSE
# )
# neg_IPD1



