############## GROUP 2 FINAL PROJECT STATISTICAL COMPUTING BANA 6043 ###################
############## FEDERAL EMPLOYMENT ENGAGEMENT DATA ANALYSIS 2010 USING R ################

rm(list=ls())
# Data set: Employee Engagement Survey 2010

# Program Flow:
# 1. Loading data set and required libraries
# 2. Cleaning and sub setting data set
# 3. Creating Indexes
# 4. Univariate Analysis
# 5. Bivariate Analysis
# 6. Hypothesis Testing
# 7. Linear regression and Logistic regression

# EmpIndex - Employment Engagement Index (EEI)
#JSI - Job Satisfaction Index


# 1. Loading the dataset and required libraries--------------------

setwd("/Users/teresa/Desktop/R data")
library(plotrix)
library(psych)
library(tidyverse)
library(ggplot2)
library(gplots)
library(dplyr)
library(tidyr)

#load the data set - importing to R 
EmpIndex<- read.csv("EVS2010_PRDF.csv",stringsAsFactors = T, header = T, na.strings = c(""," ","NA"))

View(EmpIndex) #263475 obs of 92 vars
attach(EmpIndex)

# 2. sub setting and data cleaning using dplyr-------------------
# a) Sub setting the required variables from the top 5 federal agencies on the basis of number of employees
#selected variables:AGENCY,DLOC,DSUPER,DSEX,DAGEGRP,DLEAVING,Q3,Q4,Q5,Q6,Q11,Q13,Q47,Q48,Q53,Q56,Q63,Q67,Q69,Q70
#sub setting $AGENCY column: top 5 agencies (Army, Agriculture, Commerce, Justice and Treasury.)
#data cleaning: drop unused levels in $AGENCY column

selected<- c("AR","DJ","TR","CM","AG")
EmpIndex_subset1<- EmpIndex%>%
      select(AGENCY,DLOC,DSUPER,DSEX,DAGEGRP,DLEAVING,
             Q3,Q4,Q5,Q6,Q11,Q13,Q47,Q48,Q53,Q56,Q63,Q67,Q69,Q70)%>%
      filter(AGENCY %in% selected)%>%
      droplevels()

View(EmpIndex_subset1)# 83349 obs of 20 variables
summary(EmpIndex_subset1)#noticed NA and unused levels in some columns 


# b) Data cleaning 
#answer "X:do not know" is shown as a level in data set
#treat X as NA

EmpIndex_subset1[EmpIndex_subset1=="X"] <-NA

# c) Converting variable data types and cleaning messy data labels

EmpIndex_subset1$Q47<- as.integer(EmpIndex_subset1$Q47)
EmpIndex_subset1$Q11<- as.integer(EmpIndex_subset1$Q11)
EmpIndex_subset1$Q13<- as.integer(EmpIndex_subset1$Q13)
EmpIndex_subset1$Q53<- as.integer(EmpIndex_subset1$Q53)
EmpIndex_subset1$Q56<- as.integer(EmpIndex_subset1$Q56)

EmpIndex_subset1$DLOC <- gsub("A","HQ",EmpIndex_subset1$DLOC)
EmpIndex_subset1$DLOC <- gsub("B","F",EmpIndex_subset1$DLOC)
EmpIndex_subset1$DLOC<- as.factor(EmpIndex_subset1$DLOC)

EmpIndex_subset1$DSEX <- gsub("A","M",EmpIndex_subset1$DSEX)
EmpIndex_subset1$DSEX <- gsub("B","F",EmpIndex_subset1$DSEX)
EmpIndex_subset1$DSEX <- as.factor(EmpIndex_subset1$DSEX)

EmpIndex_subset1$DSUPER <- gsub("A","Non-Supervior/Team-Leader",EmpIndex_subset1$DSUPER)
EmpIndex_subset1$DSUPER <- gsub("B","Supervisor",EmpIndex_subset1$DSUPER)
EmpIndex_subset1$DSUPER <- gsub("C","Manager/Executive",EmpIndex_subset1$DSUPER)
EmpIndex_subset1$DSUPER <- as.factor(EmpIndex_subset1$DSUPER)

EmpIndex_subset1$DAGEGRP <- gsub("B","29 and Under",EmpIndex_subset1$DAGEGRP)
EmpIndex_subset1$DAGEGRP <- gsub("C","30-39",EmpIndex_subset1$DAGEGRP)
EmpIndex_subset1$DAGEGRP <- gsub("D","40-49",EmpIndex_subset1$DAGEGRP)
EmpIndex_subset1$DAGEGRP <- gsub("E","50-59",EmpIndex_subset1$DAGEGRP)
EmpIndex_subset1$DAGEGRP <- gsub("F","60 or Above",EmpIndex_subset1$DAGEGRP)
EmpIndex_subset1$DAGEGRP <- as.factor(EmpIndex_subset1$DAGEGRP)

EmpIndex_subset1$DLEAVING <- gsub("A","N",EmpIndex_subset1$DLEAVING)
EmpIndex_subset1$DLEAVING <- gsub("B","R",EmpIndex_subset1$DLEAVING)
EmpIndex_subset1$DLEAVING <- gsub("C","Y",EmpIndex_subset1$DLEAVING)
EmpIndex_subset1$DLEAVING <- gsub("D","Y",EmpIndex_subset1$DLEAVING)
EmpIndex_subset1$DLEAVING <- gsub("E","Y",EmpIndex_subset1$DLEAVING)
EmpIndex_subset1$DLEAVING <- as.factor(EmpIndex_subset1$DLEAVING)

summary(EmpIndex_subset1)# 83349 obs of 20 variables

# d) Dropping unused levels and re-indexing
#remove missing values and unused levels

EmpIndex_subset1<- EmpIndex_subset1%>%
                    drop_na()%>%
                    droplevels()

#otherways to remove na:
#df_omit<- na.omit(EmpIndex_subset)
#View(df_omit)

rownames(EmpIndex_subset1) <- 1:nrow(EmpIndex_subset1)
str(EmpIndex_subset1)
dim(EmpIndex_subset1)
summary(EmpIndex_subset1) #71482 obs of 20 variables


# 3. Creating 2 new indexes: EmpIndex and JSI, varying from 0.0 to 1.0--------------------
attach(EmpIndex_subset1)
EmpIndex_subset1$EmpIndex<- round((Q3+Q4+Q6+Q11+Q47+Q48+Q53+Q56)/40,2) 
EmpIndex_subset1$JSI<- round((Q4+Q5+Q13+Q63+Q67+Q69+Q70)/35,2)

View(EmpIndex_subset1)
summary(EmpIndex_subset1)

# Now the (EmpIndex_subset1) data set is ready for the exploratory analysis


# 4. Univariate Analysis-----------------------
# a) Control Variable: Agency (AGENCY)
#visualization using ggplot
ggplot(data.frame(AGENCY), aes(x=AGENCY)) + 
  geom_bar()

### pie chart to show distribution by top 5 agencies

percentlabelAGENCY<- round(100*table(AGENCY)/sum(table(AGENCY)), 1)
pie3D(table(AGENCY), labels = percentlabelAGENCY, explode = 0.1, main = " % Distribution by Top 5 Agencies")
legend("topright", cex = 0.70, legend = c("Army", "Agriculture","Commerce","Justice","Treasury"), 
       fill = c("yellow", "red","green","blue","purple"))

# b) Control Variable: Location (DLOC)
dev.off()

summary(EmpIndex_subset1$DLOC)

barplotLOC<- barplot(round(prop.table(table(EmpIndex_subset1$DLOC))*100,1), main = "% Distribution by Location ", horiz = TRUE, col=c("orange","grey"),xlim=c(0,60))
text(0,barplotLOC,round(prop.table(table(EmpIndex_subset1$DLOC))*100,1),cex=1,pos=4) 
legend("topright", legend = c("Field","Headquaters"), 
       fill = c("grey","orange"), cex = 0.75)	
box()

# c) Control Variable: Supervisory Status (DSUP)
dev.off()

summary(EmpIndex_subset1$DSUPER)

barplotSUP<-barplot(round(100*prop.table(table(EmpIndex_subset1$DSUPER)),1), main = "% Distribution by Supervisory Status ", horiz = TRUE, col=c("lightgreen","orange","steelblue"),xlim=c(0,75))
text(0,barplotSUP,round(100*prop.table(table(EmpIndex_subset1$DSUPER)),1),cex=1,pos=4) 
legend("topright", legend = c("Supervisor","Non-Supervisor/Team-Leader","Manager/Executive"), 
       fill = c("steelblue","orange","lightgreen"), cex = 0.75)	
box()

# d) Control Variable: Sex (DSEX)
dev.off()

summary(EmpIndex_subset1$DSEX)

barplotSEX<-barplot(round(100*prop.table(table(EmpIndex_subset1$DSEX)),1), main = "% Distribution by Sex", col=c("orange","green"),ylim=c(0,60))
text(barplotSEX,5,round(100*prop.table(table(EmpIndex_subset1$DSEX)),1),cex=1,pos=3) 
legend("topright", legend = c("Female","Male"), 
       fill = c("orange","green"), cex = 0.75)	
box()

# e) Control Variable: Leaving Status (DLEAVING)
dev.off()

summary(EmpIndex_subset1$DLEAVING)

barplotLEAVING<-barplot(round(100*prop.table(table(EmpIndex_subset1$DLEAVING)),1), main = "% Distribution by Leaving Status", col=c("yellow","lightgreen","steelblue"),ylim=c(0,80))
text(barplotLEAVING,5, round(100*prop.table(table(EmpIndex_subset1$DLEAVING)),1),cex=1,pos=3) 
legend("topright", legend = c("No","Retire","Yes"), 
       fill = c("yellow","lightgreen","steelblue"), cex = 0.75)	
box()

# f) Control Variable: Age (DAGE)
dev.off()

summary(EmpIndex_subset1$DAGEGRP)

barplotAGE<-barplot(table(EmpIndex_subset1$DAGEGRP), main = "Distribution by Age Group", col=c("orange","grey","lightgreen","steelblue","red"),ylim=c(0,30000))
text(barplotAGE,5000,table(EmpIndex_subset1$DAGEGRP),cex=1,pos=3)
legend("topright", legend = c("<29","30-39","40-49","50-59",">60"), 
       fill = c("orange","grey","lightgreen","steelblue","red"), cex = 0.75)	
box()

## pie chart demonstrating Age distribution

percentlabelAGE<- round(100*table(DAGEGRP)/sum(table(DAGEGRP)), 1)
pie3D(table(DAGEGRP), labels = percentlabelAGE, explode = 0.1, main = " % Distribution by Age Group")
legend("topright", legend = c("29 and Under", "30-39","40-49","50-59","60 or Above"), 
       fill = c("yellow", "red","green","blue","purple"), cex = 0.65)	

# g) Dependent Index: Job Satisfaction Index (JSI)
dev.off()
par(mfrow=c(2,1))

summary(EmpIndex_subset1$JSI)
attach(EmpIndex_subset1)
hist(JSI, col=c("orange"), freq = F)
lines(density(JSI),col=c("blue"),lwd =3.0)
box()

boxplot(JSI, horizontal = TRUE,col=c("orange"), main="Boxplot of JSI")
text(x=fivenum(JSI), labels =fivenum(JSI), y=1.35)

# Squaring variable JSI to reduce left skewness in the distribution


qplot(JSI^2, data=EmpIndex_subset1, geom="density",fill=DLEAVING, alpha=I(.5), 
      main="Distribution of Squared JSI", xlab="Squared JSI", 
      ylab="Density")

describe(JSI)

# h) Dependent Index: Employee Engagement Index (EmpIndex)
dev.off()
par(mfrow=c(2,1))
summary(EmpIndex_subset1$EmpIndex)
attach(EmpIndex_subset1)

hist(EmpIndex_subset1$EmpIndex, col=c("steelblue"),freq=F, ylim = c(0,4))
lines(density(EmpIndex_subset1$EmpIndex),col=c("red"),lwd =3.0)
box()

boxplot(EmpIndex_subset1$EmpIndex, horizontal = TRUE,col=c("steelblue"), main="Boxplot of EmpIndex")
text(x=fivenum(EmpIndex_subset1$EmpIndex), labels =fivenum(EmpIndex), y=1.35)

# Squaring variable EmpIndex to reduce left skewness in the distribution

qplot(EmpIndex_subset1$EmpIndex^2, data=EmpIndex_subset1, geom="density",fill=DLEAVING, alpha=I(.5), 
      main="Distribution of Squared EmpIndex", xlab="Squared EmpIndex", 
      ylab="Density")

describe(EmpIndex_subset1$EmpIndex)

# 5. Bivariate Analysis-----------------------

# a) EmpIndex and Location
dev.off()
library(ggplot2)
qplot(EmpIndex,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DLOC,colour = factor(DLOC))
# Distribution of EmpIndex by Location

# The median EmpIndex of Field is almost same as Headquarter.

qplot(DLOC,EmpIndex, data=EmpIndex_subset1, geom=c("boxplot","jitter"), 
      fill=DLOC, main="EmpIndex by LOCATION",
      xlab="", ylab="EmpIndex")


qplot(DLOC,EmpIndex, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DLOC, main="EmpIndex by Location",
      xlab="", ylab="EmpIndex")

library(gplots)
plotmeans(EmpIndex_subset1$EmpIndex~EmpIndex_subset1$DLOC, xlab="Location", ylab="EmpIndex Index", lwd=3, col="red", p=0.99)
library(psych)
describeBy(EmpIndex_subset1$EmpIndex, EmpIndex_subset1$DLOC)
# b) EmpIndex and AGEGRP
dev.off()

qplot(EmpIndex,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DAGEGRP,colour = factor(DAGEGRP))
# Distribution of EmpIndex by Age

# The median EmpIndex is similar between different age groups. Slightly higher for 60 or Above

qplot(DAGEGRP,EmpIndex, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DAGEGRP, main="EmpIndex by Age",
      xlab="", ylab="EmpIndex")

plotmeans(EmpIndex_subset1$EmpIndex~EmpIndex_subset1$DAGEGRP, xlab="AGE GROUP", ylab="EmpIndex Index", lwd=3, col="red", p=0.99)

# c) EmpIndex and SEX
dev.off()

qplot(EmpIndex,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DSEX,colour = factor(DSEX))

# Distribution of EmpIndex by Sex

# The median EmpIndex is the same for males and females in the top 5 federal agencies
qplot(DSEX,EmpIndex, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DSEX, main="EmpIndex by Sex",
      xlab="sex", ylab="EmpIndex")

plotmeans(EmpIndex_subset1$EmpIndex~EmpIndex_subset1$DSEX, xlab="SEX", ylab="EmpIndex Index", lwd=3, col="red", p=0.99)


# d) EmpIndex and SUPERVISORY STATUS

qplot(EmpIndex,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DSUPER,colour = factor(DSUPER))
# Distribution of EmpIndex by Supervisory status

# The median EmpIndex for Manager level is slightly higher than Non-Supervisor level and Supervisor level
qplot(DSUPER,EmpIndex, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DSUPER, main="EmpIndex by Supervisory status",
      xlab="", ylab="EmpIndex")

plotmeans(EmpIndex_subset1$EmpIndex~EmpIndex_subset1$DSUPER, xlab="SUPERVISORY STATUS", ylab="EmpIndex Index", lwd=3, col="red", p=0.99)

# e) EmpIndex and LEAVING STATUS

qplot(EmpIndex,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DLEAVING,colour = factor(DLEAVING))
# Distribution of EmpIndex by Leaving status

# The median EmpIndex value for employees not leaving or retiring from their organisations is higher than employees leaving as expected. 
qplot(DLEAVING,EmpIndex, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DLEAVING, main="EmpIndex by Leaving Status",
      xlab="", ylab="EmpIndex")

plotmeans(EmpIndex_subset1$EmpIndex~EmpIndex_subset1$DLEAVING, xlab="LEAVING STATUS", ylab="EmpIndex Index", lwd=3, col="red", p=0.99)

# f) JSI and AGENCY
dev.off()

qplot(JSI,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$AGENCY,colour = factor(AGENCY))
# Distribution of JSI by Agency

# The median JSI is almost similar lying between the range 0.6 - 0.65 between the top 5 agencies

qplot(AGENCY,JSI, data=EmpIndex_subset1, geom=c("boxplot","jitter"), 
      fill=AGENCY, main="JSI by Agency",
      xlab="", ylab="JSI")

qplot(AGENCY,JSI, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=AGENCY, main="JSI by Agency",
      xlab="", ylab="JSI")

plotmeans(EmpIndex_subset1$JSI~EmpIndex_subset1$AGENCY, xlab="AGENCY", ylab="JSI Index", lwd=3, col="red", p=0.99)


# g) JSI and AGEGRP
dev.off()

qplot(JSI,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DAGEGRP,colour = factor(DAGEGRP))
# Distribution of JSI by Age Group

# The median JSI is similar between different age groups. Slightly lower for 29 and Under and 30 - 39 groups in comparison with other groups

qplot(DAGEGRP,JSI, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DAGEGRP, main="JSI by Age",
      xlab="", ylab="JSI")

plotmeans(EmpIndex_subset1$JSI~EmpIndex_subset1$DAGEGRP, xlab="AGE GROUP", ylab="JSI Index", lwd=3, col="red", p=0.99)


# h) JSI and SEX
dev.off()

qplot(JSI,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DSEX,colour = factor(DSEX))

# Distribution of JSI by Sex

# The median JSI is slighly higher for males in comparison to females in the top 5 federal agencies
qplot(DSEX,JSI, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DSEX, main="JSI by Sex",
      xlab="SEX", ylab="JSI")

plotmeans(EmpIndex_subset1$JSI~EmpIndex_subset1$DSEX, xlab="SEX", ylab="JSI Index", lwd=3, col="red", p=0.99)


# i) JSI and SUPERVISORY STATUS

qplot(JSI,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DSUPER,colour = factor(DSUPER))
# Distribution of JSI by Supervisory Status

# The median JSI for Manager level is slightly higher than Non-Supervisor level and Supervisor level. The pattern is similar to the bivariable relation between EmpIndex and Supervisory Status.
qplot(DSUPER,JSI, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DSUPER, main="JSI by Supervisory status",
      xlab="", ylab="JSI")

plotmeans(EmpIndex_subset1$JSI~EmpIndex_subset1$DSUPER, xlab="SUPERVISORY STATUS", ylab="JSI Index", lwd=3, col="red", p=0.99)


# j) JSI and LEAVING STATUS

qplot(JSI,data=EmpIndex_subset1, facets = . ~EmpIndex_subset1$DLEAVING,colour = factor(DLEAVING))
# Distribution of JSI by Leaving status

# The median JSI value for employees not leaving or retiring from their organisations is higher than employees leaving as expected. The pattern is similar to the bivariable relation between JSI and Supervisory Status.
qplot(DLEAVING,JSI, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DLEAVING, main="JSI by Leaving Status",
      xlab="", ylab="JSI")

plotmeans(EmpIndex_subset1$JSI~EmpIndex_subset1$DLEAVING, xlab="LEAVING STATUS", ylab="JSI Index", lwd=3, col="red", p=0.99)


#k) JSI and EmpIndex in relation with Q70

# High correlation between JSI and EmpIndex when satisfaction with pay is high. 
qplot(JSI, EmpIndex, data=EmpIndex_subset1, geom=c( "smooth","point"),
      color=Q70, 
      main="JSI and EmpIndex by Pay Satisfaction levels", 
      xlab="JSI", ylab="EmpIndex")

# l)  Agency and Q5 in relation with Supervisory Status

# The Likeness to work is similar and high between the top 5 agencies in various supervisory statues.
qplot(AGENCY,Q5, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DSUPER, main="Agency and likeness to work with Supervisory Status",
      xlab="AGENCY", ylab="Q5")

# n) Location and Q69 in relation with Supervisory Status

# The Job Satisfaction levels is similar and high between the locations in various supervisory statues.
qplot(DLOC,Q69, data=EmpIndex_subset1, geom=c("boxplot"), 
      fill=DSUPER, main="Location and Job Satisfaction levels with Supervisory Status",
      xlab="LOCATION", ylab="Q69")


# 6. Hypothesis Testing

# a) Positive relation between JSI and EmpIndex
attach(EmpIndex_subset1)
cor(JSI,EmpIndex_subset1$EmpIndex)
# The correlation between JSI and EmpIndex is 0.848 indicating a high positive relation between the two indexes.

remod <- lm(EmpIndex ~ JSI, data = EmpIndex_subset1)
summary(remod)
# Adjusted R-squared:  0.7201 and p-value: < 2.2e-16

# b) There is a difference in the average EmpIndex between Males and Females

# Null Hypothesis: There is no difference in the average EmpIndex between Males and Females
# Alternative Hypothesis: There is a difference in the average EmpIndex between Males and Females

# Since the distribution is not normal, therefore wilcox test is better suited to test the hypothesis

wilcox.test(EmpIndex_subset1$EmpIndex[EmpIndex_subset1$DSEX=="M"],EmpIndex_subset1$EmpIndex[EmpIndex_subset1$DSEX=="F"], alternative="two.sided") 

# p-value = 0.0069 implies that the null hypothesis can be rejected at 99% confidence

# c) There is a difference in the average EmpIndex between employees of different age groups
# Null Hypothesis: There is no difference in the average EmpIndex between different age groups
# Alternative Hypothesis: There is a difference in the average EmpIndex between different age groups
library(dplyr)
EmpIndex_subset1 %>% 
  group_by(DAGEGRP) %>% 
  summarize(avg = mean(EmpIndex), std = sd(EmpIndex), med = median(EmpIndex)) 

boxplot(EmpIndex~DAGEGRP, data=EmpIndex_subset1, col=2:6, xlab="EmpIndex Index")

library(gplots)
plotmeans(EmpIndex_subset1$EmpIndex~EmpIndex_subset1$DAGEGRP, xlab="EmpIndex Index", ylab="AGE GROUP", lwd=3, col="red", p=0.99)

# ANOVA Test
EmpIndexage.aov <- aov(EmpIndex~DAGEGRP, data=EmpIndex_subset1)
EmpIndexage.aov
summary(EmpIndexage.aov)
# p-value = 7.23e-1 implies that the null hypothsis can be rejected at 99% confidence

# Tukey Test
EmpIndexage.tk <- TukeyHSD(EmpIndexage.aov)
round(EmpIndexage.tk$DAGEGRP,2)
# The difference in mean values of EmpIndex is mainly between the following groups:
#   i) 30-39 & 29 and Under
#   ii) 40-49 & 29 and Under
#   iii) 50-59 & 29 and Under 
#   iv) 40-49 & 30-39 
#   v) 50-59 & 30-39 
#   vi) 50-59 & 40-49 

# d) There is a difference in the average EmpIndex of employees located in Field and HQ

# Null Hypothesis: There is no difference in the average EmpIndex of employees located in Field and HQ
# Alternative Hypothesis: There is a difference in the average EmpIndex of  located in Field and HQ

# Since the distribution is not normal, therefore wilcox test is better suited to test the hypothesis
wilcox.test(EmpIndex_subset1$EmpIndex[EmpIndex_subset1$DLOC=="F"],EmpIndex_subset1$EmpIndex[EmpIndex_subset1$DLOC=="HQ"], alternative="two.sided") 
# p-value = 2.924e-15 implies that the null hypothesis can be rejected at 99% confidence

# e) There is a difference in the average EmpIndex between employees of different supervisory statuses
# Null Hypothesis: There is no difference in the average EmpIndex between different supervisory statuses
# Alternative Hypothesis: There is a difference in the average EmpIndex between different supervisory statuses

EmpIndex_subset1 %>% group_by(DSUPER) %>% summarize(avg = mean(EmpIndex), std = sd(EmpIndex), med = median(EmpIndex)) 

boxplot(EmpIndex~DSUPER, data=EmpIndex_subset1, col=2:5, xlab="EmpIndex Index")
plotmeans(EmpIndex_subset1$EmpIndex~EmpIndex_subset1$DSUPER, xlab="EmpIndex Index", ylab="Supervisory Status", lwd=3, col="red", p=0.99)

# ANOVA Test
EmpIndexsuper.aov <- aov(EmpIndex~DSUPER, data=EmpIndex_subset1)
EmpIndexsuper.aov
summary(EmpIndexsuper.aov)
# p-value = 2e-16 implies that the null hypothesis can be rejected at 99% confidence

# Tukey Test
EmpIndexsuper.tk <- TukeyHSD(EmpIndexsuper.aov)
EmpIndexsuper.tk
# The difference in mean values of EmpIndex between different supervisory statuses

# f) There is a difference in the average EmpIndex between employees of different leaving statuses
# Null Hypothesis: There is no difference in the average EmpIndex between different leaving statuses
# Alternative Hypothesis: There is a difference in the average EmpIndex between different leaving statuses
library(dplyr)
EmpIndex_subset1 %>% 
  group_by(DLEAVING) %>% 
  summarize(avg = mean(EmpIndex), std = sd(EmpIndex), med = median(EmpIndex)) 

boxplot(EmpIndex~DLEAVING, data=EmpIndex_subset1, col=2:5, xlab="EmpIndex Index")
plotmeans(EmpIndex_subset1$EmpIndex~EmpIndex_subset1$DLEAVING, xlab="EmpIndex Index", ylab="Leaving Status", lwd=3, col="red", p=0.99)

# ANOVA Test
EmpIndexleaving.aov <- aov(EmpIndex~DLEAVING, data=EmpIndex_subset1)
EmpIndexleaving.aov
summary(EmpIndexleaving.aov)
# p-value = 2e-16 implies that the null hypothesis can be rejected at 99% confidence

# Tukey Test
EmpIndexleaving.tk <- TukeyHSD(EmpIndexleaving.aov)
EmpIndexleaving.tk
# The difference in mean values of EmpIndex between different leaving statuses


# 7. Linear regression and Logistic regression
# a)linear regression

#created a new variable: JSI_new to model
#JSI_new: a binary variable,assign 1 to JSI>=0.6 (satisfied); else assign 0. 

EmpIndex_subset1$JSI_new<- as.factor(ifelse(JSI>=0.6, 1, 0))
levels(JSI_new)

#split the dataset into training and testing (70/30)

set.seed(123)
train_idx <- sample(nrow(EmpIndex_subset1),.70*nrow(EmpIndex_subset1))
eei_train <- EmpIndex_subset1[train_idx,]
eei_test <- EmpIndex_subset1[-train_idx,]
model1<- lm(EmpIndex ~ DSEX+DLOC+DSUPER+AGENCY+DAGEGRP+DLEAVING, data = eei_train)
summary(model1)
mod1_oos_preds <- predict(model1, eei_test)
str(mod1_oos_preds)

model2<- lm(EmpIndex ~ DSEX+DLOC+DSUPER+DLEAVING+AGENCY+DAGEGRP+ JSI_new, data = eei_train)
summary(model2)
mod2_oos_preds <- predict(model2, eei_test)
str(mod2_oos_preds)

###select one from the two models 
#write a function to calculate RMSE:
calc_performance <- function(actual, pred) {
  
  rmse <- sqrt(mean((actual - pred)**2))
  mae <- mean(abs(actual - pred))
  mape <- mean(abs((actual-pred)/actual))
  
  retvals <- list(rmse = rmse, mae = mae, mape = mape)
  return(retvals)
  
}
# performance measures for candidate model 1
oos_mod1 <- calc_performance(eei_test$EmpIndex, mod1_oos_preds)
oos_mod1

# performance measures for candidate model 2
oos_mod2 <- calc_performance(eei_test$EmpIndex, mod2_oos_preds)
oos_mod2

#choose model based upon the out-of-sample RMSE
final_model <- lm(EmpIndex ~ DSEX+DLOC+DSUPER+DLEAVING+AGENCY+DAGEGRP+ JSI_new, EmpIndex_subset1)
summary(final_model)
plot(final_model$fitted.values,final_model$residuals)
ggplot(final_model, aes(x=EmpIndex_subset1$EmpIndex, y=final_model$fitted.values)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# b)logstic regression on leaving
EmpIndex_subset1$leave[DLEAVING=="N"]<-0
EmpIndex_subset1$leave[DLEAVING=="R"]<-1
EmpIndex_subset1$leave[DLEAVING=="Y"]<-1

attach(EmpIndex_subset1)

#split the dataset into training and testing (70/30)
set.seed(123)
train_idx <- sample(nrow(EmpIndex_subset1),.70*nrow(EmpIndex_subset1))
leave_train <- EmpIndex_subset1[train_idx,]
leave_test <- EmpIndex_subset1[-train_idx,]
str(leave_train) 
attach(leave_train)
logreg<- glm(leave~ EmpIndex+ JSI, family = binomial, leave_train)
summary(logreg)

logit_test<-predict(logreg,leave_test, type="response")
hist(logit_test)

#predicted probability cutoff of 0.25 
Y_hat_logit  <- as.numeric(logit_test > 0.25)

#Confusion Matrix
table(leave_test$leave, Y_hat_logit , dnn = c("Actual", "Predicted"))

#accuracy of model 
mean(leave_test$leave == Y_hat_logit)

