## R code for statistical analyses 
## of multi-year realistic BEF experiment
## Amelia Wolf
## 2021-04-20

# load required R packages, nlme for ANOVAs and reghelper for slope extraction
library(nlme)
library(reghelper)

## data available in this Github repository (ameliawolfUT/Wolf-et-al-Coyote_Ridge)

# read in biomass data from all years
biomass <- read.csv("biomassforR.csv")
# read in biomass data from all years with community-weighted means
# for PC2 and PC1
allyearspca <- read.csv("allyearsPCA.csv")
# read in trait data separated by year 2009-2010
traits2010 <- read.csv("community-weighted-means-1v2.csv")
# read in trait data separated by year 2010-2011
traits2011 <- read.csv("community-weighted-means-2v2.csv") 
# read in trait data separated by year 2011-2012
traits2012 <- read.csv("community-weighted-means-3v2.csv") 

# effects of experimental treatments on ecosystem functioning
# productivity
biomassmodelfull <- lme(sqrt(Total) ~ Treatment*Depth*Species*Year, 
	random=~1|Block, data=biomass[!is.na(biomass$Total),])
summary(biomassmodelfull)
anova(biomassmodelfull)
# invasion resistance
exoticmodelfull <- lme(log(Exotic+1) ~ Depth*Treatment*Species*Year, 
	random=~1|Block, data=biomass[!is.na(biomass$Total),])
summary(exoticmodelfull)
anova(exoticmodelfull)
# extracting simple slopes for overall effect of random vs realistic 
# species loss on productivity, holding other factors equal
biomassmodellml <- lme((Total) ~ Species*Treatment, 
	random=~1|Block, data=biomass[!is.na(biomass$Total),])
simple_slopes(biomassmodellml)

# Examining the significant three-way Species*Treatment*Year 
# interaction on Invasion Resistance - broken down by year
exotic2010 <- lme(log(Exotic2010+1) ~ Species*Depth*Treatment, 
	random=~1|Block, data=traits2010)
anova(exotic2010) #significant species*treatment interaction 

# species*treatment interaction broken down below by treatment
exotic2010r <- lme(log(Exotic2010+1) ~ Species*Depth, 
	random=~1|Block, data=traits2010[traits2010$Treatment=="Random",])
anova(exotic2010r)

exotic2010o <- lme(log(Exotic2010+1) ~ Species*Depth, 
	random=~1|Block, data=traits2010[traits2010$Treatment=="Ordered",])
anova(exotic2010o)

exotic2011 <- lme(log(Exotic2011+1) ~ Species*Depth*Treatment, 
	random=~1|Block, data=traits2011[!is.na(traits2011$Exotic2011),])
anova(exotic2012) #no species*treatment interaction

exotic2012 <- lme(log(Exotic2012+1) ~ Species*Depth*Treatment, 
	random=~1|Block, data=traits2012[!is.na(traits2012$Exotic2012),])
anova(exotic2012) #significant species*treatment interaction 

# species*treatment interaction broken down below by treatment
exotic2012r <- lme(log(Exotic2012+1) ~ Species*Depth, 
	random=~1|Block, data=traits2012[!is.na(traits2012$Exotic2012) & 
	traits2012$Treatment=="Random",])
anova(exotic2012r)

exotic2012o <- lme(log(Exotic2012+1) ~ Species*Depth, 
	random=~1|Block, data=traits2012[!is.na(traits2012$Exotic2012) & 
	traits2012$Treatment=="Ordered",])
anova(exotic2012o)

# examining cwmPC1 and cwmPC2 as effect traits
combPC1totlme <- lme(sqrt(Total) ~ PC1CWM*Year*Depth*Treatment, 
	random=~1|Block, data=allyearspca[!is.na(allyearspca$Exotic),])
anova(combPC1totlme)

combPC1exlme <- lme(log(Exotic+1) ~ PC1CWM*Year*Depth*Treatment, 
	random=~1|Block, data=allyearspca[!is.na(allyearspca$Exotic),])
anova(combPC1exlme)

combPC2totlme <- lme(sqrt(Total) ~ PC2CWM*Year*Depth*Treatment, 
	random=~1|Block, data=allyearspca[!is.na(allyearspca$Exotic),])
anova(combPC2totlme)

combPC2exlme <- lme(log(Exotic+1) ~ PC2CWM*Year*Depth*Treatment, 
	random=~1|Block, data=allyearspca[!is.na(allyearspca$Exotic),])
anova(combPC2exlme)

# examining cwmPC1 and cwmPC2 as response traits
PC1resplme <- lme(PC1CWM ~ Species*Year*Depth*Treatment, 
	random=~1|Block, data=allyearspca[!is.na(allyearspca$Exotic),])
anova(PC1resplme)

PC2resplme <- lme(PC2CWM ~ Species*Year*Depth*Treatment, 
	random=~1|Block, data=allyearspca[!is.na(allyearspca$Exotic),])
anova(PC2resplme)
