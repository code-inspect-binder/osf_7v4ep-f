### Project: "Collaboration enhances career progression in academic science, especially for female researchers"
## Authors: "Jessica E.M. van der Wal*, Rose Thorogood, and Nicholas P.C. Horrocks"
## Date: 20 July 2021
## *e-mail: jessicavanderwal1@gmail.com

### PART 2: ADJUSTING COLLABORATION BEHAVIOUR METRICS, SUMMARIES, STATISTICS, PLOTTING

# Input: "social.authors.data.csv" - Final dataset of 935 focal authors and the associated social network metrics and academic performance measures.
# Also: "social.authors.data_earlycareer.csv" - Subset of 390 focal authors and the associated social network metrics calculated for the first 10 years, and academic performance measures.

# Function: adjust measures for publication output, testing for the effects of gender, run AFT models, create graphs in manuscript.

### Methods: 
#(1) Summary statistics of raw data
#(2) Adjusting social metrics by number of papers
#(3) Effects of gender on output and adjusted collaboration metrics
#(4) PI status: effects of gender and collaborative behaviour metrics
#(5) Accelerated Time Failure (AFT) models
#(6) Figure plotting

############################################################################################
###			(1) Summary statistics of raw data
############################################################################################
library(readr)
library(jtools)
library(simr)
library(pwr)
library(ggsignif)
library(readr)
library(gridExtra)

#set working directory
setwd("C:/Users/JWAL00/Dropbox/Jes NEW/Manuscript/Proceedings B/05.07.21/R code/Part 2_Adjusting collaboration behaviour metrics, summaries, statistics, plotting")

#full dataset
new = read_csv("social.authors.data.csv")

#early career dataset
#new = read_csv("social.authors.data_earlycareer.csv")

str(new)
## Relevant columns explained:
# author.name = focal authors, anonymised, based on their first name and/or online profile
# no.of.papers = number of papers
# Gender = focal authors' inferred gender
# indegree = network size
# strength.mean = mean strength
# clusteringcoefficient.global = global clustering coefficient
# triangles = Numerator for global clustering coefficient calculation
# demoninator = denominator for global clustering coefficient calculation
# first.year = year of first publication
# last.year = year of last  publication
# n.years = career length
# first.lastauthor = year of first last-author paper 
# second.lastauthor = = year of second last-author paper 
# third.lastauthor = = year of third last-author paper 
# gap = time between first and third last-author paper
# Time.to.PI = time to PI
# mean.nAU = Number of co-authors/paper
# firstauthor.papers = number of first author papers
# lastauthor.papers = number of last author papers
# allmultipleauthor.papers = number of multi-author papers

# Female focal authors dataset
new.F = new[new$Gender=="F",] 
new.F = new.F[(!is.na(new.F$Gender)),] 
new.F$Gender = factor(new.F$Gender)#298

#Male focal authors dataset
new.M = new[new$Gender=="M",] 
new.M = new.M[(!is.na(new.M$Gender)),]
new.M$Gender = factor(new.M$Gender) #637

#SUMMARY STATISTICS

# Network size
mean(new$indegree)
median(new$indegree)
min(new$indegree)
max(new$indegree)
sd(na.omit(new$indegree)/sqrt(nrow(new)))

#F
mean(new.F$indegree)
median(new.F$indegree)
min(new.F$indegree)
max(new.F$indegree)
sd(na.omit(new.F$indegree)/sqrt(nrow(new.F)))
#M
mean(new.M$indegree)
median(new.M$indegree)
min(new.M$indegree)
max(new.M$indegree)
sd(na.omit(new.M$indegree)/sqrt(nrow(new.M)))

# Tie strength
mean(new$strength.mean)
median(new$strength.mean)
min(new$strength.mean)
max(new$strength.mean)
sd(na.omit(new$strength.mean)/sqrt(nrow(new)))

#F
mean(new.F$strength.mean)
median(new.F$strength.mean)
min(new.F$strength.mean)
max(new.F$strength.mean)
sd(na.omit(new.F$strength.mean)/sqrt(nrow(new.F)))

#M
mean(new.M$strength.mean)
median(new.M$strength.mean)
min(new.M$strength.mean)
max(new.M$strength.mean)
sd(na.omit(new.M$strength.mean)/sqrt(nrow(new.M)))

#Clustering coefficient
mean(na.omit(new$clusteringcoefficient.global))
median(na.omit(new$clusteringcoefficient.global))
min(na.omit(new$clusteringcoefficient.global))
max(na.omit(new$clusteringcoefficient.global))
sd(na.omit(new$clusteringcoefficient.global))/sqrt(na.omit(nrow(new[!is.na(new$clusteringcoefficient.global),])))

#F
mean(na.omit(new.F$clusteringcoefficient.global))
median(na.omit(new.F$clusteringcoefficient.global))
min(na.omit(new.F$clusteringcoefficient.global))
max(na.omit(new.F$clusteringcoefficient.global))
sd(na.omit(new.F$clusteringcoefficient.global))/sqrt(na.omit(nrow(new.F[!is.na(new.F$clusteringcoefficient.global),])))

#M
mean(na.omit(new.M$clusteringcoefficient.global))
median(na.omit(new.M$clusteringcoefficient.global))
min(na.omit(new.M$clusteringcoefficient.global))
max(na.omit(new.M$clusteringcoefficient.global))
sd(na.omit(new.M$clusteringcoefficient.global))/sqrt(na.omit(nrow(new.M[!is.na(new.M$clusteringcoefficient.global),])))

# Number of co-authors/paper
hist(log(new$mean.nAU))
hist(new$mean.nAU)
mean(new$mean.nAU)
median(new$mean.nAU)
min(new$mean.nAU)
max(new$mean.nAU)
sd(new$mean.nAU)/sqrt(nrow(new))
plot(new$mean.nAU~new$no.of.papers)

#F
hist(log(new.F$mean.nAU))
hist(new.F$mean.nAU)
mean(new.F$mean.nAU)
median(new.F$mean.nAU)
min(new.F$mean.nAU)
max(new.F$mean.nAU)
sd(new.F$mean.nAU)/sqrt(nrow(new.F))
plot(new.F$mean.nAU~new.F$no.of.papers)

#M
hist(log(new.M$mean.nAU))
hist(new.M$mean.nAU)
mean(new.M$mean.nAU)
median(new.M$mean.nAU)
min(new.M$mean.nAU)
max(new.M$mean.nAU)
sd(new.M$mean.nAU)/sqrt(nrow(new.M))
plot(new.M$mean.nAU~new.M$no.of.papers)

# Career length
hist(new$n.years,xlab="number of publication years", main="")
abline(v = mean(new$n.years),col = "royalblue",lwd = 2)
mean(new$n.years)
median(new$n.years)
min(new$n.years)
max(new$n.years)
sd(na.omit(new$n.years)/sqrt(nrow(new)))

#F
mean(new.F$n.years)
median(new.F$n.years)
min(new.F$n.years)
max(new.F$n.years)
sd(na.omit(new.F$n.years)/sqrt(nrow(new.F)))

#M
mean(new.M$n.years)
median(new.M$n.years)
min(new.M$n.years)
max(new.M$n.years)
sd(na.omit(new.M$n.years)/sqrt(nrow(new.M)))

## Number of papers
hist(new$no.of.papers, xlab="number of papers", main="")
abline(v = mean(new$no.of.papers),col = "royalblue",lwd = 2)
max(new$no.of.papers)
min(new$no.of.papers)
mean(new$no.of.papers)
median(new$no.of.papers)
sd(new$no.of.papers)/sqrt(nrow(new))


############################################################################################
###			(2) Adjusting social metrics by number of papers
############################################################################################
library(readr) 
library(lmvar) 
library(lme4) 
library(jtools) 
library(emmeans)
library(sjstats)
library(ResourceSelection)
library(car)
library(rsq)
library(ggplot2)
library(doBy)

## Read in data provided with Script
new = read_csv("~/social.authors.data.csv")

#make 'male' the reference throughout
new$Gender = factor(new$Gender, levels = c("M", "F"))

#Gender composition
table(new$Gender)

#Total number of papers
sum(new$no.of.papers) 

#number of papers per gender
summaryBy(no.of.papers~Gender, data =new, FUN = sum) 
41337/11361 

###(2.1) GET MEASURES ADJUSTED FOR NUMBER OF PAPERS (RESIDUALS)

### Get residuals of network size = adjusted network size
plot(new$indegree~new$no.of.papers)
model_networksize = glm(indegree~no.of.papers:Gender+no.of.papers, family="poisson", data = new) 
model_networksize = glm(indegree~no.of.papers:Gender, family="poisson", data = new) 
hist(resid(model_networksize))
qqnorm(resid(model_networksize))
summary(model_networksize)
summ(model_networksize)
#add new measure to dataframe
new$residuals.degree = resid(model_networksize)

# Scale
new$residuals.degree  = scale(new$residuals.degree)
new$residuals.degree = as.numeric(new$residuals.degree)
hist(new$residuals.degree)

#### Get residuals of tie strength = adjusted tie strength

#model_tiestrength = glm(strength.mean~no.of.papers:Gender, family="gaussian", data = new) 
# model with strength not transformed looked a bit dodgy, so we log-transformed strength
model_tiestrength = glm(log(strength.mean)~no.of.papers:Gender+no.of.papers, family="gaussian", data = new) 
model_tiestrength = glm(log(strength.mean)~no.of.papers:Gender, family="gaussian", data = new) 
hist(resid(model_tiestrength), main = "residuals of model with log(tie strength)")
qqnorm(resid(model_tiestrength))
summary(model_tiestrength) 
summ(model_tiestrength)
new$residuals.strength <- resid(model_tiestrength)
hist(new$residuals.strength)

# Scale
new$residuals.strength <- scale(new$residuals.strength)
new$residuals.strength <- as.numeric(new$residuals.strength)
plot(new$residuals.strength~new$no.of.papers)

hist(new$residuals.strength)

#### Get residuals of clustering coefficient = adjusted clustering coefficient
model_cluster <- glm(cbind(triangles,demoninator-triangles)~no.of.papers:Gender+no.of.papers,data=new,family="binomial", na.action = na.exclude)
model_cluster <- glm(cbind(triangles,demoninator-triangles)~no.of.papers:Gender,data=new,family="binomial", na.action = na.exclude)
hist(resid(model_cluster))
qqnorm(resid(model_cluster))
new$residuals.cluster <- resid(model_cluster)
summ(model_cluster) 
hist(new$residuals.cluster)

# Scale
new$residuals.cluster = scale(new$residuals.cluster)
new$residuals.cluster = as.numeric(new$residuals.cluster)
hist(new$residuals.cluster)

plot(new$residuals.cluster, new$residuals.cluster1)

### So now we have three sets of residuals to work with 

# Residuals.degree = adjusted network size
# Residuals.strength = adjusted tie strength
# Residuals.cluster = adjusted clustering coefficient

### Check how  measures are correlated with number of papers (Fig. ESM5.1)
new.corr = new
names(new.corr)[names(new.corr) == "indegree"] <- "Network Size"
names(new.corr)[names(new.corr) == "strength.mean"] <- "Tie Strength"
names(new.corr)[names(new.corr) == "clusteringcoefficient.global"] <- "Cluster. Coeff."
names(new.corr)[names(new.corr) == "no.of.papers"] <- "No. of Papers"
names(new.corr)[names(new.corr) == "residuals.degree"] <- "Adj. Network Size"
names(new.corr)[names(new.corr) == "residuals.strength"] <- "Adj. Tie Strength"
names(new.corr)[names(new.corr) == "residuals.cluster"] <- "Adj. Cluster. coef."

select <- c("Network Size", "Tie Strength","Cluster. Coeff.","No. of Papers","Adj. Network Size","Adj. Tie Strength","Adj. Cluster. coef.")

panel.cor <- function(x,y,digits = 2,prefix = "",cex.cor,...)
{
   usr <- par("usr")
   on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   r <- abs(cor(x, y))
   txt <- format(c(r, 0.23456789), digits = digits)[1]
   txt <- paste0(prefix, txt)
   test <- cor.test(x,y)
   Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
   text(0.5, 0.5, paste("r=",txt))
   text(.5, .6, Signif)
}

panel.hist <- function(x, ...)
{
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(usr[1:2], 0, 1.5) )
   h <- hist(x,col = "light gray", plot = FALSE)
   breaks <- h$breaks; nB <- length(breaks)
   y <- h$counts; y <- y/max(y)
   abline(v = mean(x),col = "royalblue",lwd = 2)
   rect(breaks[-nB], 0, breaks[-1], y, col="white",border="black", ...)
}

pairs(na.omit(new.corr[, select]),upper.panel = panel.cor,diag.panel=panel.hist,cex.labels=1.25, pch=20, font.labels =2) 
par(mfrow = c(1,1)) #reset par to default for subsequent plots

# Look at statistic for each adjusted measure for paper

#Female focal authors dataset
new.F = new[new$Gender=="F",] 
new.F = new.F[(!is.na(new.F$Gender)),] 
new.F$Gender = factor(new.F$Gender)#298

#Male focal authors dataset
new.M = new[new$Gender=="M",] 
new.M = new.M[(!is.na(new.M$Gender)),]
new.M$Gender = factor(new.M$Gender) #637

#Adjusted network size
#F
mean(new.F$residuals.degree)
median(new.F$residuals.degree)
min(new.F$residuals.degree)
max(new.F$residuals.degree)
sd(new.F$residuals.degree)/sqrt(nrow(new.F))

#M
mean(new.M$residuals.degree)
median(new.M$residuals.degree)
min(new.M$residuals.degree)
max(new.M$residuals.degree)
sd(new.M$residuals.degree)/sqrt(nrow(new.M))

#Adjusted tie strength
#F
mean(new.F$residuals.strength)
median(new.F$residuals.strength)
min(new.F$residuals.strength)
max(new.F$residuals.strength)
sd(new.F$residuals.strength)/sqrt(nrow(new.F))

#M
mean(new.M$residuals.strength)
median(new.M$residuals.strength)
min(new.M$residuals.strength)
max(new.M$residuals.strength)
sd(new.M$residuals.strength)/sqrt(nrow(new.M))

#Adjusted clustering coeff
#F
mean(na.omit(new.F$residuals.cluster))
median(na.omit(new.F$residuals.cluster))
min(na.omit(new.F$residuals.cluster))
max(na.omit(new.F$residuals.cluster))
sd(na.omit(new.F$residuals.cluster))/sqrt(nrow(na.omit(new.F)))

#M
mean(na.omit(new.M$residuals.cluster))
median(na.omit(new.M$residuals.cluster))
min(na.omit(new.M$residuals.cluster))
max(na.omit(new.M$residuals.cluster))
sd(na.omit(new.M$residuals.cluster))/sqrt(nrow(na.omit(new.M)))

############################################################################################
###			(3) Effects of gender on publication output and collaboration behaviour metrics
############################################################################################
library(plyr) 
library(DHARMa)
# Number of papers
plot(new$no.of.papers~new$Gender)
model = glm(no.of.papers~Gender, data=new, family=poisson())
#check that result holds if we add career length as an offset --. it does
model = glm(no.of.papers~Gender, offset = log(n.years), data=new, family=poisson())
hist(resid(model))
qqnorm(resid(model))
summary(model)
summ(model) # if exp = TRUE, then "reports exponentiated coefficients with confidence intervals for expo-nential models like logit and Poisson models. This quantity is known as an oddsratio for binary outcomes and incidence rate ratio for count models" that's cool
hist(new$no.of.papers)
#get number of papers (and other stats) per gender
ddply(new, "Gender",summarise,
      mean = mean(no.of.papers, na.rm = TRUE),  
      median = median(no.of.papers, na.rm = TRUE),
      sd = sd(no.of.papers, na.rm = TRUE),
      N = sum(!is.na(no.of.papers)),
      se = sd / sqrt(N),
      min = min(no.of.papers, na.rm = TRUE),
      max = max(no.of.papers, na.rm = TRUE)) 

####
## GLM stats
####

# Degree
model = glm(indegree~Gender,data = new, family="poisson") 
hist(resid(model))
qqnorm(resid(model))
summary(model)
summ(model) # if exp = TRUE, then "reports exponentiated coefficients with confidence intervals for expo-nential models like logit and Poisson models. This quantity is known as an oddsratio for binary outcomes and incidence rate ratio for count models" that's cool

# Consistency
model = lm(log(strength.mean)~Gender, data = new)
hist(resid(model))
qqnorm(resid(model))
summary(model) 
summ(model) # if exp = TRUE, then "reports exponentiated coefficients with confidence intervals for expo-nential models like logit and Poisson models. This quantity is known as an oddsratio for binary outcomes and incidence rate ratio for count models" that's cool

# Global clustering coefficient
model <- glm(cbind(triangles,demoninator-triangles)~as.factor(Gender),data=new,family="binomial")
hist(resid(model))
qqnorm(resid(model))
summary(model)
summ(model) # if exp = TRUE, then "reports exponentiated coefficients with confidence intervals for expo-nential models like logit and Poisson models. This quantity is known as an oddsratio for binary outcomes and incidence rate ratio for count models" that's cool

# Adjusted network size
model = lm(new$residuals.degree~as.factor(new$Gender))
hist(resid(model))
qqnorm(resid(model))
summary(model) 
summ(model) 

# Adjusted tie strength
model = lm(new$residuals.strength~as.factor(new$Gender))
hist(resid(model))
qqnorm(resid(model))
summary(model) 
summ(model) 

# Adjusted clustering coefficient
model = lm(new$residuals.cluster~as.factor(new$Gender))
hist(resid(model))
qqnorm(resid(model))
summary(model)
summ(model)

# Gender difference of author position?
new$prestige = new$lastauthor.papers+new$firstauthor.papers
model = glm(cbind(prestige,no.of.papers-prestige)~as.factor(Gender)+first.year,data =new[(new$Gender!="UNK"),],family=binomial)
hist(resid(model))
summary(model)
summ(model)

ddply(new, "Gender",summarise,
      sum.multi = sum(allmultipleauthor.papers, na.rm = TRUE),
      sum.prestige = sum(prestige, na.rm = TRUE),
      sum.prestige/sum.multi) 

### Fig ESM5.2###

#Fig ESM5.2A
A = ggplot(data=new, aes(x=Gender, y=indegree, colour=Gender))+geom_violin(trim=FALSE)+
   geom_jitter(position=position_jitter(0.4), shape=16, size = 0.5)+labs(tag = "A")+
   stat_summary(fun.y = "mean", geom = "point", pch = "_", size = 10, colour = "black")+
   theme_bw()+ylab("Network Size")+xlab("")+
   theme(panel.grid.major = element_blank(),
         legend.position="none" ,legend.title=element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"))+
   theme(axis.title.y = element_text( size=12, colour="black"),
         axis.title.x = element_text( size=12, colour="black"),
         axis.text.x  = element_text(size=12, colour="black"),
         axis.text.y= element_text(size=12, colour="black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"),
         panel.border = element_rect(colour = "black"))+
   scale_color_manual(values=c("F"="darkorange", "M" = "darkslateblue"))+
   geom_signif(comparisons = list(c("F", "M")),  colour = "black",
               map_signif_level=TRUE,   textsize = 5)

#Fig ESM5.2B
B = ggplot(data=new, aes(x=Gender, y=strength.mean, colour=Gender))+geom_violin(trim=FALSE)+
   geom_jitter(position=position_jitter(0.4), shape=16, size = 0.5)+labs(tag = "B")+
   stat_summary(fun.y = "mean", geom = "point", pch = "_", size = 10, colour = "black")+
   theme_bw()+ylab("Tie Strength")+xlab("")+
   theme(panel.grid.major = element_blank(),
         legend.position="none" ,legend.title=element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"))+
   theme(axis.title.y = element_text( size=12, colour="black"),
         axis.title.x = element_text( size=12, colour="black"),
         axis.text.x  = element_text(size=12, colour="black"),
         axis.text.y= element_text(size=12, colour="black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"),
         panel.border = element_rect(colour = "black"))+
   scale_color_manual(values=c("F"="darkorange", "M" = "darkslateblue"))+
   geom_signif(comparisons = list(c("F", "M")),  colour = "black",
               map_signif_level=TRUE,  textsize = 5) + ylim(0,7.6)

#Fig ESM5.2C
C = ggplot(data=new, aes(x=Gender, y=clusteringcoefficient.global, colour=Gender))+geom_violin(trim=FALSE)+
   geom_jitter(position=position_jitter(0.4), shape=16, size = 0.5)+labs(tag = "C")+
   stat_summary(fun.y = "mean", geom = "point", pch = "_", size = 10, colour = "black")+
   theme_bw()+ylab("Clustering Coefficient")+xlab("")+
   theme(panel.grid.major = element_blank(),
         legend.position="none" ,legend.title=element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"))+
   theme(axis.title.y = element_text( size=12, colour="black"),
         axis.title.x = element_text( size=12, colour="black"),
         axis.text.x  = element_text(size=12, colour="black"),
         axis.text.y= element_text(size=12, colour="black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"),
         panel.border = element_rect(colour = "black"))+
   scale_color_manual(values=c("F"="darkorange", "M" = "darkslateblue"))+
   geom_signif(comparisons = list(c("F", "M")),  colour = "black",
               map_signif_level=TRUE,  textsize = 5)+ylim(0,1.1)

grid.arrange(arrangeGrob(A,B,C,ncol=3))

############################################################################################
###			(4) PI STATUS: EFFECTS OF GENDER AND COLLABORATIVE BEHAVIOUR METRICS 
############################################################################################


#status.PI is a binary reponse (1 = achieved PI status, 0 = never achieved PI status)
new$status.PI = ifelse(!is.na(new$Time.to.PI),1,0)
#table(new$status.PI, new$last.year)
#table(new$status.PI, new$Gender)

#Binary response: hence will require logistic regression

#Does gender predict the likelihood of being a PI ((regardless of how long it takes to actually become a PI?)
model1<-glm(status.PI~Gender,family="binomial",data=new)
summ(model1) 
#Gender significantly predicts likelihood of becoming a PI

#Do any of the collaborative behaviour metrics predict likelihood of becoming a PI (regardless of how long it takes to actually become
#a PI)? #Likelihood of becoming a PI

#Adjusted network size
plot(new$residuals.degree~new$status.PI)
m1<-glm(as.factor(status.PI)~Gender*residuals.degree,family="binomial",data=new)
summ(m1) 
m1<-glm(as.factor(status.PI)~Gender+residuals.degree,family="binomial",data=new)
summ(m1) 
#to get odds ratio (= effect size)
summ(m1, exp = TRUE)

#Check for influential values
plot(m1, which=4, id.n=3) #Three most extreme values plotted as Cook's distance values
model.data <- augment(m1) %>%    mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd) #The data for the top 3 largest values, according to the Cook’s distance
ggplot(model.data, aes(index, .std.resid)) +    geom_point(aes(color = new$status.PI), alpha = .5) +   theme_bw()
model.data %>%    filter(abs(.std.resid) > 3)
#Only three data points with standardized residuals > 3 (and they are only just above 3), which could be potential #outliers. 

#Check for collinearity
vif(m1)#All variables have VIF value much less than 5 or 10, so no evidence of collinearity

#See http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
#for more details.

#Adjusted tie strength
m2<-glm(status.PI~Gender*residuals.strength,family="binomial",data=new)
m2<-glm(status.PI~Gender:residuals.strength,family="binomial",data=new)
m2<-glm(status.PI~Gender+residuals.strength,family="binomial",data=new)
summary(m2)
summ(m2) 
#to get odds ratio (= effect size)
summ(m2, exp = TRUE) #1.42[1.02, 1.97]

#Check for influential values
plot(m2, which=4, id.n=3) #Three most extreme values plotted as Cook's distance values
model.data <- augment(m2) %>%    mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd) #The data for the top 3 largest values, according to the Cook’s distance
ggplot(model.data, aes(index, .std.resid)) +    geom_point(aes(color = status.PI), alpha = .5) +   theme_bw()
model.data %>%    filter(abs(.std.resid) > 3)#No outliers
#Check for collinearity
vif(m2) #All variables have VIF value much less than 5 or 10, so no evidence of collinearity

#Adjusted clustering coefficient
m3<-glm(status.PI~Gender*residuals.cluster,family="binomial",data=new)
m3<-glm(status.PI~Gender+residuals.cluster,family="binomial",data=new)
summ(m3)
#to get odds ratio (= effect size)
summ(m3, exp = TRUE)

#Check for influential values
plot(m3, which=4, id.n=3) #Three most extreme values plotted as Cook's distance values
model.data <- augment(m3) %>%    mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd) #The data for the top 3 largest values, according to the Cook’s distance
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = status.PI), alpha = .5) +theme_bw()
model.data %>%    filter(abs(.std.resid) > 3)#No outliers

#Check for collinearity
vif(m3) #All variables have VIF value much less than 5 or 10, so no evidence of collinearity

############################################################################################
###			(5) Accelerated Time Failure (AFT) models
############################################################################################
library(survival) 
library(plyr) 
library(readr) 
library(reshape2) 
library(rms)
library(ggplot2) 
library(flexsurv)
library(e1071) 
library(KMsurv)
library(survRM2) 
library(car)
library(tidyverse) 
library(broom)

#####
## SHORTCUT to final analyses
#####

##NOTE: Run all the code in section 2 first.

########
###TIME TO PI (time.PI)
########
new.PI = new
# Create censoring VARIABLE
# PI status: 1 if PI, 0 if author did not make it to PI
new.PI$status.PI = ifelse(!is.na(new.PI$Time.to.PI),1,0)

#exclude authors that did not become PI and also left science
new.PI = new.PI[new.PI$status.PI==1|(new.PI$status.PI==0&new.PI$last.year>2015),] #935-772=163 authors out
#check
table(new.PI$status.PI, new.PI$last.year)

#GAP
#hist(new.PI$gap)
#mean(na.omit(new.PI[new.PI$status.PI==1,]$gap)) #5.6
#sd(na.omit(new.PI[new.PI$status.PI==1,]$gap))# 4.43. #5.6+4.4=9.6
#new.PI$exclude = ifelse(new.PI$status.PI==1&new.PI$gap>10,1,0)
#new.PI1 = new.PI[new.PI$exclude==0,]
#new.PI.exclude = new.PI[new.PI$exclude==1,]
#table(new.PI.exclude$Gender)
#new.PI = new.PI1
#run to investigate how restricting time from first to second last author papers affects the results. It does not.


#Create new.PI variable: time to PI, or career length if didn't become a PI.
new.PI$time.PI = ifelse(!is.na(new.PI$Time.to.PI),new.PI$Time.to.PI,new.PI$n.years)

new.PI$time.PI[new.PI$time.PI<1]= 0.5 #This accounts for 1 individual who reached PI status the same year as they started publishing

table(new.PI$status.PI) #64,708
table(new.PI$Gender) #553,219

table(new.PI$status.PI,new.PI$Gender)
#   M   F
#0 32 32
#1 521 187

#64/772 = 8% censored
#187/(32+187) = 85% of women become PI
#521/(32+521) = 94% of men become PI

new.PI$indegree.scale = scale(new.PI$indegree)
new.PI$strength.mean.scale = scale(new.PI$strength.mean)
new.PI$cluster.scale = scale(new.PI$clusteringcoefficient.global)
#Female focal authors dataset
new.PI.F = new.PI[new.PI$Gender=="F",] 
new.PI.F = new.PI.F[(!is.na(new.PI.F$Gender)),] 
new.PI.F$Gender = factor(new.PI.F$Gender)#219

#Male focal authors dataset
new.PI.M = new.PI[new.PI$Gender=="M",] 
new.PI.M = new.PI.M[(!is.na(new.PI.M$Gender)),]
new.PI.M$Gender = factor(new.PI.M$Gender) #6553

#Gender  - effect of Gender on time to PI
flexgender<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender, dist="lnorm", data=new.PI)
flexgender

###raw measures###

## network size
flexAFT2<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender*indegree.scale, dist="lnorm", data=new.PI)
flexAFT2
vif(flexAFT2)

# CIs for interaction term overlaps 1. Therefore remove this term.
flexAFTPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+indegree.scale, dist="lnorm", data=new.PI)
flexAFTPI

#F
flexAFT_F_collabPI<-flexsurvreg(Surv(time.PI,status.PI)~indegree.scale, dist="lnorm", data=new.PI.F)
flexAFT_F_collabPI
#M
flexAFT_M_collabPI<-flexsurvreg(Surv(time.PI,status.PI)~indegree.scale, dist="lnorm", data=new.PI.M)
flexAFT_M_collabPI

## tie strength
flexAFT_consistPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender*strength.mean.scale, dist="lnorm", data=new.PI)
flexAFT_consistPI
vif(flexAFT_consistPI)

# CIs for interaction term overlaps 1. Therefore remove this term.
flexAFT_consistPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+strength.mean.scale, dist="lnorm", data=new.PI)
flexAFT_consistPI
#F
flexAFT_F_consistPI<-flexsurvreg(Surv(new.PI.F$time.PI,new.PI.F$status.PI)~strength.mean.scale, dist="lnorm", data=new.PI.F)
flexAFT_F_consistPI
#M
flexAFT_M_consistPI<-flexsurvreg(Surv(new.PI.M$time.PI,new.PI.M$status.PI)~strength.mean.scale, dist="lnorm", data=new.PI.M)
flexAFT_M_consistPI

## clustering coefficient
flexAFT_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender*cluster.scale, dist="lnorm", data=new.PI)
flexAFT_connectPI

# CIs for interaction term overlaps 1. Therefore remove this term.
flexAFT_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender+cluster.scale, dist="lnorm", data=new.PI)
flexAFT_connectPI
#F
flexAFT_F_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~cluster.scale, dist="lnorm", data=new.PI.F)
flexAFT_F_connectPI
#M
flexAFT_M_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~cluster.scale, dist="lnorm", data=new.PI.M)
flexAFT_M_connectPI

###adjusted measures###

##Adjusted network size
flexAFT2<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender*residuals.degree, dist="lnorm", data=new.PI)
flexAFT2
vif(flexAFT2)

# CIs for interaction term overlaps 1. Therefore remove this term.
flexAFTPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+residuals.degree, dist="lnorm", data=new.PI)
flexAFTPI

#F
flexAFT_F_collabPI<-flexsurvreg(Surv(time.PI,status.PI)~residuals.degree, dist="lnorm", data=new.PI.F)
flexAFT_F_collabPI
#M
flexAFT_M_collabPI<-flexsurvreg(Surv(time.PI,status.PI)~residuals.degree, dist="lnorm", data=new.PI.M)
flexAFT_M_collabPI

## Adjusted tie strength
flexAFT_consistPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender*residuals.strength, dist="lnorm", data=new.PI)
flexAFT_consistPI
vif(flexAFT_consistPI)
# CIs for interaction term overlaps 1. Therefore remove this term.
flexAFT_consistPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+residuals.strength, dist="lnorm", data=new.PI)
flexAFT_consistPI
#F
flexAFT_F_consistPI<-flexsurvreg(Surv(new.PI.F$time.PI,new.PI.F$status.PI)~residuals.strength, dist="lnorm", data=new.PI.F)
flexAFT_F_consistPI
#M
flexAFT_M_consistPI<-flexsurvreg(Surv(new.PI.M$time.PI,new.PI.M$status.PI)~residuals.strength, dist="lnorm", data=new.PI.M)
flexAFT_M_consistPI

## Adjusted clustering coefficient
flexAFT_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender*residuals.cluster, dist="lnorm", data=new.PI)
flexAFT_connectPI

vif(flexAFT_connectPI)
# CIs for interaction term overlaps 1. Therefore remove this term.
flexAFT_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender+residuals.cluster, dist="lnorm", data=new.PI)
flexAFT_connectPI
#F
flexAFT_F_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~residuals.cluster, dist="lnorm", data=new.PI.F)
flexAFT_F_connectPI
#M
flexAFT_M_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~residuals.cluster, dist="lnorm", data=new.PI.M)
flexAFT_M_connectPI

## CAREER LENGTH

### Create censoring VARIABLE
#Rule: if author stopped publishing in 2015 or earlier, 0 if not 
new$status = ifelse(new$last.year<2016, 1, 0) #This gives data for censoring
#new$status = ifelse(new$last.year<2014, 1, 0) #Run to see how different censoring cut-off affects results
#new$status = ifelse(new$last.year<2012, 1, 0) #Run to see how different censoring cut-off affects results

#status indicates whether the event (stopped publishing) was observed (status = 1) or that event did not happen before end of study  (status = 0).
table(new$status) #for 293 out of 935 authors in our dataset the event happened, i.e. they were still publishing at the end of our study (defined as 2016 or later)
table(new$status, new$Gender) #123 women filtered out
#123/(123+175) = 41%
#170/(170+467) = 27%
#642/935 * 100 = 69%
#175/642* 100 = 27%

new$indegree.scale = scale(new$indegree)
new$strength.mean.scale = scale(new$strength.mean)
new$cluster.scale = scale(new$clusteringcoefficient.global)

#Female focal authors dataset
new.F = new[new$Gender=="F",] 
new.F = new.F[(!is.na(new.F$Gender)),] 
new.F$Gender = factor(new.F$Gender)#298

#Male focal authors dataset
new.M = new[new$Gender=="M",] 
new.M = new.M[(!is.na(new.M$Gender)),]
new.M$Gender = factor(new.M$Gender) #637

#Gender 
flexgender<-flexsurvreg(Surv(new$n.years,new$status)~Gender, dist="lnorm", data=new)
flexgender

###raw measures

## network size
flexAFT2<-flexsurvreg(Surv(new$n.years,new$status)~Gender*indegree.scale, dist="lnorm", data=new)
flexAFT2
vif(flexAFT2)

# F
flexAFT_F_collab<-flexsurvreg(Surv(new.F$n.years,new.F$status)~indegree.scale, dist="lnorm", data=new.F)
flexAFT_F_collab

# M
flexAFT_M_collab<-flexsurvreg(Surv(new.M$n.years,new.M$status)~indegree.scale, dist="lnorm", data=new.M)
flexAFT_M_collab

## tie strength
AFT_consist<-flexsurvreg(Surv(new$n.years,new$status)~Gender*strength.mean.scale, dist="lognormal", data=new)
AFT_consist
# CIs for interaction term overlaps 1. Therefore remove this term.
AFT_consist<-flexsurvreg(Surv(new$n.years,new$status)~Gender+strength.mean.scale, dist="lognormal", data=new)
AFT_consist

# F
flexAFT_F_consist<-flexsurvreg(Surv(new.F$n.years,new.F$status)~strength.mean.scale, dist="lnorm", data=new.F)
flexAFT_F_consist
# M
flexAFT_M_consist<-flexsurvreg(Surv(new.M$n.years,new.M$status)~strength.mean.scale, dist="lnorm", data=new.M)
flexAFT_M_consist

## clustering coefficient
flexAFT_connect<-flexsurvreg(Surv(n.years,status)~Gender*cluster.scale, dist="lnorm", data=new)
flexAFT_connect
# CIs for interaction term overlaps 1. Therefore remove this term.
flexAFT_connect<-flexsurvreg(Surv(n.years,status)~Gender+cluster.scale, dist="lnorm", data=new)
flexAFT_connect
# F
flexAFT_F_connect<-flexsurvreg(Surv(n.years,status)~cluster.scale, dist="lnorm", data=new.F)
flexAFT_F_connect
# M 
flexAFT_M_connect<-flexsurvreg(Surv(n.years,status)~cluster.scale, dist="lnorm", data=new.M)
flexAFT_M_connect

###adjusted measures

## Adjusted network size
flexAFT2<-flexsurvreg(Surv(new$n.years,new$status)~Gender*residuals.degree, dist="lnorm", data=new)
flexAFT2
vif(flexAFT2)

## CIs for interaction term overlaps 1. Therefore remove this term (only for early career)
flexAFT2<-flexsurvreg(Surv(new$n.years,new$status)~Gender+residuals.degree, dist="lnorm", data=new)
flexAFT2
vif(flexAFT2)

# F
flexAFT_F_collab<-flexsurvreg(Surv(new.F$n.years,new.F$status)~residuals.degree, dist="lnorm", data=new.F)
flexAFT_F_collab

# M
flexAFT_M_collab<-flexsurvreg(Surv(new.M$n.years,new.M$status)~residuals.degree, dist="lnorm", data=new.M)
flexAFT_M_collab

## Adjusted tie strength
AFT_consist<-flexsurvreg(Surv(new$n.years,new$status)~Gender*residuals.strength, dist="lognormal", data=new)
AFT_consist
# CIs for interaction term overlaps 1. Therefore remove this term.
AFT_consist<-flexsurvreg(Surv(new$n.years,new$status)~Gender+residuals.strength, dist="lognormal", data=new)
AFT_consist

# F
flexAFT_F_consist<-flexsurvreg(Surv(new.F$n.years,new.F$status)~residuals.strength, dist="lnorm", data=new.F)
flexAFT_F_consist
# M
flexAFT_M_consist<-flexsurvreg(Surv(new.M$n.years,new.M$status)~residuals.strength, dist="lnorm", data=new.M)
flexAFT_M_consist

## Adjusted clustering coefficient
flexAFT_connect<-flexsurvreg(Surv(n.years,status)~Gender*residuals.cluster, dist="lnorm", data=new)
flexAFT_connect
# CIs for interaction term overlaps 1. Therefore remove this term.
flexAFT_connect<-flexsurvreg(Surv(n.years,status)~Gender+residuals.cluster, dist="lnorm", data=new)
flexAFT_connect
# F
flexAFT_F_connect<-flexsurvreg(Surv(n.years,status)~residuals.cluster, dist="lnorm", data=new.F)
flexAFT_F_connect
# M 
flexAFT_M_connect<-flexsurvreg(Surv(n.years,status)~residuals.cluster, dist="lnorm", data=new.M)
flexAFT_M_connect

####
#look at subset: authors that became PI's, time = time from when they became PI to end of publication. Event = stopped publishing
new.PIsonly = new[new$status.PI==1,]
#find year they became PI
new.PIsonly$yearPI = new.PIsonly$first.year + new.PIsonly$Time.to.PI
#find time from when they became PI until they stopped publishing
new.PIsonly$n.years.PI = new.PIsonly$last.year - new.PIsonly$yearPI
new.PIsonly$status = ifelse(new.PIsonly$last.year<2016, 1, 0) #This gives data for censoring
new.PIsonly$n.years.PI[new.PIsonly$n.years.PI<1]= 0.5 #We set a value of 0.5 for individuals who stopped publishing the same year they became PI
#new.PIsonly = new.PIsonly[new.PIsonly$n.years.PI>0.5,] #run to see how excluding the 18 authors that stopped publishing the year they become PI affected the results. They did not.
table(new.PIsonly$Gender)
flexgender<-flexsurvreg(Surv(new.PIsonly$n.years.PI,new.PIsonly$status)~Gender, dist="lnorm", data=new.PIsonly)
flexgender #0.51 [0.32, 0.82]

new.PIsonly2 <- new.PIsonly

#get mean, SE and range per gender
new.PIsonly.F = new.PIsonly[new.PIsonly$Gender=="F",] 
new.PIsonly.F = new.PIsonly.F[(!is.na(new.PIsonly.F$Gender)),] 
new.PIsonly.F$Gender = factor(new.PIsonly.F$Gender)#187

new.PIsonly.M = new.PIsonly[new.PIsonly$Gender=="M",] 
new.PIsonly.M = new.PIsonly.M[(!is.na(new.PIsonly.M$Gender)),] 
new.PIsonly.M$Gender = factor(new.PIsonly.M$Gender)#521

#F
mean(na.omit(new.PIsonly.F$n.years.PI))
median(na.omit(new.PIsonly.F$n.years.PI))
min(na.omit(new.PIsonly.F$n.years.PI))
max(na.omit(new.PIsonly.F$n.years.PI))
sd(na.omit(new.PIsonly.F$n.years.PI))/sqrt(na.omit(nrow(new.PIsonly.F[!is.na(new.PIsonly.F$n.years.PI),])))

mean(na.omit(new.PIsonly.M$n.years.PI))
median(na.omit(new.PIsonly.M$n.years.PI))
min(na.omit(new.PIsonly.M$n.years.PI))
max(na.omit(new.PIsonly.M$n.years.PI))
sd(na.omit(new.PIsonly.M$n.years.PI))/sqrt(na.omit(nrow(new.PIsonly.M[!is.na(new.PIsonly.M$n.years.PI),])))

### Now follows the long code on how we go to that:

########
###5.1 TIME TO PI (time.PI)
########

# Create new censoring variable: 0 if author did not make it to PI

# all authors
new.PI$time.PI = ifelse(!is.na(new.PI$Time.to.PI),new.PI$Time.to.PI,new.PI$n.years)
new.PI$status.PI = ifelse(!is.na(new.PI$Time.to.PI),1,0)
new.PI$time.PI[new.PI$time.PI<1]= 0.5 #This accounts for 1 individual who reached PI status the same year as they started publishing

#Female
table(!is.na(new.PI.F$Time.to.PI)) 
new.PI.F$time.PI = ifelse(!is.na(new.PI.F$Time.to.PI),new.PI.F$Time.to.PI,new.PI.F$n.years)
new.PI.F$status.PI = ifelse(!is.na(new.PI.F$Time.to.PI),1,0)

hist(new.PI.F$Time.to.PI,xlab="time to PI", main="")
abline(v = mean(new.PI.F$Time.to.PI),col = "royalblue",lwd = 2)
mean(na.omit(new.PI.F$Time.to.PI))
median(na.omit(new.PI.F$Time.to.PI))
min(na.omit(new.PI.F$Time.to.PI))
max(na.omit(new.PI.F$Time.to.PI))
sd(na.omit(new.PI.F$Time.to.PI))/sqrt(na.omit(nrow(new.PI.F[!is.na(new.PI.F$Time.to.PI),])))

#Male
table(!is.na(new.PI.M$Time.to.PI))
new.PI.M$time.PI = ifelse(!is.na(new.PI.M$Time.to.PI),new.PI.M$Time.to.PI,new.PI.M$n.years)
new.PI.M$status.PI = ifelse(!is.na(new.PI.M$Time.to.PI),1,0)

hist(new.PI.M$time.PI,xlab="time to PI", main="")
abline(v = mean(new.PI.M$time.PI),col = "royalblue",lwd = 2)
mean(na.omit(new.PI.M$Time.to.PI))
median(na.omit(new.PI.M$Time.to.PI))
min(na.omit(new.PI.M$Time.to.PI))
max(na.omit(new.PI.M$Time.to.PI))
sd(na.omit(new.PI.M$Time.to.PI))/sqrt(na.omit(nrow(new.PI.M[!is.na(new.PI.M$Time.to.PI),])))

#Important variables are now time.PI and status.PI

fit_KM <- survfit(Surv(new.PI$time.PI,new.PI$status.PI)~1,type="kaplan-meier",conf.type="log-log")
fit_FH <- survfit(Surv(new.PI$time.PI,new.PI$status.PI)~1,type="fleming-harrington", conf.type="log-log")
fit_FH2 <- survfit(Surv(new.PI$time.PI,new.PI$status.PI)~1, type="fh2", conf.type="log-log")

#plot survival
plot(fit_KM, main="survival function for n.years (K-M estimate)", xlab="years", ylab="probability of being a PI",fun="event")
plot(fit_FH)
plot(fit_FH2)
#K-M is the only plot that seems to look like what we want

#####CHECKING DISTRIBUTION####

#Check which distributional assumption provides the best fit
fit_exp<-survreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="exponential")
fit_weibull<-survreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="weibull")
fit_gauss<-survreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="gaussian")
fit_logistic<-survreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="logistic")
fit_lognormal<-survreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="lognormal")
fit_loglogistic<-survreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="loglogistic")
summary(fit_exp)
summary(fit_weibull)
summary(fit_gauss)
summary(fit_logistic)
summary(fit_lognormal) 
summary(fit_loglogistic)

AIC(fit_exp,fit_weibull,fit_gauss,fit_logistic,fit_lognormal,fit_loglogistic)
#fit_lognormal and fit_loglogistic lowest AICs, best fit

# Package flexsurv provides access to additional distributions
# Also allows plotting options -want red lines to match as close as possible to KM curve
fit_exp<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="exp")
fit_weibull<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="weibull")
fit_gamma<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="gamma")
fit_gengamma<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="gengamma")
fit_genf<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="genf")
fit_lognormal<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~1, dist="lnorm")
fit_gompertz<-flexsurvreg(Surv(new.PI$n.years,new.PI$status.PI)~1, dist="gompertz")
fit_loglogistic<-flexsurvreg(Surv(new.PI$n.years,new.PI$status.PI)~1, dist="llogis")
fit_exp
fit_weibull
fit_gamma
fit_gengamma
fit_genf
fit_lognormal
fit_gompertz
fit_loglogistic
plot(fit_exp)
plot(fit_weibull)
plot(fit_gamma)
plot(fit_gengamma)
plot(fit_genf)
plot(fit_lognormal) 
plot(fit_gompertz) 
plot(fit_loglogistic)

#log likelihood test
fit_exp$loglik
fit_weibull$loglik
fit_gamma$loglik
fit_gengamma$loglik
fit_genf$loglik
fit_lognormal$loglik
fit_gompertz$loglik
fit_loglogistic$loglik

AIC(fit_exp,fit_weibull,fit_gamma,fit_gengamma,fit_genf,fit_lognormal,fit_gompertz,fit_loglogistic)

dist_test<-2*(fit_gompertz$loglik - fit_lognormal$loglik) #2 df
dist_test
#lognormal dist. not significantly different from gompertz

######GENDER#######
library(plotrix)

#Examine gender differences first, ignoring any of the social metrics

#Effect of gender on time to PI (time.PI)

flexAFTgenderPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender,dist="lnorm",data=new.PI)
flexAFTgenderPI

#Summary stats for time to PI
mean(new.PI[new.PI$Gender=="F",]$time.PI) #12.76
std.error(new.PI[new.PI$Gender=="F",]$time.PI) #0.50
mean(new.PI[new.PI$Gender=="M",]$time.PI) #10.48
std.error(new.PI[new.PI$Gender=="M",]$time.PI) #0.27


plot(flexAFTgenderPI,col=c("blue","red"),ci=T,xlab="Years",ylab="Survival probability")
#This doesn't plot what we want, because it gives survival curve, and we want to plot the 
#probability of the event (becoming PI) having occured by time t, against time.

#How to plot what we want:
plotY<-flexsurvreg(Surv(time.PI,status.PI)~Gender,dist="lognormal",data=new.PI) #This gives the KM curve
plot((plotY),ci=FALSE,fun='event',est=F,lty.obs=2,lwd.obs=2,las=1) #,col.obs=c("red","blue")
#This gives two KM curves, one for each gender, and shows them as probability of PI increasing over time
#A bit like an 'inverse' survival curve

#Now we need to overlay the model curves on top, one for each gender
#F
plotF<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lognormal",data=new.PI.F)
plotF
#Estimates: 
#   est     L95%    U95%    se    
#meanlog  2.4068  2.2631  2.5505  0.0733
#sdlog    1.1308  1.0167  1.2576  0.0613
#
#N = 298,  Events: 187,  Censored: 111
#Total time at risk: 3157.5
#Log-likelihood = -708.4906, df = 2
#AIC = 1420.981

t<-1:39 #(Because our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 
#where meanlog and sdlog are the estimates from the fitted model. 
#For other distributions, just use the equivalent cumulative distribution function, such as pgamma, pweibull... 
#This comes from email to Chris Jackson, author of flexsurvreg package
pF<-plnorm(t,plotF$res[1],plotF$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above

lines(pF,col="red",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
#Add CIs
lowerCI_p<-plnorm(t,plotF$res[3],plotF$res[4])
upperCI_p<-plnorm(t,plotF$res[5],plotF$res[5])

lines(lowerCI_p,col="red",lty=2) 
lines(upperCI_p,col="red",lty=2)


#Do the same for M
#M
plotM<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lognormal",data=new.PI.M)
plotM
t<-1:39
pM<-plnorm(t,plotM$res[1],plotM$res[2])
lines(pM,col="blue",lwd=3)

lowerCI_p<-plnorm(t,plotM$res[3],plotM$res[4])
upperCI_p<-plnorm(t,plotM$res[5],plotM$res[6])

lines(lowerCI_p,col="blue",lty=2)
lines(upperCI_p,col="blue",lty=2)


#Another way to check this - rerun AFT model with dist= 'Weibull' or "exponential"
#AFT models with these distributions are special forms, which can be made equivalent to Cox PH models
#Check whether this AFT model gives the same hazard ratios as a Cox PH model.

#Cox PH model
coxph.gender2 = coxph(Surv(time.PI,status.PI) ~Gender,data=new.PI)
summary(coxph.gender2)
#exp(coef) i.e. the HR = 0.6261

#Rerun AFT model with Weibull distribution and survreg rather than flexsurvreg
wei_genderPI<-survreg(Surv(time.PI,status.PI)~Gender,dist="weibull",data=new.PI)
summary(wei_genderPI)

#PH interpretation
#Multiply coefficent by -1, then multiply by 1/scale parameter
exp((0.40*-1)*1/0.814)
#0.61 = this is the hazard ratio comparing gender=F to gender=M.
#This value matches the HR calculated with the Cox PH model - feel confident that our AFT estimate is correct!

#Rerun AFT model with exponetial distribution and survreg rather than flexsurvreg
exp_genderPI<-survreg(Surv(time.PI,status.PI)~Gender,dist="exponential",data=new.PI)
summary(exp_genderPI)

#PH interpretation
#Multiply coefficent by -1, then multiply by 1/scale parameter = 1 for an exponential model
exp((0.4512*-1)*1/1)
#0.64 - again, approximately matches HR value from Cox PH model. Feel confident in the AFT estimate.

#See this link for details of the code above
#https://rstudio-pubs-static.s3.amazonaws.com/5564_bc9e2d9a458c4660aa82882df90b7a6b.html

############
# Now look at effects of social metrics on time to PI (time.PI)

# Start with a simple model first just examining gender and residuals.degree

######ADJUSTED NETWORK SIZE#######

# Start with model that includes an interaction between gender and adjusted network size
# Here we can ask the question whether adjusted network size has different effects on F and M focal authors.
flexAFTPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+residuals.degree+Gender:residuals.degree, dist="lnorm", data=new.PI)
flexAFTPI

# Rerun model without interaction term
flexAFTPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+residuals.degree, dist="lnorm", data=new.PI)
flexAFTPI
# AIC not really different but go with the simpler model for parity.

# Check whether residuals look OK - does the assumed distribution fit the observed data?
#Rerun main model using survreg
new.PI_NA<-subset(new.PI,new.PI$Gender!="NA")
srAFT2PI<-survreg(Surv(time.PI,status.PI)~Gender+residuals.degree+Gender:residuals.degree, dist="lognormal",data=new.PI_NA)

fitted_valuesPI<- srAFT2PI$linear.predictors
residsPI <- (log(srAFT2PI$y[, 1]) - fitted_valuesPI) / srAFT2PI$scale

resKMPI <- survfit(Surv(residsPI, status.PI) ~ 1, data = new.PI_NA)
plot(resKMPI, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(residsPI), max(residsPI), length.out = 35)
yy <- pnorm(xx, lower.tail = FALSE)
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

# Fit of curves look good - happy with this.
# See http://www.drizopoulos.com/courses/emc/ep03_%20survival%20analysis%20in%20r%20companion#accelerated-failure-time-models

######ADJUSTED TIE STRENGTH#######
#Go straight to interaction model
flexAFT_consistPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+residuals.strength+Gender:residuals.strength, dist="lnorm", data=new.PI)
flexAFT_consistPI
#Interaction of gender with tie strength is significant (CIs don't overlap 1) 

#Gender-specific models:
#F
flexAFT_F_consistPI<-flexsurvreg(Surv(new.PI.F$time.PI,new.PI.F$status.PI)~residuals.strength, dist="lnorm", data=new.PI.F)
flexAFT_F_consistPI

#M
flexAFT_M_consistPI<-flexsurvreg(Surv(new.PI.M$time.PI,new.PI.M$status.PI)~residuals.strength, dist="lnorm", data=new.PI.M)
flexAFT_M_consistPI

#Check whether residuals look OK - does the assumed distribution fit the observed data?
#rerun main model using survreg
#NA values in dataset, so first create new.PI dataset with NA values removed from Gender
new.PI_NA<-subset(new.PI,new.PI$Gender!="NA")
AFT_consistPI<-survreg(Surv(new.PI_NA$time.PI,new.PI_NA$status.PI)~Gender+residuals.strength+Gender:residuals.strength, dist="lognormal", data=new.PI_NA)

fitted_valuesPI<- AFT_consistPI$linear.predictors
residsPI <- (log(AFT_consistPI$y[, 1]) - fitted_valuesPI) / AFT_consistPI$scale

resKMPI <- survfit(Surv(residsPI, status.PI) ~ 1, data = new.PI_NA)
plot(resKMPI, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(residsPI), max(residsPI), length.out = 35)
yy <- pnorm(xx, lower.tail = FALSE)
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

#Fit looks OK
#Distribution fits well to data


######ADJUSTED CLUSTERING COEFFICIENT#######
#Go straight to interaction model
flexAFT_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender+residuals.cluster+Gender:residuals.cluster, dist="lnorm", data=new.PI)
flexAFT_connectPI
#AIC term barely different, but for parity, keep interaction term out of model.
flexAFT_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender+residuals.cluster, dist="lnorm", data=new.PI)
flexAFT_connectPI
###

#Run gender-specific models anyway, just to check:
#F
flexAFT_F_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~residuals.cluster, dist="lnorm", data=new.PI.F)
flexAFT_F_connectPI

#M
flexAFT_M_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~residuals.cluster, dist="lnorm", data=new.PI.M)
flexAFT_M_connectPI
#Both gender-specific models confirm result of main model: negative effect of adjusted clustering coefficient
#on time to PI. Having co-authors who are more connected is not a good thing for time to PI.


#Check whether residuals look OK - does the assumed distribution fit the observed data?
#rerun main model using survreg - use model without interaction term
new.PI_NA<-subset(new.PI,new.PI$Gender!="NA")
new.PI_NA_connect<-subset(new.PI_NA,new.PI_NA$residuals.cluster!="NA")
AFT_connectPI<-survreg(Surv(time.PI,status.PI)~Gender+residuals.cluster, dist="lognormal", data=new.PI_NA_connect)

fitted_valuesPI<- AFT_connectPI$linear.predictors
residsPI <- (log(AFT_connectPI$y[, 1]) - fitted_valuesPI) / AFT_connectPI$scale

resKMPI <- survfit(Surv(residsPI, status.PI) ~ 1, data = new.PI_NA_connect)
plot(resKMPI, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(residsPI), max(residsPI), length.out = 35)
yy <- pnorm(xx, lower.tail = FALSE)
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

##Distribution fits well to data

########
###5.2 CAREER LENGTH (n.years)
########

### 1. Decide on survival curve

# survfit fits survival curves with various methods
# Kaplan-Meier is most common, so use that if in doubt

fit_KM <- survfit(Surv(new$n.years,new$status)~1,type="kaplan-meier",conf.type="log-log")
fit_FH <- survfit(Surv(new$n.years,new$status)~1,type="fleming-harrington", conf.type="log-log")
fit_FH2 <- survfit(Surv(new$n.years,new$status)~1, type="fh2", conf.type="log-log")
#See https://stats.stackexchange.com/questions/361354/choosing-conf-type-for-survfit-in-r
#Seems like log-log may be as good as anything.

# Plot survival
plot(fit_KM, main="survival function for n.years (K-M estimate)", xlab="years", ylab="probability of being in science")
plot(fit_FH)
plot(fit_FH2)
# Basically the curve does not change at all - fine to use K-M

### CHECKING DISTRIBUTION

# Check which distributional assumption provides the best fit
fit_exp<-survreg(Surv(new$n.years,new$status)~1, dist="exponential")
fit_weibull<-survreg(Surv(new$n.years,new$status)~1, dist="weibull")
fit_gauss<-survreg(Surv(new$n.years,new$status)~1, dist="gaussian")
fit_logistic<-survreg(Surv(new$n.years,new$status)~1, dist="logistic")
fit_lognormal<-survreg(Surv(new$n.years,new$status)~1, dist="lognormal")
fit_loglogistic<-survreg(Surv(new$n.years,new$status)~1, dist="loglogistic")
summary(fit_exp)
summary(fit_weibull)
summary(fit_gauss)
summary(fit_logistic)
summary(fit_lognormal) #Best fit
summary(fit_loglogistic)

AIC(fit_exp,fit_weibull,fit_gauss,fit_logistic,fit_lognormal,fit_loglogistic)
#fit_lognormal    Lowest AIC, best fit

# Package flexsurv provides access to additional distributions
# Also allows plotting options -want red lines to match as close as possible to KM curve

fit_exp<-flexsurvreg(Surv(new$n.years,new$status)~1, dist="exp")
fit_weibull<-flexsurvreg(Surv(new$n.years,new$status)~1, dist="weibull")
fit_gamma<-flexsurvreg(Surv(new$n.years,new$status)~1, dist="gamma")
fit_gengamma<-flexsurvreg(Surv(new$n.years,new$status)~1, dist="gengamma")
fit_genf<-flexsurvreg(Surv(new$n.years,new$status)~1, dist="genf")
fit_lognormal<-flexsurvreg(Surv(new$n.years,new$status)~1, dist="lnorm")
fit_gompertz<-flexsurvreg(Surv(new$n.years,new$status)~1, dist="gompertz")
fit_exp
fit_weibull
fit_gamma
fit_gengamma
fit_genf
fit_lognormal
fit_gompertz
plot(fit_exp)
plot(fit_weibull)
plot(fit_gamma)
plot(fit_gengamma)
plot(fit_genf)
plot(fit_lognormal)
plot(fit_gompertz)

# log likelihood test
fit_exp$loglik
fit_weibull$loglik
fit_gamma$loglik
fit_gengamma$loglik
fit_genf$loglik
fit_lognormal$loglik
fit_gompertz$loglik
# gengamma and genf distributions have loglikelihood values closest to zero, but lognormal is also close

AIC(fit_exp,fit_weibull,fit_gamma,fit_gengamma,fit_genf,fit_lognormal,fit_gompertz)

dist_test<-2*(fit_gengamma$loglik - fit_lognormal$loglik) #2 df
dist_test #2.96
# Generalized gamma distribution is not sig. better fit than lognormal distribution. Justifies using lognormal.

### AFT model

# AFT model uses survreg function from Survival package
# using lognormal distribution according to earlier exploration of the best distribution for the data
# use survreg for stepwise
# then flexsurvreg to generate graphs

### GENDER
## Examine gender differences first, ignoring any of the social metrics

#Effect of gender on career length (n.years)

flexAFTgender<-flexsurvreg(Surv(n.years,status)~Gender,dist="lnorm",data=new)
flexAFTgender

plot(flexAFTgender,col=c("blue","red"),ci=T,xlab="Years",ylab="Survival probability")

#Summary stats for career length
mean(new[new$Gender=="F",]$n.years) #22.16
std.error(new[new$Gender=="F",]$n.years) #0.55
mean(new[new$Gender=="M",]$n.years) #25.90
std.error(new[new$Gender=="M",]$n.years) #0.34

# Another way to check this - rerun AFT model with dist= 'Weibull' or "exponential"
# AFT models with these distributions are special forms, which can be made equivalent to Cox PH models
# Check whether this AFT model gives the same hazard ratios as if using a Cox PH model.
# Cox proportional hazards model is another type of commonly used survival analysis.

# Cox PH model
# coxph is from the Survival package
coxph.baseline.gender = coxph(Surv(new$n.years,new$status)~Gender,data=new)
summary(coxph.baseline.gender)
# exp(coef) i.e. the HR = 1.854

#Rerun AFT model with Weibull distribution and survreg rather than flexsurvreg
wei_gender<-survreg(Surv(n.years,status)~Gender,dist="weibull",data=new)
summary(wei_gender)
# AFT interpretation
# Having gender=F accelerates the time to event (leaving science) by a factor of exp(-0.4558) 
# = 0.63 (63% shorter survival compared to gender=M)

# PH interpretation
# Multiply coefficent by -1, then multiply by 1/scale parameter
exp((-0.4558*-1)*1/0.72)
# 1.88 = this is the hazard ratio comparing gender=F to gender=M.
# This value ~matches the HR calculated with the Cox PH model (1.854) - feel confident that our AFT estimate is correct!

# Rerun AFT model with exponetial distribution and survreg rather than flexsurvreg
exp_gender<-survreg(Surv(n.years,status)~Gender,dist="exponential",data=new)
summary(exp_gender)
# AFT interpretation
# Having gender=F accelerates the time to event (leaving science) by a factor of exp-(0.5919) 
# = 0.55 (55% shorter survival compared to gender=M)

# PH interpretation
# Multiply coefficent by -1, then multiply by 1/scale parameter = 1 for an exponential model
exp((-0.5919*-1)*1/1)
# 1.81 - again, approximately matches HR value from Cox PH model. Feel confident in the AFT estimate.

# See this link for details of the code above:https://rstudio-pubs-static.s3.amazonaws.com/5564_bc9e2d9a458c4660aa82882df90b7a6b.html


############
# Now look at effects of social metrics on career length (n.years)

# Start with a simple model first just examining gender and residuals.degree (i.e. adjusted network size)

######ADJUSTED NETWORK SIZE#######

aftmodel1<-survreg(Surv(new$n.years,new$status)~Gender+residuals.degree, dist="lognormal", data=new)
aftmodel1
summary(aftmodel1) #provides a bit more information than just calling the model

exp(aftmodel1$coefficients) # Allows you to interpret the coefficients in a more informative way
# Interpretation e.g. Females, on average, survive for only 79% of the time that males do. Females have career
#lengths 21% shorter than males.

# Run the same model but this time using flexsurvreg
flexAFT1<-flexsurvreg(Surv(new$n.years,new$status)~Gender+residuals.degree, dist="lnorm", data=new)
# NOTE that distribution is defined slightly differently
flexAFT1
# Est is the same as for survreg model, but results displayed slightly differently.
# Look at exp(est) column for interpretation
# Should produce the same results as for survreg model.

# To plot this model:
flexAFTplot<-flexsurvreg(Surv(n.years,status)~1,dist="lnorm",data=new)
plot(flexAFTplot, type="survival",ci=TRUE)
# Black lines show KM survival curve with CIs. Red line shows lognormal model, with CIs - both overlap well, implying good model fit
# (and see code above for where we determine best distribution to describe our survival data)
flexAFTgenderplot<-flexsurvreg(Surv(new$n.years,new$status)~Gender,dist="lnorm",data=new)
plot(flexAFTgenderplot, type="survival",ci=TRUE)
# Now we see the survival for each gender separately.

# Extend the model to include an interaction between gender and adjusted network size
# Here we can ask the question whether adjusted network size has different effects on F and M focal authors.
flexAFT2<-flexsurvreg(Surv(new$n.years,new$status)~Gender+residuals.degree+Gender:residuals.degree, dist="lnorm", data=new)
flexAFT2

# Since the model shows an interaction with Gender, run the model again, but this time for each sex separately.
# First split dataset by gender
new.F = new[new$Gender=="F",] 
new.F = new.F[(!is.na(new.F$Gender)),] 
new.F$Gender = factor(new.F$Gender)#298

new.M = new[new$Gender=="M",] 
new.M = new.M[(!is.na(new.M$Gender)),]
new.M$Gender = factor(new.M$Gender) #637

# F
flexAFT_F_collab<-flexsurvreg(Surv(new.F$n.years,new.F$status)~residuals.degree, dist="lnorm", data=new.F)
flexAFT_F_collab

# M
flexAFT_M_collab<-flexsurvreg(Surv(new.M$n.years,new.M$status)~residuals.degree, dist="lnorm", data=new.M)
flexAFT_M_collab

# Check whether residuals look OK - does the assumed distribution fit the observed data?
# Rerun main model using survreg
srAFT2<-survreg(Surv(n.years,status)~Gender+residuals.degree+Gender:residuals.degree, dist="lognormal",data=new)
#NA values in dataset, so first create new dataset with NA values removed from Gender
new_NA<-subset(new,new$Gender!="NA")
srAFT2<-survreg(Surv(n.years,status)~Gender+residuals.degree+Gender:residuals.degree, dist="lognormal",data=new_NA)

fitted_values<- srAFT2$linear.predictors
resids <- (log(srAFT2$y[, 1]) - fitted_values) / srAFT2$scale

resKM <- survfit(Surv(resids, status) ~ 1, data = new_NA)
plot(resKM, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(resids), max(resids), length.out = 35)
yy <- pnorm(xx, lower.tail = FALSE)
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

# Repeat with dist set to logistic, since we expect this to be a much worse fit (see above 
# where we modelled distributions of the data and looked at AIC and log-likelihood values)

srAFT3<-survreg(Surv(n.years,status)~Gender+residuals.degree+Gender:residuals.degree, dist="logistic",data=new_NA)

fitted_values<- srAFT3$linear.predictors
resids <- (log(srAFT3$y[, 1]) - fitted_values) / srAFT3$scale

resKM <- survfit(Surv(resids, status) ~ 1, data = new_NA)
plot(resKM, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(resids), max(resids), length.out = 35)
yy <- plogis(xx, lower.tail = FALSE) #Note the change in code here
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

# Residual fit is significantly worse - no real overlapping of lines.
# Suggests that the approach does work
# See http://www.drizopoulos.com/courses/emc/ep03_%20survival%20analysis%20in%20r%20companion#accelerated-failure-time-models

######ADJUSTED TIE STRENGTH#######
# Go straight to interaction model
flexAFT_consist<-flexsurvreg(Surv(new$n.years,new$status)~Gender+residuals.strength+Gender:residuals.strength, dist="lnorm", data=new)
flexAFT_consist
#model without interaction
flexAFT_consist<-flexsurvreg(Surv(new$n.years,new$status)~Gender+residuals.strength, dist="lnorm", data=new)
flexAFT_consist

flexAFTgenderplot_consist<-flexsurvreg(Surv(new$n.years,new$status)~Gender,dist="lnorm",data=new)
plot(flexAFTgenderplot_consist, type="survival",ci=TRUE)

# Gender-specific models:
# F
flexAFT_F_consist<-flexsurvreg(Surv(new.F$n.years,new.F$status)~residuals.strength, dist="lnorm", data=new.F)
flexAFT_F_consist

# M
flexAFT_M_consist<-flexsurvreg(Surv(new.M$n.years,new.M$status)~residuals.strength, dist="lnorm", data=new.M)
flexAFT_M_consist

# Compare to survreg model
AFT_consist<-survreg(Surv(new$n.years,new$status)~Gender+residuals.strength+Gender:residuals.strength, dist="lognormal", data=new)
AFT_consist
summary(AFT_consist)
exp(AFT_consist$coefficients)

# Check whether residuals look OK - does the assumed distribution fit the observed data?
# Rerun main model using survreg
# srAFT2<-survreg(Surv(n.years,status)~Gender+residuals.degree+Gender:residuals.degree, dist="lognormal",data=new)
# NA values in dataset, so first create new dataset with NA values removed from Gender
new_NA<-subset(new,new$Gender!="NA")
AFT_consist<-survreg(Surv(new_NA$n.years,new_NA$status)~Gender+residuals.strength+Gender:residuals.strength, dist="lognormal", data=new_NA)

fitted_values<- AFT_consist$linear.predictors
resids <- (log(AFT_consist$y[, 1]) - fitted_values) / AFT_consist$scale

resKM <- survfit(Surv(resids, status) ~ 1, data = new_NA)
plot(resKM, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(resids), max(resids), length.out = 35)
yy <- pnorm(xx, lower.tail = FALSE)
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

# Repeat with dist set to logistic, since we expect this to be a much worse fit (see above 
# where we modelled distributions of the data and looked at AIC and log-likelihood values)

AFT_consist1<-survreg(Surv(n.years,status)~Gender+residuals.degree+Gender:residuals.degree, dist="logistic",data=new_NA)

fitted_values<- AFT_consist1$linear.predictors
resids <- (log(AFT_consist1$y[, 1]) - fitted_values) / AFT_consist1$scale

resKM <- survfit(Surv(resids, status) ~ 1, data = new_NA)
plot(resKM, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(resids), max(resids), length.out = 35)
yy <- plogis(xx, lower.tail = FALSE) #Note the change in code here
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

# Distribution fits well to data

######ADJUSTED CLUSTER COEFFICIENT#######
# Go straight to interaction model
flexAFT_connect<-flexsurvreg(Surv(n.years,status)~Gender+residuals.cluster+Gender:residuals.cluster, dist="lnorm", data=new)
flexAFT_connect
# CIs for interaction term overlaps 1. Therefore remove this term.

flexAFT_connect1<-flexsurvreg(Surv(n.years,status)~Gender+residuals.cluster, dist="lnorm", data=new)
flexAFT_connect1
# AIC term barely different, but for parity, keep interaction term out of model

# Rerunning models with interaction term and without interaction term, but using survreg, and then comparing 
# with anova(simpler model, interaction model) gives ns result, suggesting that we can use simpler model because more complex
# one is not significantly better at describing data.

# Gender-specific models:
# F
flexAFT_F_connect<-flexsurvreg(Surv(n.years,status)~residuals.cluster, dist="lnorm", data=new.F)
flexAFT_F_connect

# M
flexAFT_M_connect<-flexsurvreg(Surv(n.years,status)~residuals.cluster, dist="lnorm", data=new.M)
flexAFT_M_connect

# Check whether residuals look OK - does the assumed distribution fit the observed data?
# rerun main model using survreg - use model without interaction term
new_NA<-subset(new,new$Gender!="NA")
# residuals.cluster contains NA values, so remove these too.
new_NA_connect<-subset(new_NA,new_NA$residuals.cluster!="NA")
AFT_connect<-survreg(Surv(n.years,status)~Gender+residuals.cluster, dist="lognormal", data=new_NA_connect)

fitted_values<- AFT_connect$linear.predictors
resids <- (log(AFT_connect$y[, 1]) - fitted_values) / AFT_connect$scale

resKM <- survfit(Surv(resids, status) ~ 1, data = new_NA_connect)
plot(resKM, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(resids), max(resids), length.out = 35)
yy <- pnorm(xx, lower.tail = FALSE)
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

# Repeat with dist set to logistic, since we expect this to be a much worse fit (see above here wer modelled distributions of the data and looked at AIC and log-likelihood values)

AFT_connect1<-survreg(Surv(n.years,status)~Gender+residuals.degree, dist="logistic",data=new_NA_connect)

fitted_values<- AFT_connect1$linear.predictors
resids <- (log(AFT_connect1$y[, 1]) - fitted_values) / AFT_connect1$scale

resKM <- survfit(Surv(resids, status) ~ 1, data = new_NA_connect)
plot(resKM, mark.time = FALSE, xlab = "AFT Residuals", ylab = "Survival Probability")
xx <- seq(min(resids), max(resids), length.out = 35)
yy <- plogis(xx, lower.tail = FALSE) #Note the change in code here
lines(xx, yy, col = "red", lwd = 2)
legend("bottomleft", c("KM estimate", "95% CI KM estimate", 
                       "Survival function of Extreme Value distribution"), 
       lty = c(1,2,1), col = c(1,1,2), bty = "n")

# Distribution fits well to data


############################################################################################
###			(6) Figure plotting
############################################################################################
library(graphics) 
library(flexsurv) 
library(survminer)
library(grid)
library(gridBase)


### FIGURE 1

new$Gender = factor(new$Gender, levels = c("F", "M"))
new.PI$Gender = factor(new.PI$Gender, levels = c("F", "M"))

#Change order of genders to get F first and then M in plots

# Plots to show effect of gender on time to PI(Fig. 1A), career longevity(Fig. 1B), 

# Two panels
# Plot layout

layout.matrix<-matrix(c(5,3,1,4,2,6),ncol=3,nrow=2)

layout(mat = layout.matrix,
       heights = 1.4, # Heights of the three rows
       widths = c(4,4,4)) # Widths of the three columns

layout.show(6) # Shows the layout of the plots (2 panels)

# Now fill in the individual plots

#Fig 1A
table(new$status.PI, new$Gender)
df <- data.frame("PI_status" = c("PI", "not a PI","PI", "not a PI"), "Gender" = c("F","M","M","F"), "Percentage" = c(187,116,521,111))
A = ggplot(df,aes(x = as.factor(Gender), y = Percentage ))+geom_bar(aes(fill = as.factor(PI_status)),stat='identity', position ="stack")+
   theme_bw()+ labs(tag = "A")+
   ylab("Count") + ylab("Number of focal authors")+
   theme(axis.title.x = element_blank(), # Remove y axis title
         axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
         axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
         axis.title.y = element_text(size=12,vjust=3,colour="black"), # Change size and font of x axis title and move it down a bit
         panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
         panel.grid.minor = element_blank(), # Formatting to create blank plot with box around it
         legend.position = c(0.3, 0.7),
         legend.text=element_text(size=12),
         legend.background = element_rect(fill=alpha('transparent')),
         legend.title = element_blank()) +
   scale_fill_manual("legend", values = c("PI" = "black", "not a PI" = "grey"))

# FIG 1B: Time to PI - 
# This is the model to plot
flexAFTgenderPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender,dist="lnorm",data=new.PI) 

# To plot what we want requires a bit more work...
plotY<-flexsurvreg(Surv(time.PI,status.PI)~Gender,dist="lnorm",data=new.PI) #This gives the KM curve

plot((plotY),ci=FALSE,fun='event',est=FALSE,lty.obs=2,lwd.obs=3,las=1,
     xlab="Time from first publication year", ylab="Probability of being a PI",
     cex.lab=1.5,cex.axis=1.4,xlim=c(0,40),ylim=c(0,1.0),mgp=c(2.7,1,0))
box(lwd=1.5)
#text(1,0.98,"B",cex=1.5)
legend("bottomright",legend=c("F","M"), 
       col=c("darkorange","darkslateblue"),
       lty=1.1,lwd=3,cex=1,
       bg="transparent",
       text.font=1,
       box.lty=0,seg.len=0.5)

# This gives two KM curves, one for each gender, and shows them as probability of PI increasing over time
# A bit like an 'inverse' survival curve

# Now we need to overlay the model curves on top, one for each gender
#Female
plotF<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.F)
plotF


t<-1:39 #(Because our dataset covers 39 years max)

pF<-plnorm(t,plotF$res[1,1],plotF$res[2,1]) #takes the est values for meanlog and sdlog from the plotF model
lines(pF,col="darkorange",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)

#Add CIs
lowerCI_p<-plnorm(t,plotF$res[1,2],plotF$res[2,2]) #takes the lower CIs from the plotF model
upperCI_p<-plnorm(t,plotF$res[1,3],plotF$res[2,3]) #takes the upper CIs from the plotF model

lines(lowerCI_p,col="darkorange",lty=2) 
lines(upperCI_p,col="darkorange",lty=2)

#Male
plotM<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lognormal",data=new.PI.M)
plotM
t<-1:39
pM<-plnorm(t,plotM$res[1,1],plotM$res[2,1])
lines(pM,col="darkslateblue",lwd=3)

lowerCI_p<-plnorm(t,plotM$res[1,2],plotM$res[2,2])
upperCI_p<-plnorm(t,plotM$res[1,3],plotM$res[2,3])

lines(lowerCI_p,col="darkslateblue",lty=2)
lines(upperCI_p,col="darkslateblue",lty=2)

B <- recordPlot()  
plot.new() 
B
#FIG 1B: Career longevity 
#This is the model to plot
flexAFTgender<-flexsurvreg(Surv(n.years,status)~Gender,dist="lnorm",data=new) #See line 2997

plot(flexAFTgender,type="survival",ci=TRUE,lty.obs=2,lwd.obs=3,cl=0.95,las=1,
     col=c("darkslateblue","darkorange"),xlab="Career length (no. of years publishing)", ylab="Survival probability",
     cex.lab=1.5,cex.axis=1.4,mgp=c(2.7,1,0))
box(lwd=1.5)
#text(38,0.98,"C",cex=1.5)

C <- recordPlot()  
plot.new() 
C

#Fig1D
D = ggplot(data=new, aes(x=Gender, y=residuals.degree, colour=Gender))+geom_violin(trim=FALSE)+
   geom_jitter(position=position_jitter(0.4), shape=16, size = 0.5)+labs(tag = "D")+
   stat_summary(fun = "mean", geom = "point", pch = "_", size = 10, colour = "black")+
   theme_bw()+ylab("Adjusted Network Size")+xlab("")+
   theme(panel.grid.major = element_blank(),
         legend.position="none" ,legend.title=element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"))+
   theme(axis.title.y = element_text( size=12, colour="black"),
         axis.title.x = element_text( size=12, colour="black"),
         axis.text.x  = element_text(size=12, colour="black"),
         axis.text.y= element_text(size=12, colour="black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"),
         panel.border = element_rect(colour = "black"))+
   scale_color_manual(values=c("F"="darkorange", "M" = "darkslateblue"))+
   geom_signif(comparisons = list(c("F", "M")),  colour = "black",
               map_signif_level=TRUE,  textsize = 5)
#Fig1E
E = ggplot(data=new, aes(x=Gender, y=residuals.strength, colour=Gender))+geom_violin(trim=FALSE)+labs(tag = "E")+
   geom_jitter(position=position_jitter(0.4), shape=16, size = 0.5)+
   stat_summary(fun = "mean", geom = "point", pch = "_", size = 10, colour = "black")+
   theme_bw()+ylab("Adjusted Tie Strength")+xlab("")+
   theme(panel.grid.major = element_blank(),
         legend.position="none" ,legend.title=element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"))+
   theme(axis.title.y = element_text( size=12, colour="black"),
         axis.title.x = element_text( size=12, colour="black"),
         axis.text.x  = element_text(size=12, colour="black"),
         axis.text.y= element_text(size=12, colour="black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"),
         panel.border = element_rect(colour = "black"))+
   scale_color_manual(values=c("F"="darkorange", "M" = "darkslateblue"))+
   geom_signif(comparisons = list(c("F", "M")),  colour = "black",
               map_signif_level=TRUE, textsize = 5)
#Fig1F
F = ggplot(data=new, aes(x=Gender, y=residuals.cluster, colour=Gender))+geom_violin(trim=FALSE)+labs(tag = "F")+
   geom_jitter(position=position_jitter(0.4), shape=16, size = 0.5)+
   stat_summary(fun = "mean", geom = "point", pch = "_", size = 10, colour = "black")+
   theme_bw()+ylab("Adjusted Clustering Coeff.")+xlab("")+
   theme(panel.grid.major = element_blank(),
         legend.position="none" ,legend.title=element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"))+
   theme(axis.title.y = element_text( size=12, colour="black"),
         axis.title.x = element_text( size=12, colour="black"),
         axis.text.x  = element_text(size=12, colour="black"),
         axis.text.y= element_text(size=12, colour="black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_rect(colour="black", fill="white"),
         panel.border = element_rect(colour = "black"))+
   scale_color_manual(values=c("F"="darkorange", "M" = "darkslateblue"))+
   geom_signif(comparisons = list(c("F", "M")), colour = "black",
               map_signif_level=TRUE, textsize = 5)+ylim(-5.5,5.0)

# Move to a new page
grid.newpage()
# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 3)))
# A helper function to define a region on the layout
define_region <- function(row, col){
   viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Arrange the plots
print(D, vp = define_region(row = 2, col = 1))   # Span over two columns
print(E, vp = define_region(row = 2, col = 2))   # Span over two columns
print(F, vp = define_region(row = 2, col = 3))   # Span over two columns
print(A, vp = define_region(row = 1, col = 1))

print(B, vp = define_region(row = 1, col = 1))
print(C, vp = define_region(row = 1, col = 1))



#########################

## FIGURE 2

#Plot to show relative effect sizes of each collaboration behaviour metric on:
#PI status
#Time to PI
#Career length

library(ggpubr)


#List models that I want to extract values (estimate of effect size and CIs) from:

###PI STATUS (status.PI)
########
#Effect of adjusted network size (residuals.degree) on PI status)
m1<-glm(as.factor(status.PI)~Gender:residuals.degree,family="binomial",data=new)
m1summ<-summ(m1,
             scale = FALSE, #if true, than calculates standardized regression coefficients
             vifs = getOption("summ-vifs", FALSE),#variance inflation factor:#1 = not correlated; Between 1 and 5 = moderately correlated; Greater than 5 = highly correlated.
             confint = getOption("summ-confint", TRUE),
             digits = getOption("jtools-digits", default = 3),
             exp = TRUE) # if exp = TRUE, then "reports exponentiated coefficients with confidence intervals for expo-nential models like logit and Poisson models. This quantity is known as an oddsratio for binary outcomes and incidence rate ratio for count models" that's cool
#F and M estimates
m1summ$coef[2] #extracts value for exp(Est.) from summ table m1summ - gives F estimate
m1summ$coef[3] #gives M estimate

#lower CIs
m1summ$coef[5] #F
m1summ$coef[6] #M

#upper CIs
m1summ$coef[8]
m1summ$coef[9]

#etc... for adjusted tie strength (residuals.strength) and adjusted clustering coefficient (residuals.cluster)

#Effect of adjusted tie strength (residuals.strength) on PI status)
m2<-glm(as.factor(status.PI)~Gender:residuals.strength,family="binomial",data=new)
m2summ<-summ(m2,
             scale = FALSE, #if true, than calculates standardized regression coefficients
             vifs = getOption("summ-vifs", FALSE),#variance inflation factor:#1 = not correlated; Between 1 and 5 = moderately correlated; Greater than 5 = highly correlated.
             confint = getOption("summ-confint", TRUE),
             digits = getOption("jtools-digits", default = 3),
             exp = TRUE)

#Effect of adjusted clustering coefficient (residuals.cluster) on PI status)
m3<-glm(as.factor(status.PI)~Gender:residuals.cluster,family="binomial",data=new)
m3summ<-summ(m3,
             scale = FALSE, #if true, than calculates standardized regression coefficients
             vifs = getOption("summ-vifs", FALSE),#variance inflation factor:#1 = not correlated; Between 1 and 5 = moderately correlated; Greater than 5 = highly correlated.
             confint = getOption("summ-confint", TRUE),
             digits = getOption("jtools-digits", default = 3),
             exp = TRUE)

###TIME TO PI (time.PI)
########

#Female focal authors dataset
new.PI.F = new.PI[new.PI$Gender=="F",] 
new.PI.F = new.PI.F[(!is.na(new.PI.F$Gender)),] 
new.PI.F$Gender = factor(new.PI.F$Gender)#298

#Male focal authors dataset
new.PI.M = new.PI[new.PI$Gender=="M",] 
new.PI.M = new.PI.M[(!is.na(new.PI.M$Gender)),]
new.PI.M$Gender = factor(new.PI.M$Gender) #637

#Gender 
flexgender<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender, dist="lnorm", data=new.PI)
flexgender

##Adjusted network size
flexAFTPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+residuals.degree, dist="lnorm", data=new.PI)
flexAFTPI
#F
m4F<-flexsurvreg(Surv(time.PI,status.PI)~residuals.degree, dist="lnorm", data=new.PI.F)
m4F
#To extract relevant values:
exp(m4F$res.t[3]) #gives exp(est) i.e. deceleration factor
exp(m4F$res.t[6]) #gives lower CI
exp(m4F$res.t[9]) #gives upper CI
#M
m4M<-flexsurvreg(Surv(time.PI,status.PI)~residuals.degree, dist="lnorm", data=new.PI.M)
m4M

## Adjusted tie strength
flexAFT_consistPI<-flexsurvreg(Surv(new.PI$time.PI,new.PI$status.PI)~Gender+residuals.strength, dist="lnorm", data=new.PI)
flexAFT_consistPI
#F
m5F<-flexsurvreg(Surv(new.PI.F$time.PI,new.PI.F$status.PI)~residuals.strength, dist="lnorm", data=new.PI.F)
m5F
#M
m5M<-flexsurvreg(Surv(new.PI.M$time.PI,new.PI.M$status.PI)~residuals.strength, dist="lnorm", data=new.PI.M)
m5M

## Adjusted clustering coefficient
flexAFT_connectPI<-flexsurvreg(Surv(time.PI,status.PI)~Gender+residuals.cluster, dist="lnorm", data=new.PI)
flexAFT_connectPI
#F
m6F<-flexsurvreg(Surv(time.PI,status.PI)~residuals.cluster, dist="lnorm", data=new.PI.F)
m6F
#M
m6M<-flexsurvreg(Surv(time.PI,status.PI)~residuals.cluster, dist="lnorm", data=new.PI.M)
m6M


###CAREER LENGTH (n.years)
########

#Gender 
flexgender<-flexsurvreg(Surv(new$n.years,new$status)~Gender, dist="lnorm", data=new)
flexgender

## Adjusted network size
flexAFT2<-flexsurvreg(Surv(new$n.years,new$status)~Gender*residuals.degree, dist="lnorm", data=new)
flexAFT2
# F
m7F<-flexsurvreg(Surv(new.F$n.years,new.F$status)~residuals.degree, dist="lnorm", data=new.F)
m7F
# M
m7M<-flexsurvreg(Surv(new.M$n.years,new.M$status)~residuals.degree, dist="lnorm", data=new.M)
m7M

## Adjusted tie strength
AFT_consist<-flexsurvreg(Surv(new$n.years,new$status)~Gender+residuals.strength, dist="lognormal", data=new)
AFT_consist
# F
m8F<-flexsurvreg(Surv(new.F$n.years,new.F$status)~residuals.strength, dist="lnorm", data=new.F)
m8F
# M
m8M<-flexsurvreg(Surv(new.M$n.years,new.M$status)~residuals.strength, dist="lnorm", data=new.M)
m8M

## Adjusted clustering coefficient
flexAFT_connect<-flexsurvreg(Surv(n.years,status)~Gender+residuals.cluster, dist="lnorm", data=new)
flexAFT_connect
# F
m9F<-flexsurvreg(Surv(n.years,status)~residuals.cluster, dist="lnorm", data=new.F)
m9F
# M 
m9M<-flexsurvreg(Surv(n.years,status)~residuals.cluster, dist="lnorm", data=new.M)
m9M

#First make the plots, and then arrange them as I want them.
#Forest plot style to show how effect sizes vary.
#First plot effects of collaboration behaviours on "Being a PI" - here effect size is odds ratio.
#Then plot "Time to PI" and 'Career Length" in the same area as they have the same type of effect size based on
#the AFT models.
#Use Forest plot style

#PI Status
#Set up necessary data for plotting
labeltext<-c(1:6)

#These values all come from the estimates (and lower and upper CIs) for the various models above
#and are in the following order:

#Effects of adj. network size, adj. tie strength, and adj. clustering coefficient always F first followed by M (for each C in turn)

Gender<-c("F","M","F","M","F","M")
estimate<-c(m1summ$coef[2],
            m1summ$coef[3],
            m2summ$coef[2],
            m2summ$coef[3],
            m3summ$coef[2],
            m3summ$coef[3])

lower<-c(m1summ$coef[5],
         m1summ$coef[6],
         m2summ$coef[5],
         m2summ$coef[6],
         m3summ$coef[5],
         m3summ$coef[6])

upper<-c(m1summ$coef[8],
         m1summ$coef[9],
         m2summ$coef[8],
         m2summ$coef[9],
         m3summ$coef[8],
         m3summ$coef[9])        

df<- data.frame(labeltext, estimate, lower, upper, Gender)

## Reverses the factor level ordering for labels after coord_flip()
df$labeltext<-factor(df$labeltext, levels=rev(df$labeltext))

df$colour <- c("dark grey","dark grey","black","black","dark grey","dark grey")
#Where there is a significant gender difference within a collaboration behaviour metric then plot in black.
#Else plot in grey.

plotPIstatus<-
   ggplot(data=df, aes(x=labeltext, y=estimate, ymin=lower, ymax=upper, shape=Gender, col=Gender))+
   annotate("rect", xmin = 0, xmax = 6.7, ymin = 1, ymax = 15, alpha = .2, fill = "grey") +
   annotate("rect", xmin = 0, xmax = 6.7, ymin = 0, ymax = 1, alpha = .2, fill = "white") +
      expand_limits(x = 0)+
   #fills the area of the plot where there is a beneficial effect of the collaboration behaviour on the career progression measure.
   geom_point(aes(size=2,col = colour, fill = colour),show.legend=FALSE) +
   geom_pointrange(data = df,size=0.7, col = df$colour, fill = df$colour) +
   geom_hline(yintercept=1, lty=2) +  # Add a dotted line at x=1 after flip
   geom_errorbar(aes(ymin=lower, ymax=upper, width=0.6,cex=1,col=colour),show.legend=FALSE) +
   scale_x_discrete(breaks=c(1,3,5),
                    labels=c("Adj. Network Size","Adj. Tie Strength","Adj. Clustering Coef.")) +
   coord_flip() +  # Flip coordinates (puts labels on y axis)
   xlab("Label") + ylab("Odds ratio") +
   scale_fill_manual(values=c("dark grey","dark grey","black","black","dark grey","dark grey"))+
   scale_colour_manual(values=c("black","dark grey","dark grey","dark grey","black","black")) + 
   scale_shape_manual(values=c(17,16))+
   annotate("text",x=1, y=15, label="Likelihood of being a PI",size=5,fontface="bold",hjust=1) +
   theme(axis.title.y = element_blank(), # Remove y axis title
         axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
         axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
         axis.title.x = element_text(size=14,vjust=0.5,colour="black",face="bold"), # Change size and font of x axis title and move it down a bit
         panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
         axis.line = element_line(colour="black"),
         panel.background = element_rect(colour = "black", size=1, fill=NA),
         legend.position = c(0.85,0.5), # Position legend inside plot
         legend.key = element_rect(color = NA,fill=NA), # Make background of symbols in legend grey
         legend.text=element_text(size=14),
         legend.key.size = unit(0.5,"cm"),
         legend.key.width = unit(1,"cm"),
         legend.background=element_rect(colour=NA,fill=NA),
         legend.title =element_blank())

#Now set up plots for Time to PI and Career Length (TPICL)
#Uses similar approach to above.

labeltext_TPICL<-c(1:12)

Gender_TPICL<-c("F","M","F","M","F","M","F","M","F","M","F","M")

#exp(model$res.t[3]) #gives exp(est) i.e. decceleration factor
#exp(model$res.t[6]) #gives lower CI
#exp(model$res.t[9]) #gives upper CI

estimate_TPICL<-c(exp(m4F$res.t[3]),
                  exp(m4M$res.t[3]),
                  exp(m5F$res.t[3]),
                  exp(m5M$res.t[3]),
                  exp(m6F$res.t[3]),
                  exp(m6M$res.t[3]),
                  exp(m7F$res.t[3]),
                  exp(m7M$res.t[3]),
                  exp(m8F$res.t[3]),
                  exp(m8M$res.t[3]),
                  exp(m9F$res.t[3]),
                  exp(m9M$res.t[3]))

lower_TPICL<-c(exp(m4F$res.t[6]),
               exp(m4M$res.t[6]),
               exp(m5F$res.t[6]),
               exp(m5M$res.t[6]),
               exp(m6F$res.t[6]),
               exp(m6M$res.t[6]),
               exp(m7F$res.t[6]),
               exp(m7M$res.t[6]),
               exp(m8F$res.t[6]),
               exp(m8M$res.t[6]),
               exp(m9F$res.t[6]),
               exp(m9M$res.t[6]))

upper_TPICL<-c(exp(m4F$res.t[9]),
               exp(m4M$res.t[9]),
               exp(m5F$res.t[9]),
               exp(m5M$res.t[9]),
               exp(m6F$res.t[9]),
               exp(m6M$res.t[9]),
               exp(m7F$res.t[9]),
               exp(m7M$res.t[9]),
               exp(m8F$res.t[9]),
               exp(m8M$res.t[9]),
               exp(m9F$res.t[9]),
               exp(m9M$res.t[9]))

df_TPICL <- data.frame(labeltext_TPICL, estimate_TPICL, lower_TPICL, upper_TPICL, Gender_TPICL)

## Reverses the factor level ordering for labels after coord_flip()
df_TPICL$labeltext_TPICL <- factor(df_TPICL$labeltext_TPICL, levels=rev(df_TPICL$labeltext_TPICL))

df_TPICL$colour <- c("dark grey","dark grey","dark grey","dark grey","dark grey","dark grey","black","black","dark grey","dark grey","dark grey","dark grey")


plotTPICL1<-
   ggplot(data=df_TPICL, aes(x=labeltext_TPICL, y=estimate_TPICL, ymin=lower_TPICL, ymax=upper_TPICL,
                             col=Gender_TPICL, shape=Gender_TPICL))+
   annotate("rect", xmin = 0, xmax = 6.5, ymin = 1, ymax = 5, alpha = .2, fill = "grey") +
   annotate("rect", xmin = 6.5, xmax = 12.7, ymin = 0, ymax = 1, alpha = .2, fill = "grey") +
   geom_point(aes(size=2,col = colour, fill = colour),show.legend=FALSE)+
   geom_pointrange(size=0.7, col = df_TPICL$colour, fill = df_TPICL$colour,show.legend=FALSE) + 
   geom_hline(yintercept=1, lty=2) +  # Add a dotted line at x=1 after flip
   geom_vline(xintercept=6.5, lty=1) + # Adds a solid line at y=6.5 after flip to differentiate between top panel = career longevity and bottom panel = time to PI
   geom_errorbar(aes(ymin=lower_TPICL, ymax=upper_TPICL,width=0.6,cex=1,col=colour, fill = colour),show.legend=FALSE) +
   scale_x_discrete(breaks=c(1,3,5,7,9,11),
                    labels=c("Adj. Network Size","Adj. Tie Strength","Adj. Clustering Coef.",
                             "Adj. Network Size","Adj. Tie Strength","Adj. Clustering Coef.")) +
   coord_flip() +  # Flip coordinates (puts labels on y axis)
   xlab("Label") + ylab("Deceleration factor") +
   scale_fill_manual(values=c("dark grey","dark grey","black","black","dark grey","dark grey","black","black","dark grey","dark grey","dark grey","dark grey"))+
   scale_colour_manual(values=c("black","dark grey","dark grey","dark grey","black","black")) + 
   scale_shape_manual(values=c(17,16))+
   annotate("text",x=7.2, y=5, label="Time to become a PI",size=5,fontface="bold",hjust=1) +
   annotate("text",x=1, y=5, label="Career length", size=5,fontface="bold",hjust=1) +
   theme(axis.title.y = element_blank(), # Remove y axis title
         axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
         axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
         axis.title.x = element_text(size=14,vjust=0.5,colour="black",face="bold"), # Change size and font of x axis title and move it down a bit
         panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
         axis.line = element_line(colour="black"),
         panel.background = element_rect(colour = "black", size=1, fill=NA),
         legend.title =element_blank())+
   guides(colour=FALSE) 


#Add label to plot C
plotTPICL2<-plotTPICL1+
           labs(tag="C")+
           theme(plot.tag.position = c(0.19,0.5),plot.tag = element_text(size=14,face="bold"))
                    
#Now layout plots and plot them - labels for plots A and B added here
ggarrange(plotPIstatus,plotTPICL2,heights=c(1,1.7),ncol=1,nrow=2,labels=c("A","B"),label.x=0.18,label.y=0.95)

###Fig ESM9###

#ESM9.1

# Plots to show effect of the 3Cs on time to PI 
# We want to produce graphs for each gender shown separately, and illustrating the most 'C' and least 'C'
# Therefore, define the top and bottom quartile for each C, and split these by gender to create new.PI datasets

#First split dataset by gender
new.PI.F = new.PI[new.PI$Gender=="F",] 
new.PI.F = new.PI.F[(!is.na(new.PI.F$Gender)),] 
new.PI.F$Gender = factor(new.PI.F$Gender)

new.PI.M = new.PI[new.PI$Gender=="M",] 
new.PI.M = new.PI.M[(!is.na(new.PI.M$Gender)),]
new.PI.M$Gender = factor(new.PI.M$Gender)

#Now, for each C, define the upper and bottom quartiles (e.g. top 25% most collaborative,
#and bottom 25% least collaborative)

# Residuals.degree = measure of collaborativeness
# residuals.strength = measure of consistency
# Residuals.cluster = measure of co-author connectedness

# Collaborativeness (this is all copied from above)
# All
quantile(na.omit(new.PI$residuals.degree), c(.25, .50, .75))
new.PI$strategy.degree =ifelse(new.PI$residuals.degree> (quantile(na.omit(new.PI$residuals.degree), 0.75)),"strongly collaborative",ifelse(new.PI$residuals.degree< (quantile(na.omit(new.PI$residuals.degree), 0.25)),"weakly collaborative", "other"))

# Create categorical variables for F
quantile(na.omit(new.PI.F$residuals.degree), c(.25, .50, .75))
new.PI.F$strategy.degree =ifelse(new.PI.F$residuals.degree> (quantile(na.omit(new.PI.F$residuals.degree), 0.75)) , "strongly collaborative",ifelse(new.PI.F$residuals.degree< (quantile(na.omit(new.PI.F$residuals.degree), 0.25)),"weakly collaborative", "other"))

# Create categorical variables for M
quantile(na.omit(new.PI.M$residuals.degree), c(.25, .50, .75))
new.PI.M$strategy.degree =ifelse(new.PI.M$residuals.degree>(quantile(na.omit(new.PI.M$residuals.degree), 0.75))   , "strongly collaborative",ifelse(new.PI.M$residuals.degree< (quantile(na.omit(new.PI.M$residuals.degree), 0.25)),"weakly collaborative", "other"))

# Remove "other" from strategy.degree variable for F
new.PI.Fsub_strat.deg<-subset(new.PI.F,new.PI.F$strategy.degree!="other")
str(new.PI.Fsub_strat.deg)
new.PI.Fsub_strat.deg$strategy.degree<-as.factor(new.PI.Fsub_strat.deg$strategy.degree)
dim(new.PI.F)
dim(new.PI.Fsub_strat.deg) #Reduced length, so subsetting has worked
# We need to define strategy.degree as a factor to get the plotting to work properly
# We plot using flexsurvreg and this will only plot a single population mean KM survival curve for continuous variables. Defining strategy.degree as a categorical variable (as a factor) gets around this issue.

# Remove "other" from strategy.degree variable for M
new.PI.Msub_strat.deg<-subset(new.PI.M,new.PI.M$strategy.degree!="other")
str(new.PI.Msub_strat.deg)
new.PI.Msub_strat.deg$strategy.degree<-as.factor(new.PI.Msub_strat.deg$strategy.degree)

# Consistency (this is all copied from above)
# All
quantile(na.omit(new.PI$residuals.strength), c(.25, .50, .75))
new.PI$strategy.tot =ifelse(new.PI$residuals.strength> (quantile(na.omit(new.PI$residuals.strength), 0.75))   , "strongly consistent",ifelse(new.PI$residuals.strength< (quantile(na.omit(new.PI$residuals.strength), 0.25)),"weakly consistent", "other"))

# Create categorical variables for F
quantile(na.omit(new.PI.F$residuals.strength), c(.25, .50, .75))
new.PI.F$strategy.tot =ifelse(new.PI.F$residuals.strength> (quantile(na.omit(new.PI.F$residuals.strength), 0.75))   , "strongly consistent",ifelse(new.PI.F$residuals.strength< (quantile(na.omit(new.PI.F$residuals.strength), 0.25)),"weakly consistent", "other"))

# Create categorical variables for M
quantile(na.omit(new.PI.M$residuals.strength), c(.25, .50, .75))
new.PI.M$strategy.tot =ifelse(new.PI.M$residuals.strength> (quantile(na.omit(new.PI.M$residuals.strength), 0.75))   , "strongly consistent",ifelse(new.PI.M$residuals.strength< (quantile(na.omit(new.PI.M$residuals.strength), 0.25)),"weakly consistent", "other"))

# Remove "other" from strategy.tot variable for F
new.PI.Fsub_strat.tot<-subset(new.PI.F,new.PI.F$strategy.tot!="other")
str(new.PI.Fsub_strat.tot)
new.PI.Fsub_strat.tot$strategy.tot<-as.factor(new.PI.Fsub_strat.tot$strategy.tot)

# Remove "other" from strategy.tot variable for M
new.PI.Msub_strat.tot<-subset(new.PI.M,new.PI.M$strategy.tot!="other")
str(new.PI.Msub_strat.tot)
new.PI.Msub_strat.tot$strategy.tot<-as.factor(new.PI.Msub_strat.tot$strategy.tot)

# Clustering coefficient (this is all copied from above)
# All authors
quantile(na.omit(new.PI$residuals.cluster), c(.25, .50, .75))
new.PI$clusteringcoefficient.global.cat <-   ifelse(new.PI$residuals.cluster<(quantile(na.omit(new.PI$residuals.cluster), 0.75)) ,"low",ifelse(new.PI$residuals.cluster>(quantile(na.omit(new.PI$residuals.cluster), 0.25)),  "high", "other"))

# Female
quantile(na.omit(new.PI.F$residuals.cluster), c(.25, .50, .75))
new.PI.F$clusteringcoefficient.global.cat = ifelse(new.PI.F$residuals.cluster<(quantile(na.omit(new.PI.F$residuals.cluster), 0.75)) ,"low",ifelse(new.PI.F$residuals.cluster>(quantile(na.omit(new.PI.F$residuals.cluster), 0.25)) ,  "high", "other"))
table(new.PI.F$clusteringcoefficient.global.cat)

# Male
quantile(na.omit(new.PI.M$residuals.cluster), c(.25, .50, .75))
new.PI.M$clusteringcoefficient.global.cat = ifelse(new.PI.M$residuals.cluster<(quantile(na.omit(new.PI.M$residuals.cluster), 0.75)),"low",ifelse(new.PI.M$residuals.cluster>(quantile(na.omit(new.PI.M$residuals.cluster), 0.25)),  "high", "other"))
table(new.PI.M$clusteringcoefficient.global.cat )

# Remove "other" from clusteringcoefficient.global.cat variable for F
new.PI.Fsub_ccglobal<-subset(new.PI.F,new.PI.F$clusteringcoefficient.global.cat!="other")
str(new.PI.Fsub_ccglobal)
new.PI.Fsub_ccglobal$clusteringcoefficient.global.cat<-as.factor(new.PI.Fsub_ccglobal$clusteringcoefficient.global.cat)

# Remove "other" from clusteringcoefficient.global.cat variable for M
new.PI.Msub_ccglobal<-subset(new.PI.M,new.PI.M$clusteringcoefficient.global.cat!="other")
str(new.PI.Msub_ccglobal)
new.PI.Msub_ccglobal$clusteringcoefficient.global.cat<-as.factor(new.PI.Msub_ccglobal$clusteringcoefficient.global.cat)


ddply(new.PI.Msub_ccglobal,~clusteringcoefficient.global.cat,summarise,mean=mean(n.years))
#clusteringcoefficient.global.cat     mean
#1                             high 27.84892
#2                              low 28.00725

#So the datasets and variables we need to use for plotting are:
#                    For F:                                For M:
#Collaborativeness   new.PI.Fsub_strat.deg                    new.PI.Msub_strat.deg
#                    strategy.degree                       strategy.degree
#
#Consistency         new.PI.Fsub_strat.tot                    new.PI.Msub_strat.tot
#                    strategy.tot                          strategy.tot
#
#Clustering coeff    new.PI.Fsub_ccglobal                     new.PI.Msub_ccglobal
#                    #clusteringcoefficient.global.cat     clusteringcoefficient.global.cat

# Figure layout
# We want a figure with nine panels
layout.matrixa<-matrix(c(1,4,7,2,5,8,3,6,9),ncol=3,nrow=3)
layout.matrixa
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    4    5    6
#[3,]    7    8    9
#This is how the plot will be laid out

layout(mat = layout.matrixa,
       heights = c(2,2,2), # Heights of the three rows
       widths = c(1.5,2, 2)) # Widths of the three columns

layout.show(9) #Shows the layout of the plots (9 plots)

# Now fill in the individual plots
# Plot 1 - this will just be text
par(mar = c(0,0,0,0))

plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.52, y = 0.4, paste("Adj. Network Size"), 
     cex = 1.4, col = "black",font=2)

par(mar = c(0, 6, 4, 0) + 0.1)

# Plot 2 - Top left plot
# model
# To plot what we want requires a bit more work...
plot2a<-flexsurvreg(Surv(time.PI,status.PI)~strategy.degree,dist="lnorm",data=new.PI.Fsub_strat.deg) #This gives the KM curve
plot(plot2a,fun="event",ci=FALSE,est=FALSE,lty.obs=2,lwd.obs=3,las=1,
     xaxt='n',
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     cex.axis=1.2,
     main=paste("F"),adj=0.5,font.main=2,cex.main=1.4)
Axis(side=1,labels=FALSE)
box(lwd=1.5)

legend(-9,1.1,legend=c("A"),box.lty=0,text.font=1,cex=1.5,bg="transparent")

# Now we need to overlay the model curves on top, one for each quantile.

# First for F
# Weakly collaborative
new.PI.Fsub_strat.deg_weak<-subset(new.PI.Fsub_strat.deg,new.PI.Fsub_strat.deg$strategy.degree=="weakly collaborative")
plotF2a<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Fsub_strat.deg_weak)
plotF2a
#Estimates: 
#est    L95%   U95%   se   
#meanlog  3.710  2.846  4.574  0.441
#sdlog    1.749  1.190  2.572  0.344

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pF_weak<-plnorm(t,plotF2a$res[1],plotF2a$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pF_weak,col="grey",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotF2a$res[3],plotF2a$res[4])
upperCI_p<-plnorm(t,plotF2a$res[5],plotF2a$res[6])

lines(lowerCI_p,col="grey",lty=2) 
lines(upperCI_p,col="grey",lty=2)

# Strongly collaborative
new.PI.Fsub_strat.deg_strong<-subset(new.PI.Fsub_strat.deg,new.PI.Fsub_strat.deg$strategy.degree=="strongly collaborative")
plotF2b<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Fsub_strat.deg_strong)
plotF2b
#Estimates: 
#   est     L95%    U95%    se    
#meanlog  2.0450  1.8275  2.2626  0.1110
#sdlog    0.9496  0.7983  1.1296  0.0841

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pF_strong<-plnorm(t,plotF2b$res[1],plotF2b$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pF_strong,col="black",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotF2b$res[3],plotF2b$res[4])
upperCI_p<-plnorm(t,plotF2b$res[5],plotF2b$res[6])

lines(lowerCI_p,col="black",lty=2) 
lines(upperCI_p,col="black",lty=2)

par(mar = c(0, 0.5, 4, 5.5) + 0.1)  

# Plot 3 - Top right plot
# model
plot3a<-flexsurvreg(Surv(time.PI,status.PI)~strategy.degree,dist="lnorm",data=new.PI.Msub_strat.deg) #This gives the KM curve
plot(plot3a,fun="event",ci=FALSE,est=FALSE,lty.obs=2,lwd.obs=3,las=1,
     lwd=3, lwd.ci=2, 
     axes=FALSE,frame.plot=T,
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     main=paste("M"),adj=0.5,font.main=2,cex.main=1.4)
Axis(side=1,labels=FALSE)
Axis(side=2,labels=FALSE)
box(lwd=1.5)

legend(-9,1.1,legend=c("B"),box.lty=0,text.font=1,cex=1.5,bg="transparent")
legend(7,0.4,legend=c("High","Low"), 
       col=c("black","grey"),
       lty=1.1,lwd=3,cex=1,
       bg="transparent",
       text.font=1,
       box.lty=0,seg.len=0.5)

# Now we need to overlay the model curves on top, one for each quantile, and we need to do this for each gender!
# Now for M

# Weakly collaborative
new.PI.Msub_strat.deg_weak<-subset(new.PI.Msub_strat.deg,new.PI.Msub_strat.deg$strategy.degree=="weakly collaborative")
plotM3a<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Msub_strat.deg_weak)
plotM3a
#Estimates: 
#est    L95%   U95%   se   
#meanlog  2.733  2.423  3.044  0.158
#sdlog    1.545  1.294  1.844  0.140

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pM_weak<-plnorm(t,plotM3a$res[1],plotM3a$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pM_weak,col="grey",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotM3a$res[3],plotM3a$res[3])
upperCI_p<-plnorm(t,plotM3a$res[5],plotM3a$res[5])

lines(lowerCI_p,col="grey",lty=2) 
lines(upperCI_p,col="grey",lty=2)

# Strongly collaborative
new.PI.Msub_strat.deg_strong<-subset(new.PI.Msub_strat.deg,new.PI.Msub_strat.deg$strategy.degree=="strongly collaborative")
plotM3b<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Msub_strat.deg_strong)
plotM3b
#Estimates: 
#est     L95%    U95%    se    
#meanlog  1.7347  1.6051  1.8643  0.0661
#sdlog    0.8329  0.7454  0.9306  0.0471

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pM_strong<-plnorm(t,plotM3b$res[1],plotM3b$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pM_strong,col="black",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotM3b$res[3],plotM3b$res[4])
upperCI_p<-plnorm(t,plotM3b$res[5],plotM3b$res[6])

lines(lowerCI_p,col="black",lty=2) 
lines(upperCI_p,col="black",lty=2)

# Plot 4 - this will just be text
par(mar = c(0,0,0,0))
plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.52, y = 0.5, paste("Adj. Tie Strength"), 
     cex = 1.4, col = "black",font=2)

par(mar = c(2, 6, 2, 0) + 0.1)


# Plot 5 - Middle left plot
# model
plot5a<-flexsurvreg(Surv(time.PI,status.PI)~strategy.tot,dist="lnorm",data=new.PI.Fsub_strat.tot)
# plotting code
plot(plot5a,fun="event",ci=FALSE,est=FALSE,lty.obs=2,lwd.obs=3,las=1,
     #ylab="Probability of being a PI", cex.lab=1.5,
     xaxt='n',
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     cex.axis=1.2)
#col=c("darkorange","darkorchid2"))
Axis(side=1,labels=FALSE)
box(lwd=1.5)

legend(-9,1.1,legend=c("C"),box.lty=0,text.font=1,cex=1.5,bg="transparent")

# Now we need to overlay the model curves on top, one for each quantile.
# First for F

# Weakly consistent
new.PI.Fsub_strat.tot_weak<-subset(new.PI.Fsub_strat.tot,new.PI.Fsub_strat.tot$strategy.tot=="weakly consistent")
plotF5a<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Fsub_strat.tot_weak)
plotF5a
#Estimates: 
#est    L95%   U95%   se   
#meanlog  2.996  2.606  3.386  0.199
#sdlog    1.303  1.004  1.691  0.173

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pF_weak<-plnorm(t,plotF5a$res[1],plotF5a$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pF_weak,col="grey",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotF5a$res[3],plotF5a$res[4]) 
upperCI_p<-plnorm(t,plotF5a$res[5],plotF5a$res[6]) 

lines(lowerCI_p,col="grey",lty=2)
lines(upperCI_p,col="grey",lty=2)

# Strongly consistent
new.PI.Fsub_strat.tot_strong<-subset(new.PI.Fsub_strat.tot,new.PI.Fsub_strat.tot$strategy.tot=="strongly consistent")
plotF5b<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Fsub_strat.tot_strong)
plotF5b
#Estimates: 
#est    L95%   U95%   se   
#meanlog  2.088  1.767  2.410  0.164
#sdlog    1.295  1.056  1.590  0.135

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pF_strong<-plnorm(t,plotF5b$res[1],plotF5b$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pF_strong,col="black",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotF5b$res[3],plotF5b$res[4])
upperCI_p<-plnorm(t,plotF5b$res[5],plotF5b$res[6])

lines(lowerCI_p,col="black",lty=2)
lines(upperCI_p,col="black",lty=2)

par(mar = c(2, 0.5, 2, 5.5) + 0.1)     

# Plot 6 - Middle right plot
# model
plot6a<-flexsurvreg(Surv(time.PI,status.PI)~strategy.tot,dist="lnorm",data=new.PI.Msub_strat.tot)
# plotting code
plot(plot6a,fun="event",ci=FALSE,est=FALSE,lty.obs=2,lwd.obs=3,las=1,
     lwd=3, lwd.ci=2, 
     axes=FALSE,frame.plot=T,
     bty='n',
     xlim=c(0,40),ylim=c(0,1))
#col=c("darkorange","darkorchid2"))
Axis(side=1,labels=FALSE)
Axis(side=2,labels=FALSE)
box(lwd=1.5)

legend(-9,1.1,legend=c("D"),box.lty=0,text.font=1,cex=1.5,bg="transparent")


# Now we need to overlay the model curves on top, one for each quantile.
# Now for M
# Weakly consistent
new.PI.Msub_strat.tot_weak<-subset(new.PI.Msub_strat.tot,new.PI.Msub_strat.tot$strategy.tot=="weakly consistent")
plotM6a<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Msub_strat.tot_weak)
plotM6a
#Estimates: 
#est     L95%    U95%    se    
#meanlog  2.1878  1.9998  2.3758  0.0959
#sdlog    1.1466  1.0032  1.3104  0.0781

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pM_weak<-plnorm(t,plotM6a$res[1],plotM6a$res[2])#takes the est values for meanlog and sdlog from the plotF summary above
lines(pM_weak,col="grey",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotM6a$res[3],plotM6a$res[4])#
upperCI_p<-plnorm(t,plotM6a$res[5],plotM6a$res[6])#

lines(lowerCI_p,col="grey",lty=2) 
lines(upperCI_p,col="grey",lty=2)

# Strongly consistent
new.PI.Msub_strat.tot_strong<-subset(new.PI.Msub_strat.tot,new.PI.Msub_strat.tot$strategy.tot=="strongly consistent")
plotM6b<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Msub_strat.tot_strong)
plotM6b
#Estimates: 
#est     L95%    U95%    se    
#meanlog  1.9984  1.8264  2.1705  0.0878
#sdlog    1.0491  0.9204  1.1957  0.0700

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pM_strong<-plnorm(t,plotM6b$res[1],plotM6b$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pM_strong,col="black",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotM6b$res[3],plotM6b$res[4])
upperCI_p<-plnorm(t,plotM6b$res[5],plotM6b$res[6])

lines(lowerCI_p,col="black",lty=2) 
lines(upperCI_p,col="black",lty=2)

# Plot 7 - this will just be text
par(mar = c(0,0,0,0))

plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.52, y = 0.6, paste("Adj. Clustering Coeff."), 
     cex = 1.4, col = "black",font=2)

par(mar = c(4, 6, 0, 0) + 0.1)


# Plot 8 - Bottom left plot
# model
plot8a<-flexsurvreg(Surv(time.PI,status.PI)~clusteringcoefficient.global.cat,dist="lnorm",data=new.PI.Fsub_ccglobal)
# plotting code
plot(plot8a,fun="event",ci=FALSE,est=FALSE,lty.obs=2,lwd.obs=3,las=1,
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     cex.axis=1.2)
#col=c("gray","blue"))
#Axis(side=2,labels=T)
box(lwd=1.5)

legend(-9,1.1,legend=c("E"),box.lty=0,text.font=1,cex=1.5,bg="transparent") 

# Now we need to overlay the model curves on top, one for each quantile.
# First for F
# Low co-author connectedness
new.PI.Fsub_ccglobal_low<-subset(new.PI.Fsub_ccglobal,new.PI.Fsub_ccglobal$clusteringcoefficient.global.cat=="low")
plotF8a<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Fsub_ccglobal_low)
plotF8a
#Estimates: 
#est     L95%    U95%    se    
#meanlog  2.3307  2.1702  2.4912  0.0819
#sdlog    1.0949  0.9687  1.2375  0.0684

t<-1:39 #(Because our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pF_low<-plnorm(t,plotF8a$res[1],plotF8a$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pF_low,col="grey",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotF8a$res[3],plotF8a$res[4])
upperCI_p<-plnorm(t,plotF8a$res[5],plotF8a$res[6])

lines(lowerCI_p,col="grey",lty=2) 
lines(upperCI_p,col="grey",lty=2)

## High co-author connectedness
new.PI.Fsub_ccglobal_high<-subset(new.PI.Fsub_ccglobal,new.PI.Fsub_ccglobal$clusteringcoefficient.global.cat=="high")
plotF8b<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Fsub_ccglobal_high)
plotF8b
#Estimates: 
#   est    L95%   U95%   se   
#meanlog  2.609  2.299  2.919  0.158
#sdlog    1.210  0.976  1.499  0.133

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pF_high<-plnorm(t,plotF8b$res[1],plotF8b$res[2])#takes the est values for meanlog and sdlog from the plotF summary above
lines(pF_high,col="black",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotF8b$res[3],plotF8b$res[4])
upperCI_p<-plnorm(t,plotF8b$res[5],plotF8b$res[6])

lines(lowerCI_p,col="black",lty=2) 
lines(upperCI_p,col="black",lty=2)

par(mar = c(4, 0.5, 0, 5.5) + 0.1)

# Plot 9 - Bottom right plot
# model
plot9a<-flexsurvreg(Surv(time.PI,status.PI)~clusteringcoefficient.global.cat,dist="lnorm",data=new.PI.Msub_ccglobal)
# plotting code
plot(plot9a,fun="event",ci=FALSE,est=FALSE,lty.obs=2,lwd.obs=3,las=1,
     lwd=3, lwd.ci=2, 
     axes=FALSE,frame.plot=T,
     bty='n',
     #xlab="Publication years",cex.lab=1.2,
     xlim=c(0,40),ylim=c(0,1))
#col=c("blue","gray"))
Axis(side=1,labels=T,cex.axis=1.2)
Axis(side=2,labels=FALSE)
box(lwd=1.5)

legend(-9,1.1,legend=c("F"),box.lty=0,text.font=1,cex=1.5,bg="transparent")


# Now we need to overlay the model curves on top, one for each quantile.
# Now for M
# Low co-author connectedness
new.PI.Msub_ccglobal_low<-subset(new.PI.Msub_ccglobal,new.PI.Msub_ccglobal$clusteringcoefficient.global.cat=="low")
plotM9a<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Msub_ccglobal_low)
plotM9a
#Estimates: 
#est     L95%    U95%    se    
#meanlog  1.9119  1.8205  2.0032  0.0466
#sdlog    0.9874  0.9194  1.0604  0.0359

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pM_low<-plnorm(t,plotM9a$res[1],plotM9a$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pM_low,col="grey",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotM9a$res[3],plotM9a$res[4])
upperCI_p<-plnorm(t,plotM9a$res[5],plotM9a$res[6])

lines(lowerCI_p,col="grey",lty=2) 
lines(upperCI_p,col="grey",lty=2)

## High co-author connectedness
new.PI.Msub_ccglobal_high<-subset(new.PI.Msub_ccglobal,new.PI.Msub_ccglobal$clusteringcoefficient.global.cat=="high")
plotM9b<-flexsurvreg(Surv(time.PI,status.PI)~1,dist="lnorm",data=new.PI.Msub_ccglobal_high)
plotM9b
#Estimates: 
#         est    L95%   U95%   se   
#meanlog  2.1917  2.0279  2.3555  0.0836
#sdlog    1.0126  0.8893  1.1529  0.0670

t<-1:39 #(Beacuse our dataset covers 39 years max)
#p <- plnorm(t, meanlog, sdlog) 

pM_high<-plnorm(t,plotM9b$res[1],plotM9b$res[2]) #takes the est values for meanlog and sdlog from the plotF summary above
lines(pM_high,col="black",lwd=3) #Adds the model line for F (overlays on bottom KM curve, as we expect)
# Add CIs
lowerCI_p<-plnorm(t,plotM9b$res[3],plotM9b$res[4])
upperCI_p<-plnorm(t,plotM9b$res[5],plotM9b$res[6])

lines(lowerCI_p,col="black",lty=2) 
lines(upperCI_p,col="black",lty=2)

# Now add x-axis label by overlaying new.PI plots on top of the existing one:

par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
# Creates new.PI plot over existing plot device

plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
# Adds an empty plot

text(x = 0.215, y = -1.04, paste("Time from first publication year"), 
     cex = 1.5, col = "black",font=1)

text(x=-0.42, y=0.0065, paste("Probability of being a PI"),
     cex=1.5, col="black",font=1,srt=90)

#ESM9.2

# Plots to show effect of the 3Cs on career time 
# We want to produce graphs for each gender shown separately, and illustrating the most 'C' and least 'C'
# Therefore, define the top and bottom quartile for each C, and split these by gender to create new datasets

#First split dataset by gender
new.F = new[new$Gender=="F",] 
new.F = new.F[(!is.na(new.F$Gender)),] 
new.F$Gender = factor(new.F$Gender)

new.M = new[new$Gender=="M",] 
new.M = new.M[(!is.na(new.M$Gender)),]
new.M$Gender = factor(new.M$Gender)

#Now, for each C, define the upper and bottom quartiles (e.g. top 25% most collaborative,
#and bottom 25% least collaborative)

# Residuals.degree = measure of collaborativeness
# residuals.strength = measure of consistency
# Residuals.cluster = measure of co-author connectedness

# Collaborativeness (this is all copied from above)
# All
quantile(na.omit(new$residuals.degree), c(.25, .50, .75))
new$strategy.degree =ifelse(new$residuals.degree> (quantile(na.omit(new$residuals.degree), 0.75)),"strongly collaborative",ifelse(new$residuals.degree< (quantile(na.omit(new$residuals.degree), 0.25)),"weakly collaborative", "other"))

# Create categorical variables for F
quantile(na.omit(new.F$residuals.degree), c(.25, .50, .75))
new.F$strategy.degree =ifelse(new.F$residuals.degree> (quantile(na.omit(new.F$residuals.degree), 0.75)) , "strongly collaborative",ifelse(new.F$residuals.degree< (quantile(na.omit(new.F$residuals.degree), 0.25)),"weakly collaborative", "other"))

# Create categorical variables for M
quantile(na.omit(new.M$residuals.degree), c(.25, .50, .75))
new.M$strategy.degree =ifelse(new.M$residuals.degree>(quantile(na.omit(new.M$residuals.degree), 0.75))   , "strongly collaborative",ifelse(new.M$residuals.degree< (quantile(na.omit(new.M$residuals.degree), 0.25)),"weakly collaborative", "other"))

# Remove "other" from strategy.degree variable for F
new.Fsub_strat.deg<-subset(new.F,new.F$strategy.degree!="other")
str(new.Fsub_strat.deg)
new.Fsub_strat.deg$strategy.degree<-as.factor(new.Fsub_strat.deg$strategy.degree)
dim(new.F)
dim(new.Fsub_strat.deg) #Reduced length, so subsetting has worked
# We need to define strategy.degree as a factor to get the plotting to work properly
# We plot using flexsurvreg and this will only plot a single population mean KM survival curve for continuous variables. Defining strategy.degree as a categorical variable (as a factor) gets around this issue.

# Remove "other" from strategy.degree variable for M
new.Msub_strat.deg<-subset(new.M,new.M$strategy.degree!="other")
str(new.Msub_strat.deg)
new.Msub_strat.deg$strategy.degree<-as.factor(new.Msub_strat.deg$strategy.degree)

# Consistency (this is all copied from above)
# All
quantile(na.omit(new$residuals.strength), c(.25, .50, .75))
new$strategy.tot =ifelse(new$residuals.strength> (quantile(na.omit(new$residuals.strength), 0.75))   , "strongly consistent",ifelse(new$residuals.strength< (quantile(na.omit(new$residuals.strength), 0.25)),"weakly consistent", "other"))

# Create categorical variables for F
quantile(na.omit(new.F$residuals.strength), c(.25, .50, .75))
new.F$strategy.tot =ifelse(new.F$residuals.strength> (quantile(na.omit(new.F$residuals.strength), 0.75))   , "strongly consistent",ifelse(new.F$residuals.strength< (quantile(na.omit(new.F$residuals.strength), 0.25)),"weakly consistent", "other"))

# Create categorical variables for M
quantile(na.omit(new.M$residuals.strength), c(.25, .50, .75))
new.M$strategy.tot =ifelse(new.M$residuals.strength> (quantile(na.omit(new.M$residuals.strength), 0.75))   , "strongly consistent",ifelse(new.M$residuals.strength< (quantile(na.omit(new.M$residuals.strength), 0.25)),"weakly consistent", "other"))

# Remove "other" from strategy.tot variable for F
new.Fsub_strat.tot<-subset(new.F,new.F$strategy.tot!="other")
str(new.Fsub_strat.tot)
new.Fsub_strat.tot$strategy.tot<-as.factor(new.Fsub_strat.tot$strategy.tot)

# Remove "other" from strategy.tot variable for M
new.Msub_strat.tot<-subset(new.M,new.M$strategy.tot!="other")
str(new.Msub_strat.tot)
new.Msub_strat.tot$strategy.tot<-as.factor(new.Msub_strat.tot$strategy.tot)

# Clustering coefficient (this is all copied from above)
# All authors
quantile(na.omit(new$residuals.cluster), c(.25, .50, .75))
new$clusteringcoefficient.global.cat <-   ifelse(new$residuals.cluster<(quantile(na.omit(new$residuals.cluster), 0.75)) ,"low",ifelse(new$residuals.cluster>(quantile(na.omit(new$residuals.cluster), 0.25)),  "high", "other"))

# Female
quantile(na.omit(new.F$residuals.cluster), c(.25, .50, .75))
new.F$clusteringcoefficient.global.cat = ifelse(new.F$residuals.cluster<(quantile(na.omit(new.F$residuals.cluster), 0.75)) ,"low",ifelse(new.F$residuals.cluster>(quantile(na.omit(new.F$residuals.cluster), 0.25)) ,  "high", "other"))
table(new.F$clusteringcoefficient.global.cat)

# Male
quantile(na.omit(new.M$residuals.cluster), c(.25, .50, .75))
new.M$clusteringcoefficient.global.cat = ifelse(new.M$residuals.cluster<(quantile(na.omit(new.M$residuals.cluster), 0.75)),"low",ifelse(new.M$residuals.cluster>(quantile(na.omit(new.M$residuals.cluster), 0.25)),  "high", "other"))
table(new.M$clusteringcoefficient.global.cat )

# Remove "other" from clusteringcoefficient.global.cat variable for F
new.Fsub_ccglobal<-subset(new.F,new.F$clusteringcoefficient.global.cat!="other")
str(new.Fsub_ccglobal)
new.Fsub_ccglobal$clusteringcoefficient.global.cat<-as.factor(new.Fsub_ccglobal$clusteringcoefficient.global.cat)

# Remove "other" from clusteringcoefficient.global.cat variable for M
new.Msub_ccglobal<-subset(new.M,new.M$clusteringcoefficient.global.cat!="other")
str(new.Msub_ccglobal)
new.Msub_ccglobal$clusteringcoefficient.global.cat<-as.factor(new.Msub_ccglobal$clusteringcoefficient.global.cat)


ddply(new.Msub_ccglobal,~clusteringcoefficient.global.cat,summarise,mean=mean(n.years))
#clusteringcoefficient.global.cat     mean
#1                             high 26.21384
#2                              low 25.87789

#So the datasets and variables we need to use for plotting are:
#                    For F:                                For M:
#Collaborativeness   new.Fsub_strat.deg                    new.Msub_strat.deg
#                    strategy.degree                       strategy.degree
#
#Consistency         new.Fsub_strat.tot                    new.Msub_strat.tot
#                    strategy.tot                          strategy.tot
#
#Clustering coeff    new.Fsub_ccglobal                     new.Msub_ccglobal
#                    #clusteringcoefficient.global.cat     clusteringcoefficient.global.cat


# Plots to show effect of the 3Cs on career longevity

#Figure layout
#We want a figure with nine panels
layout.matrix<-matrix(c(1,4,7,2,5,8,3,6,9),ncol=3,nrow=3)
layout.matrix
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    4    5    6
#[3,]    7    8    9
#This is how the plot will be laid out

layout(mat = layout.matrix,
       heights = c(2, 2, 2), # Heights of the three rows
       widths = c(1.5, 2, 2)) # Widths of the three columns

layout.show(9) #Shows the layout of the plots (9 plots)

# Now fill in the individual plots

# Plot 1 - this will just be text
par(mar = c(0,0,0,0))

plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.52, y = 0.4, paste("Adj. Network Size"), 
     cex = 1.4, col = "black",font=2)

par(mar = c(0, 6, 4, 0) + 0.1)


# Plot 2 - Top left plot
# model
plot2<-flexsurvreg(Surv(n.years,status)~strategy.degree,dist="lnorm",data=new.Fsub_strat.deg)
# plotting code
plot(plot2,type="survival",ci=TRUE,lty.obs=2,lwd.obs=3,cl=0.95,las=1,
     lwd=3, lwd.ci=2, #ylab="Survival probability", cex.lab=1.2,
     xaxt='n',
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     cex.axis=1.2,
     col=c("grey","black"),main=paste("F"),adj=0.5,font.main=2)
Axis(side=1,labels=FALSE)
box(lwd=1.5)

legend(-11,1.1,legend=c("A"),box.lty=0,text.font=1,cex=1.5,bg="transparent")

par(mar = c(0, 0.5, 4, 5.5) + 0.1)     

# Plot 3 - Top right plot
# model
plot3<-flexsurvreg(Surv(n.years,status)~strategy.degree,dist="lnorm",data=new.Msub_strat.deg)
# plotting code
plot(plot3,type="survival",ci=TRUE,lty.obs=2,lwd.obs=3,cl=0.95,las=1,
     lwd=3, lwd.ci=2, 
     axes=FALSE,frame.plot=T,
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     col=c("black", "grey"),main=paste("M"),adj=0.5,font.main=2)  # \n (n=636)
Axis(side=1,labels=FALSE)
Axis(side=2,labels=FALSE)
box(lwd=1.5)

legend(-11,1.1,legend=c("B"),box.lty=0,text.font=1,cex=1.5,bg="transparent")
legend(5,0.4,legend=c("High","Low"), #-0.07,0.66
       col=c("black","grey"),
       lty=1.1,lwd=3,cex=1,
       bg="transparent",
       text.font=1,
       box.lty=0,seg.len=0.5)

# Plot 4 - this will just be text
par(mar = c(0,0,0,0))
plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.52, y = 0.5, paste("Adj. Tie Strength"), 
     cex = 1.4, col = "black",font=2)

par(mar = c(2, 6, 2, 0) + 0.1)

# Plot 5 - Middle left plot
# model
plot5<-flexsurvreg(Surv(n.years,status)~strategy.tot,dist="lnorm",data=new.Fsub_strat.tot)
# plotting code
plot(plot5,type="survival",ci=TRUE,lty.obs=2,lwd.obs=3,cl=0.95,las=1,
     lwd=3, lwd.ci=2, ylab="Survival probability", cex.lab=1.5,
     xaxt='n',
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     cex.axis=1.2,
     col=c("black","grey"))
Axis(side=1,labels=FALSE)
box(lwd=1.5)

legend(-11,1.1,legend=c("C"),box.lty=0,text.font=1,cex=1.5,bg="transparent")

par(mar = c(2, 0.5, 2, 5.5) + 0.1)     

# Plot 6 - Middle right plot
# model
plot6<-flexsurvreg(Surv(n.years,status)~strategy.tot,dist="lnorm",data=new.Msub_strat.tot)
# plotting code
plot(plot6,type="survival",ci=TRUE,lty.obs=2,lwd.obs=3,cl=0.95,las=1,
     lwd=3, lwd.ci=2, 
     axes=FALSE,frame.plot=T,
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     col=c("black","grey"))
Axis(side=1,labels=FALSE)
Axis(side=2,labels=FALSE)
box(lwd=1.5)

legend(-11,1.1,legend=c("D"),box.lty=0,text.font=1,cex=1.5,bg="transparent")


# Plot 7 - this will just be text
par(mar = c(0,0,0,0))

plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.52, y = 0.6, paste("Adj. Clustering Coeff."), 
     cex = 1.4, col = "black",font=2)

par(mar = c(4, 6, 0, 0) + 0.1)

# Plot 8 - Bottom left plot
# model
plot8<-flexsurvreg(Surv(n.years,status)~clusteringcoefficient.global.cat,dist="lnorm",data=new.Fsub_ccglobal)
# plotting code
plot(plot8,type="survival",ci=TRUE,lty.obs=2,lwd.obs=3,cl=0.95,las=1,
     lwd=3, lwd.ci=2, #ylab="Survival probability", cex.lab=1.2,
     #xlab="Publication years",
     bty='n',
     xlim=c(0,40),ylim=c(0,1),
     cex.axis=1.2,
     col=c("black","grey"))
# Axis(side=2,labels=T)
box(lwd=1.5)

legend(-11,1.1,legend=c("E"),box.lty=0,text.font=1,cex=1.5,bg="transparent") 

par(mar = c(4, 0.5, 0, 5.5) + 0.1)

# Plot 9 - Bottom right plot
# model
plot9<-flexsurvreg(Surv(n.years,status)~clusteringcoefficient.global.cat,dist="lnorm",data=new.Msub_ccglobal)
# plotting code
plot(plot9,type="survival",ci=TRUE,lty.obs=2,lwd.obs=3,cl=0.95,las=1,
     lwd=3, lwd.ci=2, 
     axes=FALSE,frame.plot=T,
     bty='n',
     #xlab="Publication years",cex.lab=1.2,
     xlim=c(0,40),ylim=c(0,1),
     col=c("grey","black"))
Axis(side=1,labels=T,cex.axis=1.2)
Axis(side=2,labels=FALSE)
box(lwd=1.5)

legend(-11,1.1,legend=c("F"),box.lty=0,text.font=1,cex=1.5,bg="transparent")


#Now add x-axis label by overlaying new plots on top of the existing one:
#See https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics?rq=1

par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
#Creates new plot over existing plot device

plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
#Adds an empty plot

text(x = 0.215, y = -1.04, paste("Career length (no. of years publishing)"), 
     cex = 1.5, col = "black",font=1)

#################################################
sessionInfo()
