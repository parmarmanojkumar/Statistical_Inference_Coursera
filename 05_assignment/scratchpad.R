rm(list=ls())
cat('\014')
library(gridExtra)
library(ggplot2)
library(pastecs)
#load dataset
library(datasets)
data("ToothGrowth")
stat.desc(ToothGrowth)
#sample count dose vise and supplement wise
with(ToothGrowth,table(supp, dose))
# mean and sd of samples dose wise
with(ToothGrowth,tapply(len, dose, mean))
with(ToothGrowth,tapply(len, dose, sd))
# mean and sd of samples supplement wise
with(ToothGrowth,tapply(len, supp, mean))
with(ToothGrowth,tapply(len, supp, sd))
# mean and sd of samples dose wise & supplement wise
with(ToothGrowth,tapply(len, list(supp,dose), mean))
with(ToothGrowth,tapply(len, list(supp,dose), sd))

# converting doses to factor for plotting
ToothGrowth$dose <- factor(ToothGrowth$dose)
#plotting of raw data
g0 <- ggplot(ToothGrowth, aes(x = dose, y = len, color = supp, shape = supp)) + 
        geom_point(alpha=0.7 , size = 3) 
g0
#boxplot of data dose wise
g1 <- ggplot(ToothGrowth, aes(x = dose, y = len, fill = dose)) + 
        geom_boxplot(alpha=0.7)
g1
#boxplot of data supplement wise
g2 <- ggplot(ToothGrowth, aes(x = supp, y = len, fill = supp)) + 
        geom_boxplot(alpha=0.7)
g2

#two box plot of data supplement wise and dose wise
g3 <- ggplot(ToothGrowth, aes(x = supp, y = len, fill = dose)) + 
        geom_boxplot(alpha=0.7)
g3
g4 <- ggplot(ToothGrowth, aes(x = dose, y = len, fill = supp)) + 
        geom_boxplot(alpha=0.7)
g4

#Reload the data.
data("ToothGrowth")
#Dose wise seperation
d05 <- subset(ToothGrowth,dose==0.5,select = c(len,dose))
stat.desc(d05)
d10 <- subset(ToothGrowth,dose==1.0,select = c(len,dose))
stat.desc(d10)
d20 <- subset(ToothGrowth,dose==2.0,select = c(len,dose))
stat.desc(d20)

#supplement wise seperation
oj <- subset(ToothGrowth,supp=="OJ",select = c(len,dose))
stat.desc(oj)
vc <- subset(ToothGrowth,supp=="VC",select = c(len,dose))
stat.desc(vc)

#supplement and dosewise seperation
ojd05 <- oj[oj$dose==0.5,]$len
stat.desc(ojd05)
ojd10 <- oj[oj$dose==1.0,]$len
stat.desc(ojd10)
ojd20 <- oj[oj$dose==2.0,]$len
stat.desc(ojd20)
vcd05 <- vc[vc$dose==0.5,]$len
stat.desc(vcd05)
vcd10 <- vc[vc$dose==1.0,]$len
stat.desc(vcd10)
vcd20 <- vc[vc$dose==2.0,]$len
stat.desc(vcd20)

# hypothesis

#1 : Dose increase have impact on teeth growth
testd10vsd05 = t.test(d10$len,d05$len,paired = F,var.equal = F)
testd10vsd05
# Accept it
testd20vsd05 = t.test(d20$len,d05$len,paired = F,var.equal = F)
testd20vsd05
# Accept it
testd20vsd10 = t.test(d20$len,d10$len,paired = F,var.equal = F)
testd20vsd10
# Accept  it

#2 : OJ supplement have impact on teeth growth
testojvsvc = t.test(oj$len,vc$len, paired = F, var.equal = F)
testojvsvc
# Reject it

# 3 : dose wise, OJ supplement have impact on teeth grwoth
testojd05vsvcd05 = t.test(ojd05,vcd05,paired = F, var.equal = F)
testojd05vsvcd05
# Accept it
testojd10vsvcd10 = t.test(ojd10,vcd10,paired = F, var.equal = F)
testojd10vsvcd10
# Accept it
testojd20vsvcd20 = t.test(ojd20,vcd20,paired = F, var.equal = F)
testojd20vsvcd20
# Reject it