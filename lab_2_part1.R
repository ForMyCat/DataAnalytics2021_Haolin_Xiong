#--------------------------------------------------------
# Data Analytics 2021 FALL Lab02                         |
# By: Haolin Xiong                                       |
# RCS: xiongh                                            |
# 09/24/2021                                             |
#--------------------------------------------------------

#-------------------------------------------------------
#                        Part 1a                        |
#-------------------------------------------------------
library(modeest)
library(dplyr)
library(ggplot2)
library(gplots)

EPI_dataset <- read.csv('https://aquarius.tw.rpi.edu/html/DA/EPI/EPI_data.csv')
EPI <- as.numeric(EPI_dataset$EPI)
EPI <- na.omit(EPI)
mean(EPI) #mean
mfv(EPI) #mode
median(EPI) #median
fivenum(EPI) # five num summary 

DALY <- as.numeric(EPI_dataset$DALY)
DALY <- na.omit(DALY)
mean(DALY) #mean
mfv(DALY) #mode
median(DALY) #median
fivenum(DALY) # five num summary 

#Generate frequency histograms for EPI and DALY
stem(EPI)
hist(EPI,seq(30,95,1.0),prob=TRUE)
#lines(density(EPI,na.rm=TRUE,bw=1))
lines(density(EPI,na.rm=TRUE,bw='SJ'))

stem(DALY)
hist(DALY,seq(0,100,1.0),prob=TRUE)
#lines(density(DALY,na.rm=TRUE,bw=1))
lines(density(DALY,na.rm=TRUE,bw='SJ'))

#Generate the boxplot
#?boxplot
boxplot(EPI_dataset$ENVHEALTH,EPI_dataset$ECOSYSTEM,names = c('ENVHEALTH', 'ECOSYSTEM'))

#Generate the Q-Q plot
#?qqplot
qqplot(EPI_dataset$ENVHEALTH,EPI_dataset$ECOSYSTEM,xlab = 'ENVHEALTH', ylab = 'ECOSYSTEM')

#-------------------------------------------------------
#                        Part 1b                        |
#-------------------------------------------------------
#Find the most important feature by region
table(EPI_dataset$EPI_regions)
EPI_SSA <- filter(EPI_dataset,EPI_regions == 'Sub-Saharan Africa')
EPI_SSA <- EPI_dataset[, unlist(lapply(EPI_SSA, is.numeric))]
EPI_SSA<-na.omit(EPI_SSA)
EPI_cor <- data.frame(cor(EPI_SSA,EPI_SSA$EPI))
EPI_cor <- na.omit(EPI_cor)

EPI_cor <- cbind(newColName = rownames(EPI_cor), EPI_cor)
rownames(EPI_cor) <- 1:nrow(EPI_cor)
colnames(EPI_cor) <- c('variable name','correlation_with_EPI')
EPI_cor <- EPI_cor %>% arrange(correlation_with_EPI) 
View(EPI_cor)

#Linear and Least-Square
boxplot(EPI_dataset$ENVHEALTH,EPI_dataset$DALY,EPI_dataset$AIR_H,EPI_dataset$WATER_H,names = c('ENVHEALTH','DALY','AIR_H','WATER_H'))
lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H, data = EPI_dataset)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)
lmENVH

DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)
colnames(NEW) <- c('DALY','AIR_H','WATER_H')

pENV <- lmENVH %>% predict(NEW, interval = 'prediction')
cENV <- lmENVH %>% predict(NEW, interval = 'confidence')
View(pENV)