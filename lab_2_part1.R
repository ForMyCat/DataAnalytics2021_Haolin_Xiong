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
str(EPI_dataset)
EPI_subset <- subset(EPI_dataset, select = c(-Desert, -code, -ISO3V10, -Country, -GEO_subregion, -No_surface_water, -ACSAT_pt_imp, -WATSUP_pt_imp))
EPI_EU <- filter(EPI_subset,EPI_regions == 'Europe')
EPI_EU <- subset(EPI_EU, select = c(-CO2KWH_pt_imp ,-EPI_regions))

EPI_EU_scaled <- data.frame(lapply(EPI_EU, function(x){
  if(typeof(x) != "integer"){
    return(scale(x))
  } else{
    return(x)
  }
}))

lm(EPI ~ ., data = EPI_EU_scaled)


fitted_models <- EPI_subset %>% do(model = lm(EPI ~ . - EPI_regions, data = EPI_subset))


#summarize(EPI_subset, cor(EPI, Landlock))

#EPI_matrix <- as.matrix(EPI_subset)
#heatmap(RPI_matrix)

fitted_models <- EPI_subset %>% do(model = lm(EPI ~ . - EPI_regions, data = EPI_subset))
fitted_models$model

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