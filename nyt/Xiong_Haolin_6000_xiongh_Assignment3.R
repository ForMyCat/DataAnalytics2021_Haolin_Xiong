library(dplyr)

nyt5 <- read.csv('nyt5.csv', header=T)
nyt10 <- read.csv('nyt10.csv', header=T)
nyt15 <- read.csv('nyt15.csv', header=T)
nyt20 <- read.csv('nyt20.csv', header=T)
nyt25 <- read.csv('nyt25.csv', header=T)

nyt5 <- filter(nyt5, nyt5$Age != 0)
nyt10 <- filter(nyt10, nyt10$Age != 0)
nyt15 <- filter(nyt15, nyt15$Age != 0)
nyt20 <- filter(nyt20, nyt20$Age != 0)
nyt25 <- filter(nyt25, nyt25$Age != 0)

#a)
boxplot(nyt5$Age, nyt10$Age, nyt15$Age, nyt20$Age, nyt25$Age,main="Age",names = c('nyt5','nyt10','nyt15','nyt20','nyt25'))

boxplot(nyt5$Impressions, nyt10$Impressions, nyt15$Impressions, nyt20$Impressions, nyt25$Impressions 
        ,main="Impressions",names = c('nyt5','nyt10','nyt15','nyt20','nyt25'))

#b)
hist(nyt5$Age,breaks = 20)
hist(nyt10$Age,breaks = 20)
hist(nyt15$Age,breaks = 20)
hist(nyt20$Age,breaks = 20)
hist(nyt25$Age,breaks = 20)

hist(nyt5$Impressions,breaks = 15)
hist(nyt10$Impressions,breaks = 15)
hist(nyt15$Impressions,breaks = 15)
hist(nyt20$Impressions,breaks = 15)
hist(nyt25$Impressions,breaks = 15)

norm_dist <- rnorm(n = length(nyt5$Age), mean = 0, sd = 1)
poisson_dist8 <- rpois(n = length(nyt5$Age), 8)
poisson_dist4 <- rpois(n = length(nyt5$Impressions), 4)
hist(poisson_dist8)
hist(poisson_dist4)



#c)
plot(ecdf(nyt5$Age), do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt5$Impressions), do.points=FALSE, verticals=TRUE)

qqplot(nyt5$Age, poisson_dist8)
qqplot(nyt5$Impressions, poisson_dist4)

# h0: Age between nyt5 and nyt10 have equal means.
# Since p-value 0.010506 > 0.05, we fail to reject the null hypothesis. 
# Therefore, age between nyt5 and nyt10 have equal means.
f_test <- var.test(lm(nyt5$Clicks ~ nyt5$Impressions),lm(nyt5$Clicks ~ 1) ,alternative = "two.sided")
f_test
summary(f_test)

hist(nyt5$Clicks)
hist(nyt10$Clicks)
hist(nyt15$Clicks)
hist(nyt20$Clicks)
hist(nyt25$Clicks)

plot(ecdf(nyt5$Clicks), do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt10$Clicks), do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt15$Clicks), do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt20$Clicks), do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt25$Clicks), do.points=FALSE, verticals=TRUE)

qqplot(nyt5$Clicks, nyt5$Impressions)
qqplot(nyt10$Clicks, nyt10$Impressions)
qqplot(nyt15$Clicks, nyt15$Impressions)
qqplot(nyt20$Clicks, nyt20$Impressions)
qqplot(nyt25$Clicks, nyt25$Impressions)

# h0: Clicks between nyt5 and nyt10 have equal means.
# Since p-value 0.53 > 0.05, we fail to reject the null hypothesis. 
# Therefore, clicks between nyt5 and nyt10 have equal means.
t.test(nyt5$Clicks,nyt10$Clicks)

#Part2
nyt5_female <- filter(nyt5, nyt5$Gender == 0)
nyt10_female <- filter(nyt10, nyt10$Gender == 0)
hist(nyt5_female$Age,breaks = 20)
hist(nyt10_female$Age,breaks = 20)
hist(nyt5_female$Impressions,breaks = 15)
hist(nyt10_female$Impressions,breaks = 15)

plot(ecdf(nyt5_female$Age), do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt5_female$Impressions), do.points=FALSE, verticals=TRUE)

poisson_dist8 <- rpois(n = length(nyt5_female$Age), 8)
poisson_dist4 <- rpois(n = length(nyt5_female$Impressions), 4)
qqplot(nyt5_female$Age, poisson_dist8)
qqplot(nyt5_female$Impressions, poisson_dist4)

nyt5_male <- filter(nyt5, nyt5$Gender == 1)
nyt5_female <- filter(nyt5, nyt5$Gender == 0)
t.test(nyt5_male$Clicks,nyt5_female$Clicks)

hist(nyt5_female$Clicks,breaks = 20)
hist(nyt5_male$Clicks,breaks = 20)
