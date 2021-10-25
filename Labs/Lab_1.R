EPI_Data <- read.csv(file='2010EPI_data.csv',skip=1,header=T)

EPI_Data_xls_all_coutries <- read_excel("2010EPI_data.xls", sheet = "EPI2010_all countries")
EPI_Data_xls_onlyEPI <- read_excel("2010EPI_data.xls", sheet = "EPI2010_onlyEPIcountries")


EPI <- as.numeric(EPI_Data$EPI)
EPI<- na.omit(EPI)
summary(EPI,na.rm=TRUE)
fivenum(EPI,na.rm=TRUE)

stem(EPI)
hist(EPI,seq(30,95,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1))
lines(density(EPI,na.rm=TRUE,bw='SJ'))

#Ex1. Fitting a distribution beyong histograms
plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)

par(pty='s')
qqnorm(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for tdsn')
qqline(EPI)

help(ppoints)
help(qqplot)

#Ex2. Filtering
Landlock <- na.omit(EPI_Data$Landlock)
Landlock
EPILand <- EPI_Data[!EPI_Data$Landlock]
(na.omit(EPI_Data$Landlock)
EPILEPI_Data$Landlock 

