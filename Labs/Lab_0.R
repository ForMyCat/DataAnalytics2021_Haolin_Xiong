?read.csv

df_EPI <- read.csv(file.choose())
#names(df_EPI) <- df_EPI[1,]
#df_EPI <- df_EPI[-1,]
#dim(df_EPI)
View(df_EPI)

EPI <- as.numeric(df_EPI$EPI)
fivenum(EPI,na.rm=TRUE)
summary(EPI)
boxplot(EPI)


head(df_EPI)
names(df_EPI)
summary(df_EPI)
