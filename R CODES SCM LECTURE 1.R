getwd()
#To set the workspace
setwd('D:\\SCMA 2024\\Data')
# To check the workspace
getwd()
#To read a 'csv' file into R
df_ipl = read.csv('Cricket_data.csv')
dim(df_ipl)
names(df_ipl)
head(df_ipl)
install.packages('readxl')
library(readxl)
sheet = excel_sheets('bank2022.xlsx')
sheet 
df_bank = read_excel('bank2022.xlsx',sheet='Sheet1')
head(df_bank)
install.packages('janitor')

names(df_bank)
dput(names(df_bank))

library(janitor)
df_bank = clean_names(df_bank)

dput(names(df_bank))

# To check if here are any missing values
any(is.na(df_bank))
sum(is.na(df_bank))

sum(is.na(df_bank$net_profit_loss_for_the_year))

# To detect outliers
boxplot(df_bank$deposits)

??quartile
??interquartile
quantile(df_bank$net_profit_loss_for_the_year)

IQR(df_bank$net_profit_loss_for_the_year, na.rm = FALSE, type = 7)
median(df_bank$net_profit_loss_for_the_year)

df_bank[df_bank$deposits > 3051534,]

hist(df_bank$net_profit_loss_for_the_year, prob=TRUE)
lines(density(df_bank$net_profit_loss_for_the_year, prob=TRUE))
??fitdistr
install.packages('MASS')
library(MASS)
fitdistr(df_bank$net_profit_loss_for_the_year, "normal")

np = rnorm(10000,1678.8947,7268.8598)
hist(df_bank$net_profit_loss_for_the_year, prob=TRUE)
lines(density(np))
??fitdistr
??fitdistrplus
install.packages("fitdistrplus")
library(fitdistrplus)
descdist(df_bank$net_profit_loss_for_the_year, boot = 1000)
fitdistr(np, "lognormal")
min(df_bank$net_profit_loss_for_the_year)
np = 16420.03+df_bank$net_profit_loss_for_the_year
min(np)
rlognorm(10000,9.68586105,0.80849675)
??lognormal
ln = rlnorm(10000, meanlog = 9.68586105, sdlog = 0.80849675)
plot(density(ln))

hist(df_bank$net_profit_loss_for_the_year, prob=TRUE)
lines(density(ln))