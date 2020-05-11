setwd("c:/Users/balan/Documents/IPSDS/Courses/2020/Consulting/Project/Data/")

full_dat <- read.csv("C:/Users/balan/Documents/IPSDS/Courses/2020/Consulting/Project/Data/full_dat.csv")
View(full_dat)

library(nnet)
library(EffectStars2)



full_dat[,c("cluster", "undecided", "polinterest")] <- as.data.frame(apply(full_dat[,c("cluster", "undecided", "polinterest")], 2, factor))
full_dat[,which(sapply(full_dat, is.character))] <- as.data.frame(apply(full_dat[,which(sapply(full_dat, is.character))], 2, factor))
str(full_dat$cluster)

# Splitting the data per countries
countries <- split(full_dat, full_dat$country)
France <- countries$France
UK <- countries$UK
Germany <- countries$Germany

#Setting the reference category of the response variable for the multinomial logistic regression
full_dat$cluster <- relevel(full_dat$cluster, ref = "1") # We can later change the reference cluster (now it is routine)


##Running the multinomial logistic regresssion

#Political explanatory variables
formula_pol <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num +
                            trust.EP + trust.nat.pol)
mod_pol <- multinom(formula_pol, data = full_dat,
                    na.action = na.omit) # omit missing observations
 
#Getting the Beta coefficients (risk ratios)           
coef(mod_pol)
risk_ratio <- exp(coef(mod_pol))
risk_ratio
str(risk_ratio)

#Transpose the risk ratio matrix in order to get the right format to feed into Effectstars2
e_star_risk_ratio <- t(risk_ratio)
e_star_risk_ratio

#Here are the graphics Helmut wanted

effectstars(e_star_risk_ratio)
