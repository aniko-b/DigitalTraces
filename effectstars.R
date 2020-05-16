setwd("c:/Users/balan/Documents/IPSDS/Courses/2020/Consulting/Project/Data/")

full_dat <- read.csv("C:/Users/balan/Documents/IPSDS/Courses/2020/Consulting/Project/Data/full_dat.csv")
View(full_dat)

library(nnet)
library(EffectStars2)
library(graphics)



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

##Here are the graphics Helmut wanted

#graphical formatting
ctrl <- star.ctrl(lwd.circle = 3, col.circle = "lightblue", 
                  lty.circle = 5, col.fill = "lightgrey", lwd.star = 1.8,
                  cex.main = 1.5, cex.labels = 1.2, col.main = "black",
                  col.labels = "black", col.star = "black", dist.labels = 1.1, 
                  font.labels = 1, radius = 1)


#add p-values for each of the coefficients
summary(mod_pol)
z <- summary(mod_pol)$coefficients/summary(mod_pol)$standard.errors #z calculation for the regression coefficients
res[[2]] <- z
p <- (1 - pnorm(abs(z), 0, 1)) * 2 #2-tailed z test
head(p)


# create labels containing the response categories and all p-values
p_values <- formatC(p, format="f", digits=3)
labels <- matrix(paste0(rep(c("Search", "Social", "Unknown"), nrow(e_star_risk_ratio)), "\n(", p_values, ")"),
                 byrow = T, ncol = 3)
head(labels)
head(e_star_risk_ratio)


#plot effectstars

effectstars(e_star_risk_ratio,
            names = c("Intercept", "Registered voters", "Voted", rep("Changed mind", 3), 
                      "Undecided", "Polinterest", "Leftmidright", "Trust in EP", "Trust in NP"), #name of the star
            subs = c("", rep("(yes)", 2),  "(did not change)", "(did not vote)", "(doesn't remember)", "(yes)" , rep("", 4)), 
            #category labels of the predictors 
            labels = labels, #dependent variable categories
            control = ctrl) #graphic above
          


            









