setwd("c:/Users/balan/Documents/IPSDS/Courses/2020/Consulting/Project/Data/")

full_dat <- read.csv("C:/Users/balan/Documents/IPSDS/Courses/2020/Consulting/Project/Data/full_dat.csv")
View(full_dat)

library(nnet)
library(EffectStars2)
library(graphics)



full_dat[,c("cluster", "undecided", "polinterest")] <- as.data.frame(apply(full_dat[,c("cluster", "undecided", "polinterest")], 2, factor))
full_dat[,which(sapply(full_dat, is.character))] <- as.data.frame(apply(full_dat[,which(sapply(full_dat, is.character))], 2, factor))
str(full_dat$cluster)

#ISCED: change from class integer to factor

full_dat$ISCED <- as.factor(full_dat$ISCED)
class(full_dat$ISCED)

# Splitting the data per countries
countries <- split(full_dat, full_dat$country)
France <- countries$France
UK <- countries$UK
Germany <- countries$Germany

#Setting the reference category of the response variable for the multinomial logistic regression
full_dat$cluster <- relevel(full_dat$cluster, ref = "4") # it is 'unknown' now.


##Political variables

##Political explanatory variables
formula_pol <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num +
                            trust.EP + trust.nat.pol)
mod_pol <- multinom(formula_pol, data = full_dat,
                    
                    na.action = na.omit) # omit missing observations
# Political explanatory variables per countries
mod_pol_France <- multinom(formula_pol, data = France)
mod_pol_UK <- multinom(formula_pol, data = UK)
mod_pol_Germany <- multinom(formula_pol, data = Germany)
 


#Function to prepare political effectstars

effectstars_pol <- function(model)
{
    risk_ratio <- exp(coef(model)) #Get the Beta coefficients (risk ratios)   
  e_star_risk_ratio <- t(risk_ratio) #Transpose the risk ratio matrix in order to get the right format to feed into Effectstars2
  
  z <- summary(model)$coefficients/summary(model)$standard.errors #z calculation for the regression coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2 #2-tailed z test
  p_values <- formatC(p, format="f", digits=3) #format p
  labels <- matrix(paste0(rep(c("Routine", "Search", "Social"), nrow(e_star_risk_ratio)), "\n(", p_values, ")"),
                   byrow = T, ncol = 3) #create labels containing the response categories and all p-values
  ctrl <- star.ctrl(lwd.circle = 3, col.circle = "lightblue", #graphical formatting
                    lty.circle = 5, col.fill = "lightgrey", lwd.star = 1.8,
                    cex.main = 1.5, cex.labels = 1.2, col.main = "black",
                    col.labels = "black", col.star = "black", dist.labels = 1.1, 
                    font.labels = 1, radius = 1)
  effectstars(e_star_risk_ratio,
              names = c("Intercept", "Registered voters", "Voted", rep("Changed mind", 3), 
                        "Undecided", "Polinterest", "Leftmidright", "Trust in EP", "Trust in NP"), #name of the star
              subs = c("", rep("(yes)", 2),  "(did not change)", "(did not vote)", "(doesn't remember)", "(yes)" , rep("", 4)), 
              #category labels of the predictors 
              labels = labels, #dependent variable categories
              control = ctrl) #graphic above
  
 
}

#Plot political effectstars

effectstars_pol(mod_pol)

effectstars_pol(mod_pol_France)

effectstars_pol(mod_pol_Germany)

effectstars_pol(mod_pol_UK)




##Political and sociodemographic variables



# Political and sociodemographical explanatory variables
formula_pol_socio <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num + trust.EP +
                                  trust.nat.pol + gender + age_num + children + income + family + ISCED)

mod_pol_socio <- multinom(formula_pol_socio, data = full_dat, 
                          na.action = na.omit, # omit missing observations
                          Hess = TRUE) #get Hessian observed/expected information matrix


# Political and sociodemographical explanatory variables per countries
mod_pol_socio_France <- multinom(formula_pol_socio, data = France)
mod_pol_socio_UK <- multinom(formula_pol_socio, data = UK)
mod_pol_socio_Germany <- multinom(formula_pol_socio, data = Germany)
mod_pol_socio


#Function to prepare political and sociodemographic effectstars

effectstars_pol_socio <- function(model)
{
  risk_ratio <- exp(coef(model)) #Get the Beta coefficients (risk ratios)   
  e_star_risk_ratio <- t(risk_ratio) #Transpose the risk ratio matrix in order to get the right format to feed into Effectstars2
  
  z <- summary(model)$coefficients/summary(model)$standard.errors #z calculation for the regression coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2 #2-tailed z test
  p_values <- formatC(p, format="f", digits=3) #format p
  labels <- matrix(paste0(rep(c("Routine", "Search", "Social"), nrow(e_star_risk_ratio)), "\n(", p_values, ")"),
                   byrow = T, ncol = 3) #create labels containing the response categories and all p-values
  ctrl <- star.ctrl(lwd.circle = 3, col.circle = "lightblue", #graphical formatting
                    lty.circle = 5, col.fill = "lightgrey", lwd.star = 1.8,
                    cex.main = 1, cex.labels = 1, col.main = "black",
                    col.labels = "black", col.star = "black", dist.labels = 1.1, 
                    font.labels = 1, radius = 1)
 
  effectstars(e_star_risk_ratio,
              names = c("Intercept",   #name of the star 
                        "Registered voters", 
                        "Voted", 
                        rep("Changed mind", 3), 
                        "Undecided", 
                        "Polinterest", 
                        "Leftmidright", 
                        "Trust in EP", 
                        "Trust in NP", 
                        "Gender", 
                        "Age", 
                        rep("Chidren", 3), 
                        rep("Income", 7), 
                        rep("Family", 5), 
                        rep("ISCED", 3)),
              subs = c("", rep("(yes)", 2),  "(did not change)", "(did not vote)", "(doesn't remember)", "(yes)" , rep("", 4), "(male)",
                       "", "(2)", "(3+)", "(No)","(500-1000)", "(1000-1500)", "(1500-2000)", "(2000-2500)", "(2500+)" ,"(no income)", "(NA)",
                       "(divorced with partner)", "(divorced w/o partner)", "(married)", "(single with partner)", "(single w/o partner)", 
                       "(ISCED3)", "(ISCED4)", "(ISCED8)"), 
              #category labels of the predictors 
              labels = labels, #dependent variable categories
              
              control = ctrl) #graphic above
              
}


#Plot political and sociodemographic effectstars

effectstars_pol_socio(mod_pol_socio)

effectstars_pol_socio(mod_pol_socio_France)

effectstars_pol_socio(mod_pol_socio_Germany)

effectstars_pol_socio(mod_pol_socio_UK)
















