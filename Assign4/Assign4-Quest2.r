#Loading libraries
library(readxl)
library(car)

#Loading data
prof_wages <- read_xlsx("./Assign4/Assign4Data.xlsx", sheet = "Professor Wages")

##Removing useless obs column
prof_wages$obs <- NULL

#2.a
##Scatterplot matrix
plot(prof_wages, 
     cex = 2, 
     pch =2, 
     col = "dark blue",
     main = "Scatterplot matrix (wage)")

#2.b
##Transforming data
prof_wages$lwage <- log(prof_wages$wage,10)
##Removing wage column
prof_wages$wage <- NULL
##Scatterplot matrix
plot(prof_wages, 
     cex = 2, 
     pch =2, 
     col = "dark blue",
     main = "Scatterplot matrix (lwage)")
##Correlation matrix
prof_wages_corr_mat <- 
  round(cor(prof_wages),4)

#2.c
##Regression model
prof_wages_lm <- 
  lm(lwage~educ+exper+tenure, prof_wages)

##Regression coefficients
prof_wage_lm_coeffs <- 
  summary(prof_wages_lm)$coefficients[,1]

##Calculating model VIF
prof_wages_lm_vif <- 
  round(vif(prof_wages_lm),3)

#2.d
##New regression model
prof_wages_lm_new <- 
  lm(lwage~educ+tenure, prof_wages)

##Regression coefficients
prof_wages_lm_new_coeffs <- 
  summary(prof_wages_lm_new)$coefficients[,1]

#2.e
##Q-Q plot
qqnorm(prof_wages_lm_new$residuals, 
       datax = TRUE)
qqline(prof_wages_lm_new$residuals, 
       datax = TRUE)

##Residuals vs fitted values
plot(
  fitted(prof_wages_lm_new), 
  resid(prof_wages_lm_new),
  main = "Residuals vs Fitted Values",
  xlab = "Fitted Values",
  ylab = "Residuals")
##Adding mean line
abline(0,0)

#2.f



#2.g



#2.h



#2.i


