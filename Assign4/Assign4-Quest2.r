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
##Removing exper variable
prof_wages$exper <- NULL
##Complete model
prof_wages_lm_whole <- 
  lm(lwage~1, prof_wages)

##Test statistic - Model
prof_wages_lm_stat <- 
  summary(prof_wages_lm_new)$fstatistic[1]

##Critical value
prof_wages_lm_crit <-
  qf(0.05, 
     summary(prof_wages_lm_new)$fstatistic[2], 
     summary(prof_wages_lm_new)$fstatistic[3], 
     lower.tail = FALSE)

##Validating test
prof_wages_lm_stat > prof_wages_lm_crit

##Crit value for coefficients


##Test statistic - educ
prof_wages_lm_educ <- round(summary(
  prof_wages_lm_new)$coefficients[2,3],3)

##Crit value for coefficients
prof_wages_lm_coeff_crit <- round(qt(
  0.05/2,
  summary(prof_wages_lm_new)$df[2], 
  lower.tail = FALSE),3)

##Validating test
prof_wages_lm_educ > prof_wages_lm_coeff_crit

##Test statistic - tenure
prof_wages_lm_tenure <- round(summary(
  prof_wages_lm_new)$coefficients[3,3],3)

##Validating test
prof_wages_lm_tenure > prof_wages_lm_coeff_crit

#2.g
##No coding required

#2.h
##SE fit
prof_wages_lm_se_fit <- predict(
  prof_wages_lm_new, 
  data.frame(educ=13,
             tenure=15), 
  se.fit = TRUE)

##Confidence interval
prof_wages_lm_ci <- round(predict(
  prof_wages_lm_new, 
  data.frame(educ=13,
             tenure=15), 
  interval = "confidence"), 4)

##Prediction interval
prof_wages_lm_pi <- round(predict(
  prof_wages_lm_new, 
  data.frame(educ=13,
             tenure=15), 
  interval = "prediction"), 4)
