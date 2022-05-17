#Loading libraries
library(readxl)
library(RColorBrewer)
library(DescTools)

#Loading data
exec_sals <- read_xlsx("./Assign4/Assign4Data.xlsx", sheet = "Executives Salaries")

##Removing empty column
exec_sals$...4 <- NULL

#1.a
##Boxplot
boxplot(
  exec_sals$Salary_raw,
  main = "Boxplot of Salaraies (Unadjusted)",
  ylab = "Salaries (Unadjusted, in $)",
  col = brewer.pal(1, "PRGn"))

##Scatterplot
plot(
  exec_sals$ROG_raw, 
  exec_sals$Salary_raw,
  main = 
    "Salaries vs Rate of Growth (Unadjusted)",
  xlab = "Rate of Growth (Unadjusted, %)",
  ylab = "Salaries (Unadjusted, $)")


#1.b
##Linear regression model
sals_lm <- lm(
  Salary_raw~
    ROG_raw,
  exec_sals)

##Regression coefficients
sals_lm_coeffs <- round(
  sals_lm[["coefficients"]],3)

#1.c
##Residuals vs fitted values plot
plot(
  fitted(sals_lm), 
  resid(sals_lm),
  main = "Residuals vs Fitted Values",
  xlab = "Fitted Values",
  ylab = "Residuals")

##Adding mean line
abline(0,0)

##Q-Q plot
qqnorm(sals_lm$residuals, 
       datax = TRUE) #Data
qqline(sals_lm$residuals, 
       datax = TRUE) #QQ line

#1.d
##Linear regression model
sals_adj_lm <- lm(
  Salary_adj~
    ROG_adj,
  exec_sals)

##Regression coefficients
sals_adj_lm_coeffs <- round(
  sals_adj_lm[["coefficients"]],3)

#1.e
##Histogram of residuals
hist(sals_adj_lm$residuals,
     breaks = 20,
     xlim = c(-200,250),
     main = "Histogram of Residuals",
     xlab = "Residuals")

##Q-Q plot
qqnorm(sals_adj_lm$residuals, 
       datax = TRUE) #Data
qqline(sals_adj_lm$residuals, 
       datax = TRUE) #QQ line

#1.f
##No coding required

#1.g
##R-squared of model
sals_adj_rsq <-round(summary(
  sals_adj_lm)$r.squared,4)


#1.h
##CI critical value
sals_adj_lm_ci_cv <- round(qt(
  0.05/2,
  sals_adj_lm$df.residual,
  lower.tail = FALSE),3)

##Coefficient CI
sals_adj_lm_coeff__ci <- confint(
  sals_adj_lm,level = 0.95)[2,]


#1.i
##CI given ROG of 20
exec_sals_mean_given_20 <- predict(
  sals_adj_lm, 
  data.frame(ROG_adj = 20), 
  interval = "confidence")

