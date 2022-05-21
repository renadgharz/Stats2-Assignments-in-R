#Loading libraries
library(readxl)
library(RColorBrewer)
library(DescTools)

#Loading data
util_bill <- read_xlsx("./Assign3/Assign3Data.xlsx", sheet = "Utility Bills")

#2.a
##Calculating residuals 
util_bill_res <- resid(util_bill_anova)

##Plotting residuals
plot(fitted(
  util_bill_anova),util_bill_res,
  main = "Residuals Versus Fitted",
  xlab = "Fitted values",
  ylab = "Residuals")

##Adding mean line
abline(0,0)

#2.b
##2 way ANOVA
util_bill_anova <- aov(util_bill$`Utility Bill ($)`~util_bill$Bedroom*util_bill$City,data = util_bill)

#2.c
##Bedroom (x), City (y)
interaction.plot(
  util_bill$Bedroom, 
 util_bill$City, 
 util_bill$`Utility Bill ($)`,
 main = "Interaction Plot of Bedroom and City Factors",
 xlab = "Bedroom",
 ylab = "City",
 lwd = 5, 
 col = brewer.pal(
   4,
   "Spectral"), 
 fun = "mean",
 trace.label = "City")

##City (x), Bedroom (y)
interaction.plot(
  util_bill$City, 
  util_bill$Bedroom, 
  util_bill$`Utility Bill ($)`,
  main = "Interaction Plot of City and Bedroom Factors",
  xlab = "City",
  ylab = "Bedroom",
  lwd = 5,
  col = brewer.pal(4,"PRGn"),
  fun = "mean",
  trace.label = "Bedrooms")

#2.d
##Number of pairwise comparisons
adj_alpha <- round(
  0.05 / (2*choose(12,2)),6)

##T-value for Bonferroni CI
bonf_ci_t_val <- qt(
  adj_alpha,
  util_bill_anova[["df.residual"]])

##Bonferroni CI ME
util_bonf_me <- round(
  -bonf_ci_t_val * 
    sqrt(2480*(1/9+1/9)),2)

#2.e
##Pairwise Bonf CI
util_bonf_pairs <- as.list(PostHocTest(
  util_bill_anova, 
  method = "bonferroni",
  conf.level = 0.95, 
  digits = 4, 
  ordered = TRUE))

##Ottawa 2bd - London 2bd pairwise CI
ott_2bd_lon_2bd <- as.vector(c(row.names(
  util_bonf_pairs$`util_bill$Bedroom:util_bill$City`)[6],
  util_bonf_pairs$`util_bill$Bedroom:util_bill$City`[6,]))
