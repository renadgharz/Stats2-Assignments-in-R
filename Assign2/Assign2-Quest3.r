#Loading libraries
library(readxl)
library(dplyr)

#Loading data
activity <- read_excel('./Assign2/Assign2Data.xlsx', sheet = 'Activity')

#3.a
#No coding required

#3.b
##Plotting histogram (lean)
activity_histogram <- 
  hist(activity$`Lean Subject`,
       main = "Histogram of Lean Subject",
       xlab = "Minutes standing or walking",
       xlim = c(0, 1000),
       breaks = 10,
       prob = TRUE,
       col = "light blue")

##Mean of lean subject
lean_mean <- mean(na.omit(activity$`Lean Subject`))

##SD of lean subject
lean_sd <- sd(activity$`Obese Subject`)

##Adding normal curve to histogram
curve(dnorm(
  x,
  mean = lean_mean,
  sd = lean_sd), 
  add = TRUE)

##SBS Boxplots
activity_boxplot <- 
  boxplot(activity,
          main = "Boxplot of Activity",
          ylab = "Minutes Standing or Walking",
          col = "light blue")

#3.c
##Wilcoxon Rank-Sum Test
a2_q3_ht <- wilcox.test(
  as.numeric(unlist(activity$`Lean Subject`)), 
  as.numeric(unlist(activity$`Obese Subject`)), 
  alternative = "two.sided", 
  conf.level = 0.95, 
  paired = FALSE, 
  exact = FALSE, 
  na.rm = TRUE)

##Validating test
a2_q3_ht$p.value < 0.05
