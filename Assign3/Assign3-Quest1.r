#Loading libraries
library(readxl)
library(RColorBrewer)
library(DescTools)
library(dplyr)

#Loading data
scores <- read_xlsx("./Assign3/Assign3Data.xlsx", sheet = "Midterm Scores")

#1.a
##SBS Boxplots
scores_boxplots <- boxplot(scores,main = "Scores by Professor",ylab = "Scores",
                           col = brewer.pal(3,"Accent"))

#1.b
##No coding required

#1.c
##Sample variances
scores_who_var <- 
  round(var(scores$Who), 3) # Prof Who
scores_jones_var <- 
  round(var(scores$Jones), 3) # Prof Jones
scores_x_var <- 
  round(var(scores$X), 3) # Prof X

##Pooled variance
scores_pvar <- round(mean(c(
  scores_who_var,
  scores_jones_var,
  scores_x_var)), 2)

#1.d
##No coding required

#1.e
##Converting to long format
scores_long <- gather(
  scores, Professors, Scores)

##1 way ANOVA
scores_anova <- aov(
  scores_long$Scores~scores_long$Professors, 
  scores)

##Critical value
scores_cv <- round(qf(
  0.05, 
  scores_anova$parameter[1], 
  scores_anova$parameter[2], 
  lower.tail = FALSE), 3)

##Validating test
round(scores_anova$statistic,3) > scores_cv

#1.f
##Bonferroni pairwise CIs
scores_bonf_ci <- PostHocTest(
  scores_anova, 
  method = "bonferroni", 
  conf.level = 0.95)


#1.g
##Kruskal-Wallis test
scores_kw_test <- kruskal.test(
  scores_long$Scores~
    scores_long$Professors, 
  scores)

##Critical value
scores_kw_cv <- round(qchisq(
  0.05,
  scores_kw_test$parameter, 
  lower.tail = FALSE), 3)

##Validating the test
scores_kw_test$statistic > scores_kw_cv
