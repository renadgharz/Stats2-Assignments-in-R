#Loading libraries
library(readxl)
library(DescTools)
library(LearningStats)
library(dplyr)

#Loading data
realestate <- read_xlsx("./Assign1/Assign1Data.xlsx")

#1.1a
##Mean
realestate_mean <- round(mean(realestate$`Living Area [sq ft]`), digits = 2)
realestate_mean

##Standard deviation
realestate_sd <- round(sd(realestate$`Living Area [sq ft]`), digits = 2)
realestate_sd

##Boxplot
realestate_boxplot <- boxplot(realestate$`Living Area [sq ft]`,
                              main = "Boxplot of Living Area",
                              ylab = "Living Area (in sq ft)",
                              col = "light blue")

##Histogram
realestate_histogram <- hist(realestate$`Living Area [sq ft]`,
                             main = "Histogram of Living Area",
                             xlab = "Livigin Area (in sq ft)",
                             xlim = c(0, 5000),
                             prob = TRUE,
                             col = "light blue")
##Adding normal curve to histogram
curve(dnorm(x, 
            mean = realestate_mean, 
            sd = realestate_sd), 
      add = TRUE)


#1.1.b
##Creating new column with coded data
realestate['Coded Data'] <- 
  ifelse(realestate$`Living Area [sq ft]` >= 2400 & 
           realestate$`Living Area [sq ft]` <= 2800, 1, 0)

##Proportions of coded data

##Ideal properties population proportion
pop_ideal_props <- 
  round(
    length(which(realestate$`Coded Data` == 1)) / 
      nrow(realestate), 
    digits = 4)

##Non ideal properties population proportion
pop_non_ideal_props <- 
  round(
    length(which(realestate$`Coded Data` == 0)) / 
      nrow(realestate), 
    digits = 4)

#1.1.c
##Computing the 95% C.I. of Sample 1
sample1_mean_ci <- 
  MeanCI(realestate$`Sample 1`, 
         conf.level = 0.95, 
         na.rm = TRUE)

#1.1.d
## Creating new column for Sample 1 coded data
realestate['Sample_p'] <- 
  ifelse(realestate$`Sample 1` >= 2400 & 
           realestate$`Sample 1` <= 2800, 1, 0)

##Ideal properties sample proportion (s1)
sample1_ideal_props <- 
  round(
    length(which(realestate$`Sample_p` == 1)) / 
      length(na.omit(realestate$Sample_p)), 
    digits = 4)

##Non ideal properties sample proportion (s1)
sample1_non_ideal_props <- 
  round(
    length(which(realestate$`Sample_p` == 0)) / 
      length(na.omit(realestate$Sample_p)), 
    digits = 4)

##90% C.I. for Sample 1 (proportion)
sample1_prop_ci <- 
  round(
    proportion.CI(
      sample1_ideal_props, 
      length(na.omit(realestate$`Sample_p`)), 
      conf.level = 0.90)$CI, 
    digits = 4)

#1.1.e
##Creating the 19 random samples
samples <- 
  as.data.frame(
    replicate(19,sample(
      realestate$`Living Area [sq ft]`,40)))

#Renaming the 19 random samples
for (i in 2:20) {
  colnames(samples)[i-1] <- paste("Sample", i)
  i<- i +1
}

##Adding Sample 1 to df and shifting position
samples <- 
  cbind(
    samples, "Sample 1" = 
      na.omit(realestate$`Sample 1`)) 
samples <- 
  samples %>% select("Sample 1", everything())

#1.1.f
##No coding required
