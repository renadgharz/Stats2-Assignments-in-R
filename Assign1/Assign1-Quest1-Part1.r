#Loading libraries
library(readxl)

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
realestate_boxplot <- boxplot(realestate$`Living Area [sq ft]`)


#1.1.b



#1.1.c



#1.1.d



#1.1.e



#1.1.f

