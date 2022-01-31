##############################################################
##############################################################
#----------------------------
# Script Purpose:
# Exploratory Statistics of daughter-in-laws in India
#----------------------------
# Data Source:
# https://ihds.umd.edu/
#----------------------------
# Author: Chandana RB, 2021-Nov-01
#----------------------------
##############################################################
##############################################################


## Clear Environment
rm(list=ls())

## Load Individual Survey data
load(file="D:/71_Chandana/2011_12/ICPSR_36151-V6/DS0001/36151-0001-Data.rda")
## Load Household Survey data
load(file="D:/71_Chandana/2011_12/ICPSR_36151-V6/DS0002/36151-0002-Data.rda")

## Merge household survey data with Individual survey data into a large dataframe
df_merged = merge(da36151.0001, da36151.0002, by = "IDHH")

## Find the new names of the columns of the merged dataframe
print(colnames(df_merged))

## Summary of certian column in the merged dataframe
summary(df_merged$RO5)

## Filter the dataframe for the daughter-in-law data
##############################################################
# Filters:
# 1. Gender is Female             ::  RO3  = (2) Female 2
# 2. Married Status is true       ::  R06 != Unmarried 2
# 3. Relationship is Child-in-Law ::  RO4  = (04) Child-in-Law 4
# 4. Age is greater than 14       ::  RO5 >= 15
##############################################################
df_DIN = subset(df_merged, RO3 == "(2) Female 2" & RO6 != "Unmarried 2" & RO6 != "-" & RO4 == "(04) Child-in-Law 4" & RO5 >= 15)


## Plot statistics - 1 || Classic Plotting
##############################################################
# Plot:
# Distribution of age of the daughter-in-laws
# Important Variables:
# 1. Age                         ::  RO5
##############################################################
age = df_DIN$RO5

jpeg("D:/71_Chandana/2011_12/IHDS_DataAnalysis/DINsAge.jpg", width = 1000, height = 600)

titlestr = paste("Age Distribution of", length(age),"Daughter-in-laws in India")
barplot(table(age),
        main=titlestr,
        xlab="Age",
        ylab="Count",
        border="black",
        col="black",
        density=100
)

dev.off()


## Plot statistics - 2 || Usage of advanced ggplot2 library
##############################################################
# Plot:
# Relationship of income to corresponding age of the daughter-in-laws
# Important Variables:
# 1. Education Category          ::  EDUC7
# 2. Household percapita Income  ::  INCOMEPC.x
##############################################################
df_imp2 = df_DIN[, c('INCOMEPC.y', 'EDUC7')]

library(ggplot2)
jpeg("D:/71_Chandana/2011_12/IHDS_DataAnalysis/DINsHouseholdIncome_v_DINsEducation.jpg", width = 1500, height = 700)

titlestr = paste("Household PerCapita Income and Daughter-in-law Education Relationship of", length(age),"Daughter-in-laws")
ggplot(df_imp2, aes(x=EDUC7, y=INCOMEPC.y)) +
  geom_boxplot(fill="cyan")+
  labs(title=titlestr,x="Age", y = "Household PerCapita Income")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 150000)

dev.off()
