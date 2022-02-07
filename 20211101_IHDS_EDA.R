##############################################################
##############################################################
#----------------------------
# Script Purpose:
# Exploratory Statistics of daughters-in-law in India
#----------------------------
# Data Source:
# https://ihds.umd.edu/
#----------------------------
# Author: Chandana RB, 2021-Nov-01
#----------------------------
##############################################################
##############################################################


## Clear the Environment
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
summary(df_merged$RO7)

## Filter the dataframe for the daughter-in-law data
##############################################################
# Filters:
# 1. Gender is Female             ::  RO3  = (2) Female 2
# 2. Married Status is true       ::  R06 != Unmarried 2
# 3. Relationship is Child-in-Law ::  RO4  = (04) Child-in-Law 4
# 4. Age is greater than 14       ::  RO5 >= 15
##############################################################
df_DIN = subset(df_merged, RO3 == "(2) Female 2" & RO6 != "Unmarried 2" & RO6 != "-" & RO4 == "(04) Child-in-Law 4" & RO5 >= 15)

summary(df_DIN$RO7)

## Plot statistics - 1 || Classical Plotting
##############################################################
# Plot:
# Distribution of age of the daughters-in-law
# Important Variables:
# 1. Age                         ::  RO5
##############################################################
age = df_DIN$RO5
summary(df_DIN$RO5)

jpeg("D:/71_Chandana/2011_12/IHDS_DataAnalysis/DINsAge.jpg", width = 1000, height = 600)

titlestr = paste("Age Distribution of", length(age),"daughters-in-law in India")
barplot(table(age),
        main=titlestr,
        xlab="Age",
        ylab="Count",
        border="black",
        col="black",
        density=100
)
dev.off()


## Plot and Statistics - 2 || Usage of advanced ggplot2 library and ANOVA
##############################################################
# Plot:
# Relationship of perCapita Household income and average years of education of the daughters-in-law
# The analysis differentiates between 3 age catergories [18:35), [35:50) and [50:65)
# Important Variables:
# 1. Education Category          ::  EDUC7
# 2. Household percapita Income  ::  INCOMEPC.x
# 3. Age                         ::  RO5
# 4. Household Unique ID         ::  IDHH
##############################################################
df_imp = df_DIN[, c('INCOMEPC.y', 'EDUC7', 'RO5', 'IDHH')]

min_Age = 50
max_Age = 65

df_imp = subset(df_imp, EDUC7 != "NA" & RO5 >= min_Age & RO5 < max_Age)

df_imp$EDUC7_Years = sapply(df_imp$EDUC7, switch,
                            "(00) none 0" = 0,
                            "(03) 1-4 3"  = 3,
                            "(05) primary 5" = 5,
                            "(08) 6-9 8" = 8,
                            "(10) Secondary(&11) 10" = 10,
                            "(12) Higher sec(&13,14) 12" = 12,
                            "(15) graduate 15" = 15,
                            "(16) some post-grad 16" = 16)

df_imp_agg = aggregate(cbind(EDUC7_Years,INCOMEPC.y) ~ IDHH, df_imp, mean)
class_YoEdu = c(0, 3, 5, 8, 10, 12, 15, 16)
df_imp_agg$EDUC7_Years_class = sapply(df_imp_agg$EDUC7_Years, function(x) class_YoEdu[order(abs(x - class_YoEdu))][1])

df_imp_agg$EDUC7_Years_char = as.character(df_imp_agg$EDUC7_Years_class)
df_imp_agg$EDUC7_Years_char <- factor(df_imp_agg$EDUC7_Years_char, levels = c(0, 3, 5, 8, 10, 12, 15, 16))  


# Plot using BoxPlot and save the file
library(ggplot2)
jpeg_name = paste("D:/71_Chandana/2011_12/IHDS_DataAnalysis/DINs_HouseholdIncome_Education_AgeGroup", min_Age, "to", max_Age, ".jpg", sep = "")
jpeg(jpeg_name, width = 1500, height = 700)
titlestr = paste("Relationship between Household PerCapita Income and Average Daughters-in-law Education, ", length(df_imp$EDUC7_Years),"daughters-in-law, Age Group:", min_Age, "to", max_Age)
ggplot(df_imp_agg, aes(x=EDUC7_Years_char, y=INCOMEPC.y)) +
  geom_boxplot(fill="cyan")+
  labs(title=titlestr,x="Avg. Years of Education of daughters-in-law", y = "Household PerCapita Income")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 150000)
dev.off()

# Run an ANOVA
res_aov <- aov(EDUC7_Years_class ~ INCOMEPC.y, data = df_imp_agg)
summ = summary(res_aov)
jpeg_name = paste("D:/71_Chandana/2011_12/IHDS_DataAnalysis/DINs_HouseholdIncome_Education_AgeGroup", min_Age, "to", max_Age, "_ANOVA.txt", sep = "")
capture.output(summ, file = jpeg_name)


## Plot and Statistics - 3 || Usage of advanced ggplot2 library and ANOVA
##############################################################
# Plot:
# Relationship of perCapita Household income and average years of education of the daughters-in-law
# The analysis differentiates between 3 age catergories [18:35), [35:50) and [50:65)
# The analysis is done only for the daughters-in-law whose occupation is Housework
# Important Variables:
# 1. Education Category          ::  EDUC7
# 2. Household percapita Income  ::  INCOMEPC.x
# 3. Age                         ::  RO5
# 4. Household Unique ID         ::  IDHH
# 5. Occupation Category         ::  RO7
##############################################################
df_imp = df_DIN[, c('INCOMEPC.y', 'EDUC7', 'RO5', 'IDHH', 'RO7')]

df_imp = subset(df_imp, EDUC7 != "NA" & RO5 >= min_Age & RO5 < max_Age & RO7 == "(11) Housework 11")

df_imp$EDUC7_Years = sapply(df_imp$EDUC7, switch,
                            "(00) none 0" = 0,
                            "(03) 1-4 3"  = 3,
                            "(05) primary 5" = 5,
                            "(08) 6-9 8" = 8,
                            "(10) Secondary(&11) 10" = 10,
                            "(12) Higher sec(&13,14) 12" = 12,
                            "(15) graduate 15" = 15,
                            "(16) some post-grad 16" = 16)

df_imp_agg = aggregate(cbind(EDUC7_Years,INCOMEPC.y) ~ IDHH, df_imp, mean)
class_YoEdu = c(0, 3, 5, 8, 10, 12, 15, 16)
df_imp_agg$EDUC7_Years_class = sapply(df_imp_agg$EDUC7_Years, function(x) class_YoEdu[order(abs(x - class_YoEdu))][1])

df_imp_agg$EDUC7_Years_char = as.character(df_imp_agg$EDUC7_Years_class)
df_imp_agg$EDUC7_Years_char <- factor(df_imp_agg$EDUC7_Years_char, levels = c(0, 3, 5, 8, 10, 12, 15, 16))  


## Plot using BoxPlot and save the file
library(ggplot2)
jpeg_name = paste("D:/71_Chandana/2011_12/IHDS_DataAnalysis/DINs_HouseholdIncome_Education_HOUSEWORK_AgeGroup", min_Age, "to", max_Age, ".jpg", sep = "")
jpeg(jpeg_name, width = 1500, height = 700)
titlestr = paste("Relationship between Household PerCapita Income and Average Daughters-in-law Education, ", length(df_imp$EDUC7_Years),"daughters-in-law, Age Group:", min_Age, "to", max_Age, ", HOUSEWORK ONLY")
ggplot(df_imp_agg, aes(x=EDUC7_Years_char, y=INCOMEPC.y)) +
  geom_boxplot(fill="cyan")+
  labs(title=titlestr,x="Avg. Years of Education of daughters-in-law", y = "Household PerCapita Income")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 150000)
dev.off()

## Run an ANOVA
res_aov <- aov(EDUC7_Years_class ~ INCOMEPC.y, data = df_imp_agg)
summ = summary(res_aov)
jpeg_name = paste("D:/71_Chandana/2011_12/IHDS_DataAnalysis/DINs_HouseholdIncome_Education_HOUSEWORK_AgeGroup", min_Age, "to", max_Age, "_ANOVA.txt", sep = "")
capture.output(summ, file = jpeg_name)
