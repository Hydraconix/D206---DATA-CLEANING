#ReadR function

library(readr)
df_raw <- read.csv('C:/Users/Hydraconix/Desktop/DATA/medical_raw_data.csv')
str(df_raw)

#Describing Variables

unique(df_raw$Area)

unique(df_raw$Timezone)

unique(df_raw$Education)

unique(df_raw$Employment)

unique(df_raw$Marital)

unique(df_raw$Gender)

unique(df_raw$ReAdmis)

#Data Cleaning Process

head(df_raw)

#Removing Irrelevant columns from data_raw

#Dplyr function

library(dplyr)

#Remove X,Caseorder,Interacion,UID

df <- df_raw[,c(6:53)]

#Remove Job and Marital

df <- select(df, c(-Job, -Marital))

#Rename misleading variable names in df

names(df)[names(df) == 'Income'] <- 'Total_Income'
names(df)[names(df) == 'Item1'] <- 'Survey_TimelyAdmin'
names(df)[names(df) == 'Item2'] <- 'Survey_TimelyTreatment'
names(df)[names(df) == 'Item3'] <- 'Survey_TimelyVisits'
names(df)[names(df) == 'Item4'] <- 'Survey_Reliability'
names(df)[names(df) == 'Item5'] <- 'Survey_Options'
names(df)[names(df) == 'Item6'] <- 'Survey_HoursTreatment'
names(df)[names(df) == 'Item7'] <- 'Survey_CourteousStaff'
names(df)[names(df) == 'Item8'] <- 'Survey_ActiveListening'


head(df)

#Set Index

number_of_rows <- dim(df)[1]
row.names(df) <- c(1:number_of_rows)

head(df)

#Changing expressions of categorical data as numeric data

#State

x <- df[order(df$State),"State"]
unique(x)

#Plyr Function

library(plyr)

new_data <- df$State
df_state_dict <- c(
  "AL" = 1, "AK" = 2, "AZ" = 3, "AR" = 4, "CA" = 5, "CO" = 6, "CT" = 7, "DE" = 8, "DC" = 9, "FL" = 10, 
  "GA" = 11, "HI" = 12, "ID" = 13, "IL" = 14, "IN" = 15, "IA" = 16, "KS" = 17, "KY" = 18, "LA" = 19, "ME" = 20, 
  "MD" = 21, "MA" = 22, "MI" = 23, "MN" = 24, "MS" = 25, "MO" = 26, "MT" = 27, "NE" = 28, "NV" = 29, "NH" = 30, 
  "NJ" = 31, "NM" = 32, "NY" = 33, "NC" = 34, "ND" = 35, "OH" = 36, "OK" = 37, "OR" = 38, "PA" = 39, "PR" = 40,
  "RI" = 41, "SC" = 42, "SD" = 43, "TN" = 44, "TX" = 45, "UT" = 46, "VT" = 47, "VA" = 48, "WA" = 49, "WV" = 50,
  "WI" = 51, "WY" = 52)
df_state_val <- revalue(x= new_data, replace = df_state_dict)
df$State <- as.numeric(df_state_val)

#Area

unique(df$Area)

new_data <- df$Area
df_area_dict <- c(
  "Rural" = 1,
  "Suburban" = 2,
  "Urban" = 3)
df_area_val <- revalue(x= new_data, replace = df_area_dict)
df$Area <- as.numeric(df_area_val)

#Timezone

unique(df$Timezone)

new_data <- df$Timezone
df_timezone_dict <- c(
  "America/Puerto_Rico" = -2,
  "America/Detroit" = -3,
  "America/Indiana/Indianapolis" = -3,
  "America/Indiana/Marengo" = -3,
  "America/Indiana/Vincennes" = -3,
  "America/Indiana/Vevay" = -3,
  "America/Indiana/Winamac" = -3,
  "America/Kentucky/Louisville" = -3,
  "America/New_York" = -3,
  "America/Toronto" = -3,
  "America/Chicago" = -4,
  "America/Indiana/Knox" = -4,
  "America/Indiana/Tell_City" = -4,
  "America/Menominee" = -4,
  "America/North_Dakota/Beulah" = -4,
  "America/North_Dakota/New_Salem" = -4,
  "America/Boise" = -5,
  "America/Denver" = -5,
  "America/Phoenix" = -5,
  "America/Los_Angeles" = -6,
  "America/Anchorage" = -7,
  "America/Nome" = -7,
  "America/Sitka" = -7,
  "America/Yakutat" = -7,
  "America/Adak" = -8,
  "Pacific/Honolulu" = -8)
df_timezone_val <- revalue(x= new_data, replace = df_timezone_dict)
df$Timezone <- as.numeric(df_timezone_val)

#Education

unique(df$Education)

new_data <- df$Education
df_education_dict <- c(
  "No Schooling Completed" = 0,
  "Nursery School to 8th Grade" = 8,
  "9th Grade to 12th Grade, No Diploma" = 12,
  "GED or Alternative Credential" = 12,
  "Regular High School Diploma" = 12,
  "Some College, Less than 1 Year" = 13,
  "Some College, 1 or More Years, No Degree" = 14,
  "Associate's Degree" = 15,
  "Bachelor's Degree" = 16,
  "Master's Degree" = 18,
  "Professional School Degree" = 20,
  "Doctorate Degree" = 24
)
df_education_val <- revalue(x= new_data, replace = df_education_dict)
df$Education <- as.numeric(df_education_val)

unique(df$Complication_risk)

new_data <- df$Complication_risk
df_comprisk_dict <- c(
  "Low" = 1,
  "Medium" = 2,
  "High" = 3)
df_risk_val <- revalue(x= new_data, replace = df_comprisk_dict)
df$Complication_risk <- as.numeric(df_risk_val)

unique(df$Services)

new_data <- df$Services
df_services_dict <- c(
  "Blood Work" = 1,
  "Intravenous" = 2,
  "CT Scan" = 3,
  "MRI" = 4)
df_services_val <- revalue(x= new_data, replace = df_services_dict)
df$Services <- as.numeric(df_services_val)


#Binary expression change

#ReAdmis

unique(df$ReAdmis)

new_data <- df$ReAdmis
bi_dict <- c(
  "No" = 0,
  "Yes" = 1
)
bi_val <- revalue(x= new_data, replace = bi_dict)
df$ReAdmis <- as.numeric(bi_val)

#Soft_drink

unique(df$Soft_drink)

new_data <- df$Soft_drink
bi_val <- revalue(x= new_data, replace = bi_dict)
df$Soft_drink <- as.numeric(bi_val)

#Highblood

unique(df$HighBlood)

new_data <- df$HighBlood
bi_val <- revalue(x= new_data, replace = bi_dict)
df$HighBlood <- as.numeric(bi_val)

#stroke

new_data <- df$Stroke
bi_val <- revalue(x= new_data, replace = bi_dict)
df$Stroke <- as.numeric(bi_val)

new_data <- df$Arthritis
bi_val <- revalue(x= new_data, replace = bi_dict)
df$Arthritis <- as.numeric(bi_val)

new_data <- df$Diabetes
bi_val <- revalue(x= new_data, replace = bi_dict)
df$Diabetes <- as.numeric(bi_val)

new_data <- df$Hyperlipidemia
bi_val <- revalue(x= new_data, replace = bi_dict)
df$Hyperlipidemia <- as.numeric(bi_val)

new_data <- df$BackPain
bi_val <- revalue(x= new_data, replace = bi_dict)
df$BackPain <- as.numeric(bi_val)

new_data <- df$Allergic_rhinitis
bi_val <- revalue(x= new_data, replace = bi_dict)
df$Allergic_rhinitis <- as.numeric(bi_val)

new_data <- df$Reflux_esophagitis
bi_val <- revalue(x= new_data, replace = bi_dict)
df$Reflux_esophagitis <- as.numeric(bi_val)

new_data <- df$Asthma
bi_val <- revalue(x= new_data, replace = bi_dict)
df$Asthma <- as.numeric(bi_val)

unique(df$Employment)

#Caret Function

library(caret)

#Dummy Variables

#Employment

dmy <- dummyVars(" ~ Employment", data = df)
my_dummy <- data.frame(predict(dmy, newdata = df))
df$Employment_FullTime <- my_dummy$EmploymentFull.Time
df$Employment_PartTime <- my_dummy$EmploymentPart.Time
df$Employment_Retired <- my_dummy$EmploymentRetired
df$Student <- my_dummy$EmploymentStudent
df$Unemployed <- my_dummy$EmploymentUnemployed
df <- select(df, -Employment)

#Gender

unique(df$Gender)

dmy <- dummyVars(" ~ Gender", data = df)
my_dummy <- data.frame(predict(dmy, newdata = df))
df$Female <- my_dummy$GenderFemale
df$Male <- my_dummy$GenderMale
df$GenderPrefer.not.to.answer <- my_dummy$GenderPrefer.not.to.answer
df <- select(df, -Gender)

#Initial_admin

unique(df$Initial_admin)

dmy <- dummyVars(" ~ Initial_admin", data = df)
my_dummy <- data.frame(predict(dmy, newdata = df))
df$Admin_elective <- my_dummy$Initial_adminElective.Admission
df$Admin_observation <- my_dummy$Initial_adminObservation.Admission
df$Admin_emergency <- my_dummy$Initial_adminEmergency.Admission
df <- select(df, -Initial_admin)

#Imputation of NULL Values

summary(df)

var <- df$Children
df$Children <- replace(var, is.na(var), 0)

var <- df$Soft_drink
df$Soft_drink <- replace(var, is.na(var), 0)

var <- df$Anxiety
df$Anxiety <- replace(var, is.na(var), 0)

#Mice function

library(mice)

micedata <- df

micedata$Overweight=as.factor(micedata$Overweight)

mymice = mice(micedata,m=5,method=c("","","","","","","","","","","pmm","","pmm","","","","","","","","","","logreg","","","","","","","","","","pmm","","","","","","","","","","","","","","","","","","","","",'') ,maxit=20)

summary(micedata$Age)

mymice$imp$Age

mymicecomplete <- complete(mymice, 2)
df<- mymicecomplete

summary(df)

#Change Overweight to numeric

df$Overweight=as.numeric(df$Overweight)

md.pattern(df)

#Identifying Outliers

#Setting dataset to identify outliers

df <- df[,c(14, 1, 3, 2, 4:12, 44, 45, 46, 48, 47, 49, 50, 13, 15, 18, 16, 17, 19:32, 51:53, 33:43)]

head(df)

#ggplot2 function

library(ggplot2)

#Checking for Outliers using Boxplots

#Population

graph1 <- qplot(data = df, y= Population, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Population') +
  geom_text(aes(label=ifelse(Population %in% boxplot.stats(Population)$out,
                             as.character(Zip), "")), hjust = 1.5)
graph1

df$Population <- scale(x = df$Population)

#Children

graph2 <- qplot(data = df, y= Children, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Children')
graph2

#Need to perform Grubbs test

#Outliers function

library(outliers)

x <- df$Children
grubbs.test(x)

#Age

graph3 <- qplot(data = df, y= Age, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Age')
graph3

#Education

graph4 <- qplot(data = df, y= Education, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Education')
graph4

df$Education <- scale(x = df$Education)

#Total_Income

graph5 <- qplot(data = df, y= Total_Income, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Total_Income')
graph5

x <- df$Total_Income
grubbs.test(x)

df$Total_Income <- scale(x = df$Total_Income)

#VitD_Levels

graph6 <- qplot(data = df, y= VitD_levels, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='VitD_levels')
graph6

#Checking for coreelation between VitD_Levels and VitD_supp

plot(df$VitD_levels, df$VitD_supp, 
     main ='Vitamin D Levels and Supplements', 
     xlab ='Levels', 
     ylab = 'Supplements Administered')

high_VitD <- which(df$VitD_levels > 30 & df$VitD_supp>1)
houtput <- df[high_VitD,] ; houtput

plot(houtput$Age, houtput$Overweight)

#Doc_Visits

graph7 <- qplot(data = df, y= Doc_visits, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Doc_visits') +
  geom_text(aes(label=ifelse(Doc_visits %in% boxplot.stats(Doc_visits)$out,
                             as.character(ReAdmis), "")), hjust = 1.5)
graph7

#Full_meals_eaten

graph8 <- qplot(data = df, y= Full_meals_eaten, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Full_meals_eaten')
graph8

x <- df$Full_meals_eaten
grubbs.test(x)

df$Full_meals_eaten <- scale(x = df$Full_meals_eaten)

#Complication_risk

graph9 <- qplot(data = df, y= Complication_risk, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Complication_risk') +
  geom_text(aes(label=ifelse(Complication_risk %in% boxplot.stats(Complication_risk)$out,
                             as.character(ReAdmis), "")), hjust = 1.5)
graph9

#Initial_days

graph10 <- qplot(data = df, y= Initial_days, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Initial_days') +
  geom_text(aes(label=ifelse(Initial_days %in% boxplot.stats(Initial_days)$out,
                             as.character(Initial_days), "")), hjust = 1.5)
graph10

#TotalCharge

graph11 <- qplot(data = df, y= TotalCharge, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='TotalCharge') +
  geom_text(aes(label=ifelse(TotalCharge %in% boxplot.stats(TotalCharge)$out,
                             as.character(ReAdmis), "")), hjust = 1.5)
graph11

#Additional_charges

graph12 <- qplot(data = df, y= Additional_charges, x=1,
                 geom='boxplot', 
                 outlier.color='deeppink2',
                 xlim=c(0,2), 
                 main='Additional_charges') +
  geom_text(aes(label=ifelse(Additional_charges %in% boxplot.stats(Additional_charges)$out,
                             as.character(ReAdmis), "")), hjust = 1.5)
graph12

df$Additional_charges <- scale(x = df$Additional_charges)

#Survey_results

survey_results <- df[,46:53]
for (col in survey_results){
  graph13 <- qplot(data = survey_results, y= col, x=1,
                   geom='boxplot', 
                   outlier.color='deeppink2',
                   xlim=c(0,2))
  print(graph13)
}

for (col in survey_results){
  x <- col
  print(grubbs.test(x))
}

#Principle Component Analysis

head(df)

df_sub <- scale(x = df[,c(5, 8, 9, 11:53)])
head(df_sub)

library(FactoMineR)

df_sub.pca <- PCA(df_sub, scale.unit=TRUE, graph=F)

eig.val <- df_sub.pca$eig

barplot(eig.val[, 2],
        
        main = "Explained Variance (%)",
        
        xlab = "Principal Components",
        
        ylab = "(%) of Variance",
        
        col = "darkblue")


plot(df_sub.pca, choix = "var", autoLab = "auto", col.var="darkblue", label="var", graph.type = "ggplot", select="cos2 0.40")





