
#Include libraries that are required 
library(dplyr)
library(MASS)
library(car)
library(reshape2)
library(ggplot2)
library(cowplot)
library(tis)
library(e1071)
library(caret)
library(ROCR)

#Read CSV files into dataframes
employee <- read.csv('general_data.csv')
employee_survey <- read.csv('employee_survey_data.csv' , stringsAsFactors = FALSE)
manager_survey <- read.csv('manager_survey_data.csv' , stringsAsFactors = FALSE)
in_time <- read.csv('in_time.csv',stringsAsFactors = FALSE);
out_time <- read.csv('out_time.csv',stringsAsFactors = FALSE);

#Collating the Data
length(unique(tolower(employee$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(employee_survey$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(in_time$X)))  # 4410, confirming X is key, X is employeeid
length(unique(tolower(out_time$X))) # 4410, confirming X is key, x is employee id

setdiff(employee$EmployeeID,in_time$X) 
setdiff(employee$EmployeeID,out_time$X) 
setdiff(employee$EmployeeID,employee_survey$EmployeeID) 
setdiff(employee$EmployeeID,manager_survey$EmployeeID) 

# As number of rows and Employee_IDs are same across input files data is merged into single master file
employee_master <- merge(employee , employee_survey , by = 'EmployeeID')
employee_master <- merge(employee_master , manager_survey , by = 'EmployeeID')
sum(is.na(employee))
sum(is.na(employee$NumCompaniesWorked))
sum(is.na(employee$TotalWorkingYears))

#Preparing In & Out time csv files
#eliminating the holiday columns (that have na values for all the employees)
in_time_working_days <-  in_time[,colSums(is.na(in_time))<nrow(in_time)];
out_time_working_days <-  out_time[,colSums(is.na(out_time))<nrow(out_time)];


##Setting the date time to correct formats
in_time_working_days[,-1] <- lapply(in_time_working_days[,-1], function(x) {as.POSIXct(x,format = "%Y-%m-%d %H:%M:%S")})
class(in_time_working_days$X2015.01.05)
out_time_working_days[,-1] <- lapply(out_time_working_days[,-1], function(x) {as.POSIXct(x,format = "%Y-%m-%d %H:%M:%S")})
#Sorting the file based on emp id
in_time_working_days <- arrange(in_time_working_days,X)
out_time_working_days <- arrange(out_time_working_days,X)

## Calculating the hours spent by employee in office
daily_office_hours <- out_time_working_days[,-1] - in_time_working_days[-1]


##Now calculating the average hours spent in office by an employee for the year by converting difftime into numeric
daily_office_hours_num <- data.frame(lapply(daily_office_hours,function(x) { as.numeric(x)}))
average <- data.frame(round(rowMeans(daily_office_hours_num,na.rm = TRUE),2))

# Calculating leaves per employee
sum(is.na(out_time_working_days))
sum(is.na(in_time_working_days))
sum(is.na(daily_office_hours_num))
#As count of NA is same across in & out time it is safe to assume these are leaves
#Calculating leaves per employee
Leaves_per_employee <- data.frame(apply(in_time_working_days, 1, function(x) sum(is.na(x))))

#Binding leaves and average time per employee to master file

punch_in <- cbind(out_time_working_days[,1],average,Leaves_per_employee)
colnames(punch_in) <- c("EmployeeID","Average_office_hours","Leaves_taken")

employee_master <- merge(employee_master,punch_in,by="EmployeeID")

#Data Preparation for other csv files


str(employee_master)


#removing columns with single constant values
employee_master <- employee_master[,-c(9,16,18)]

# NA values Treatment
sum(is.na(employee_master))
sum(is.na(employee_master$NumCompaniesWorked))
#removing NA values rows as count is less than 3%
employee_master <- na.omit(employee_master)
#######################################

## Factor Conversion

employee_master$Education <- as.factor(employee_master$Education)
employee_master$JobLevel <- as.factor(employee_master$JobLevel)
employee_master$StockOptionLevel <- as.factor(employee_master$StockOptionLevel)


# Outlier Treatment

quantile(employee_master$DistanceFromHome,probs = seq(0, 1, 0.01), na.rm = FALSE)

quantile(employee_master$NumCompaniesWorked,probs = seq(0, 1, 0.01), na.rm = FALSE)

quantile(employee_master$MonthlyIncome,probs = seq(0, 1, 0.01), na.rm = FALSE)

quantile(employee_master$PercentSalaryHike,probs = seq(0, 1, 0.01), na.rm = FALSE)

quantile(employee_master$TotalWorkingYears,seq(0,1,0.01),na.rm = TRUE)
employee_master$TotalWorkingYears[which(employee_master$TotalWorkingYears > 32)] <- 32

quantile(employee_master$TrainingTimesLastYear,probs = seq(0, 1, 0.01), na.rm = FALSE)

quantile(employee_master$YearsAtCompany,probs = seq(0, 1, 0.01), na.rm = FALSE)
# Capped at 95 percentile 
employee_master$YearsAtCompany[which(employee_master$YearsAtCompany>25)]<-25

quantile(employee_master$YearsSinceLastPromotion,probs = seq(0, 1, 0.01), na.rm = FALSE)

quantile(employee_master$YearsWithCurrManager,probs = seq(0, 1, 0.01), na.rm = FALSE)
#Capped at 99 percentile
employee_master$YearsWithCurrManager[which(employee_master$YearsWithCurrManager>14)]<-14
str(employee_master)


#Converting int to categorical values
employee_master$Education<-recode_factor(employee_master$Education,'1'="Below College",'2'="College",'3'="Bachelor",'4'="Master",'5'="Doctor")
employee_master$EnvironmentSatisfaction<-recode_factor(employee_master$EnvironmentSatisfaction,'1'="Low",'2'="Medium",'3'="High",'4'="Very High")
employee_master$JobInvolvement<-recode_factor(employee_master$JobInvolvement,'1'="Low",'2'="Medium",'3'="High",'4'="Very High")
employee_master$JobSatisfaction<-recode_factor(employee_master$JobSatisfaction,'1'="Low",'2'="Medium",'3'="High",'4'="Very High")
employee_master$PerformanceRating<-recode_factor(employee_master$PerformanceRating,'1'="Low",'2'="Good",'3'="Excellent",'4'="Outstanding")
employee_master$WorkLifeBalance<-recode_factor(employee_master$WorkLifeBalance,'1'="Bad",'2'="Good",'3'="Better",'4'="Best")

str(employee_master)
employee_data <- employee_master
#Normalizing the continuous features
employee_master$Age  <-scale(employee_master$Age)
employee_master$DistanceFromHome  <-scale(employee_master$DistanceFromHome)
employee_master$MonthlyIncome  <-scale(employee_master$MonthlyIncome)
employee_master$NumCompaniesWorked  <-scale(employee_master$NumCompaniesWorked)
employee_master$PercentSalaryHike  <-scale(employee_master$PercentSalaryHike)
employee_master$TotalWorkingYears  <-scale(employee_master$TotalWorkingYears)
employee_master$TrainingTimesLastYear  <-scale(employee_master$TrainingTimesLastYear)
employee_master$YearsAtCompany  <-scale(employee_master$YearsAtCompany)
employee_master$YearsSinceLastPromotion  <-scale(employee_master$YearsSinceLastPromotion)
employee_master$YearsWithCurrManager  <-scale(employee_master$YearsWithCurrManager)
employee_master$Average_office_hours  <-scale(employee_master$Average_office_hours)
employee_master$Leaves_taken<-scale(employee_master$Leaves_taken)


#Processing Categorical Attributes
#Factors with 2 levels to numerical
levels(employee_master$Attrition) <- c(0,1) # Ye for 1 and 0 for No
employee_master$Attrition <- as.numeric(levels(employee_master$Attrition))[employee_master$Attrition]
levels(employee_master$Gender) <- c(1,0) # FEMALE for 1 and 0 for MALE
employee_master$Gender <- as.numeric(levels(employee_master$Gender))[employee_master$Gender]
levels(employee_master$PerformanceRating) <- c(1,0) # No for 1 and 0 for Excellent & Outstanding rating as only 2 ratings are there for each employee
employee_master$PerformanceRating <- as.numeric(levels(employee_master$PerformanceRating))[employee_master$PerformanceRating]

#Factors with more than 2 levels, creating dummies 
employee_m<- employee_master[,c(4,5,7,8,10:12,16,22:25)]
dummies<- data.frame(sapply(employee_m, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_m))[,-1]))
employee_master_final<- cbind(employee_master[,-c(1,4,5,7,8,10:12,16,22:25)],dummies) 



sum(is.na(employee_master_final)) #Zero NAs

employee_master_final1<-employee_master_final
#Preparing for regression
set.seed(100)
trainindices= sample(1:nrow(employee_master_final), 0.7*nrow(employee_master_final))
train = employee_master_final[trainindices,]
test = employee_master_final[-trainindices,]

########################################################################
# Logistic Regression: 
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 2115.3

model_2<- stepAIC(model_1, direction="both")
summary(model_2)
#Checking Coliniarity 
vif(model_2) #2078.2

#Removing Department.xSales as it is having high vif and high p-value


model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
      YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
      BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
      Education.xCollege + EducationField.xOther + 
      EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
      JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
      JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
      JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
      EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
      EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
      JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
      WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
    family = "binomial", data = train)
summary(model_3) # 2089.2
vif(model_3)

#removing BusinessTravel.xTravel_Rarely as high vif and low significance 
model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
               Education.xCollege + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
             family = "binomial", data = train)
summary(model_4) #2095.1
vif(model_4)

#Removing JobRole.xManufacturing.Director  as High P Values
model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
               Department.xResearch...Development + 
               Education.xCollege + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xLaboratory.Technician  + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
             family = "binomial", data = train)
summary(model_5) #2095

#removing Department.xResearch...Development as high P-Value
model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
               Education.xCollege + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xLaboratory.Technician  + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
             family = "binomial", data = train)
summary(model_6) #2095.1

#Removing EducationField.xTechnical.Degree as high Pvalues
model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
               Education.xCollege + EducationField.xOther + 
                JobLevel.x2 + JobLevel.x5 + 
               JobRole.xLaboratory.Technician  + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
             family = "binomial", data = train)
summary(model_7) #2095.4

#Removing PercentSalaryHike  as high P-Value
model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
               Education.xCollege + EducationField.xOther + 
               JobLevel.x2 + JobLevel.x5 + 
               JobRole.xLaboratory.Technician  + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
             family = "binomial", data = train)
summary(model_8) #2096.8

#Removing StockOptionLevel.x1 as high PValue
model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
               Education.xCollege + EducationField.xOther + 
               JobLevel.x2 + JobLevel.x5 + 
               JobRole.xLaboratory.Technician  + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle +  
               EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
             family = "binomial", data = train)
summary(model_9) #2097.3

#Removing EducationField.xOther then  JobLevel.x5 then Education.xCollege sequentially 
model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
               JobLevel.x2 + 
               JobRole.xLaboratory.Technician  + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle +  
               EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
               WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
             family = "binomial", data = train)
summary(model_10) #2098.8,2100.2,2102

#Removing  JobLevel.x2  as high P Value
model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician  + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
              family = "binomial", data = train)
summary(model_11) #2104

#Removing JobRole.xLaboratory.Technician  as high P-Value
model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
              family = "binomial", data = train)
summary(model_12) #2108.1

#Removing JobRole.xResearch.Scientist  as high p-value
model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
              family = "binomial", data = train)
summary(model_13) #2109

#Removing JobRole.xSales.Executive as high P-Value
model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + 
                 MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
              family = "binomial", data = train)
summary(model_14) #2112.1

#Removing JobRole.xResearch.Director   as high P-Value
model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xHigh, 
              family = "binomial", data = train)
summary(model_15) #2113.7

#Removing JobInvolvement.xHigh  as high P-Value
model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest , 
              family = "binomial", data = train)
summary(model_16) #2117.5

#Removing JobSatisfaction.xMedium as high P-Value
model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest , 
              family = "binomial", data = train)
summary(model_17) #2122.3

#Removing JobSatisfaction.xHigh 
model_18<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest , 
              family = "binomial", data = train)
summary(model_18) #2126.6

#Removing TrainingTimesLastYear 
model_19<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest , 
              family = "binomial", data = train)
summary(model_19) #2135.4

#All variables are having *** now but have high VIF thus removing highest vif WorkLifeBalance.xBetter 
vif(model_19)

model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBest , 
              family = "binomial", data = train)
summary(model_20) #2169.1
vif(model_20)
#removing insignificant WorkLifeBalance.xBest
model_21<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xGood 
                 , 
              family = "binomial", data = train)
summary(model_21) #2167.6
vif(model_21)
#removing insignificant WorkLifeBalance.xGood

model_22<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + Average_office_hours + BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +  
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xVery.High 
              , 
              family = "binomial", data = train)
summary(model_22) #2169
vif(model_22)
################## all values are significant *** and VIF < 2.5
final_model<- model_22

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)


test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf


#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 1 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,1,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
#cut-off is .17
###################################################################################
# Let's choose a cutoff value of .17 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.17, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec


##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
pred_object_test
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")


ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart



lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)),
           Random_Gain=bucket*10) 
  return(gaintable)
}

attr_decile = lift(test_actual_attrition, test_pred, groups = 10)

ggplot(attr_decile,aes(bucket))+
  geom_line(aes(y=Gain),colour="Blue")+geom_point(aes(y=Gain))+
geom_line(aes(y=Random_Gain),colour="Red")+geom_point(aes(y=Random_Gain))



# Graphs depicting the significance of Model Attributes

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


# Graphical visualisation of 
#BusinessTravel, JobSatisfaction, EnviromenySatisfaction, MaritalStatus
plot_grid(ggplot(employee_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(),           
          ggplot(employee_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(),           
          ggplot(employee_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(),           
          ggplot(employee_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(), align = "h")      
#Age, NumCompaniesWorked, TotalWorkingYears, YearsSinceLastPromotion, YearsWithCurrentManager, AverageOfficeHours
plot_grid(ggplot(employee_data, aes(x=Age,fill=Attrition))+ geom_bar(),           
          ggplot(employee_data, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(),           
          ggplot(employee_data, aes(x=TotalWorkingYears,fill=Attrition))+ geom_bar(),           
          ggplot(employee_data, aes(x=YearsSinceLastPromotion ,fill=Attrition))+ geom_bar(),           
          ggplot(employee_data, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar(),           
          ggplot(employee_data, aes(x=Average_office_hours,fill=Attrition))+ geom_bar(),align = "h")     


