#I used this to test my income vs inflation script prior to implementing it into my project


#Loading all necessary libraries
library(tidyverse)
library(here)

#Extracting required data into data frame
mean_salary_change_df <- read_csv(here("data","refined","mean_salary_change.csv"))
cpih_df <- read_csv(here("data","refined","cpih_data.csv"))
salary_change_df <- read_csv(here("data","refined","salary_change.csv"))

#Removing useless columns in all dfs
mean_salary_change_df <- mean_salary_change_df[,-1]
cpih_df <- cpih_df[,-1]
salary_change_df <- salary_change_df[,-1]

#Reformatting cpih so that the index starts at 0, to match salary change dfs
for (i in c(1:nrow(cpih_df))){
  cpih_df$cpih[i] <- as.numeric(cpih_df[i,2]-100)
}

#Creating a cpih df to be compatible with all groups of salary_change_df
for (i in c(1:5)){
  if (i==1){
    cpih_all_groups <- cpih_df
  }
  else{
    cpih_all_groups <- rbind(cpih_all_groups,cpih_df)
  }
}


#First project will be to compare mean data with cpih data

#Merging mean data and cpih data & removing superfluous date column
cpih_vs_mean_salary <- cbind(mean_salary_change_df,cpih_df)
cpih_vs_mean_salary <- cpih_vs_mean_salary[,-3]

#Calculating difference between cpih and mean salary change
cpih_vs_mean_salary$diff <- cpih_vs_mean_salary$mean_salary_change-cpih_vs_mean_salary$cpih

#Plotting the difference
p_mean_diff <- ggplot(cpih_vs_mean_salary,aes(x=date,y=diff))

p_mean_diff+geom_line()+geom_smooth(method="gam")






#Calculating effective money gained/lost from beginning to end of time sequence

#The comparison income will be the mean income per month of all doctors
mean_income <- mean(salary_change_df$amount)

#Altering cpih_vs_mean_salary to include a column with cumulative income change
for (i in c(1:nrow(cpih_vs_mean_salary))){
  if (i==1){
    cpih_vs_mean_salary$cumulative_income_change[1] <- 0 #set baseline
  }
  else{
    cpih_vs_mean_salary$cumulative_income_change[i] <- 
      (mean_income*(cpih_vs_mean_salary$diff[i]/100))+
      cpih_vs_mean_salary$cumulative_income_change[i-1]
  }
}

#Plotting the above
p_cumulative <- ggplot(cpih_vs_mean_salary,aes(x=date,y=cumulative_income_change))

p_cumulative+geom_line()


#Data can be changed to represent cumulative income lost due to inflation




mean_income_end <- mean_income

for (i in c(1:nrow(cpih_vs_mean_salary))){
  mean_income_end <- (mean_income*(cpih_vs_mean_salary$diff[i]/100))+
    mean_income_end
  print(mean_income_end)
}





#Second project is to compare each doctor group with cpih data

#Merging all groups data and cpih data & removing superfluous date column
cpih_vs_salary <- cbind(salary_change_df,cpih_all_groups)
cpih_vs_salary <- cpih_vs_salary[,-5]

#Calculating difference between cpih and mean salary change
cpih_vs_salary$diff <- cpih_vs_salary$change-cpih_vs_salary$cpih

#Plotting the difference
p_diff <- ggplot(cpih_vs_salary,aes(x=date,y=diff,color=group))

p_diff+geom_line()+geom_smooth(method="gam")
