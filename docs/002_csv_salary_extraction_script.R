#I used this to test my csv extraction script prior to implementing it into my project


#Loading all necessary libraries
library(tidyverse)
library(here)

#Extracting data into data frame
salary_df <- read_csv(here("data","raw","csv","2010-08-2021-12.csv"))

#Extracting only date, staff group, payment type, and amount
salary_df <- salary_df %>% select("Date","Staff Group","Payment type","Amount")

#Renaming column titles to be code-friendly
names(salary_df) <- c("date","group","pay_code","amount")


#Creating a vector of the group roles we want to filter in
target_groups <- c("Consultant","Specialty Registrar","Core Training",
                   "Foundation Doctor Year 2","Foundation Doctor Year 1")

#filtering in the desired pay code data
salary_df <- salary_df %>% filter(group %in% target_groups,
                     pay_code=="PUBGRP_010_BASIC_PAY_PER_FTE")

#Removing pay_code since it is now not needed
salary_df <- salary_df[,-c(3)]

#Ensuring 'date' column is considered date data
salary_df$date <- as.Date(salary_df$date,format="%Y-%m-%d")


#Plotting the data to visualise change in salary over time
p <- ggplot(salary_df,aes(x=date,y=amount,color=group))

p+geom_point()+
  geom_smooth(method=lm)


#creating df to store salary change data from upcoming loop
salary_change_df <- data.frame(NA,NA,NA,NA)
colnames(salary_change_df) <- c("date","group","amount","salary_change")
salary_change_df$date <- as.Date(salary_change_df$date,format="%Y-%m-%d")


#For loop to calculate % change in salary between months for each group
for (i in c(1:length(target_groups))){
  #Creating temporary df organising pay data by group and in date order
  temp_df <- filter(salary_df,group==target_groups[i])
  temp_df <- temp_df[order(temp_df$date),]
  
  #Finding % change in salary per month compared to August 2010, for specified group
  for (val in c(1:nrow(filter(salary_df,group==target_groups[i])))){
    temp_df$salary_change[val] <- as.numeric(100*(temp_df[val,3]-temp_df[1,3])/temp_df[1,3])
  }
  
  #Adding temp_df to salary_change_df
  salary_change_df <- rbind(salary_change_df,temp_df)
  
  #Removing superfluous NA row from salary_change_df
  if (i==length(target_groups)){
    salary_change_df <- salary_change_df[-1,]
    rownames(salary_change_df) <- c(1:length(salary_change_df[,1]))
  }
  #End of loop
}

#Plot to visualise salary change for each group 
p_csv_salary <- ggplot(salary_change_df,aes(x=date,y=salary_change,color=group))

p_csv_salary+geom_point()+
  geom_smooth(method="gam")


#Creating df to measure mean salary change across all groups, over time
mean_salary_change_df <- salary_change_df %>% 
  group_by(date) %>%
  summarise(mean_salary_change=mean(salary_change))

#Visualising this data, to check it is correct
p_mean_salary_change <- ggplot(mean_salary_change_df,aes(x=date,y=mean_salary_change))

p_mean_salary_change+geom_point()+
  geom_smooth(method="gam")


#Saving both pieces of data to 'refined' data folder
write.csv(salary_change_df,here("data","refined","salary_change.csv"))
write.csv(mean_salary_change_df,here("data","refined","mean_salary_change.csv"))

