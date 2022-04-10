#I used this to test my income vs inflation script prior to implementing it into my project


#Loading all necessary libraries
library(tidyverse)
library(here)
library(scales)

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


#First Data-Wrangling & Visualisation:
#Comparing mean salary data with cpih data

#Merging mean data and cpih data & removing superfluous date column
cpih_vs_mean_salary <- cbind(mean_salary_change_df,cpih_df)
cpih_vs_mean_salary <- cpih_vs_mean_salary[,-3]

#Calculating difference between cpih and mean salary change
cpih_vs_mean_salary$diff <- cpih_vs_mean_salary$mean_salary_change-cpih_vs_mean_salary$cpih

#Plotting the difference
p_mean_diff <- ggplot(cpih_vs_mean_salary,aes(x=date,y=diff))

p_mean_diff+geom_line()+geom_smooth(method="gam")



#Second Data-Wrangling & Visualisation:
#Calculating effective money gained/lost from beginning to end of time sequence
#For average income of all doctors

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

#Changing data to represent cumulative income lost due to inflation
cpih_vs_mean_salary$cumulative_income_lost <- -cpih_vs_mean_salary$cumulative_income_change

#Plotting the above, with changes to make the plot aesthetically pleasing
p_mean_cumulative <- ggplot(cpih_vs_mean_salary,aes(x=date,y=cumulative_income_lost))

p_mean_cumulative+geom_line(colour="steelblue",size=1)+
  scale_y_continuous(labels=dollar_format(prefix="£"),
                     breaks=seq(0,30000,5000),
                     minor_breaks=seq(-1000,31000,1000),
                     limits=c(-1000,30000),
                     expand=c(0,0))+
  scale_x_date(date_breaks="1 year",
               date_labels="%Y",
               limits=as.Date(c("2009-07-01","2022-07-01")),
               expand=c(0,0))+
  labs(title="Average Cumulative Income Lost to Inflation by an NHS Doctor",
       subtitle="From Sept 2009 to Dec 2021")+
  theme(plot.title=element_text(colour="steelblue"))+
  xlab("Date")+
  ylab("Cumulative Income Lost")+
  theme(axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(vjust=2)) #shifting axis titles to give space



#Third Data-Wrangling & Visualisation:
#Comparing per-group salary data with cpih data

#Merging all groups data and cpih data & removing superfluous date column
cpih_vs_salary <- cbind(salary_change_df,cpih_all_groups)
cpih_vs_salary <- cpih_vs_salary[,-5]

#Calculating difference between cpih and salary change
cpih_vs_salary$diff <- cpih_vs_salary$change-cpih_vs_salary$cpih

#Plotting the difference
p_diff <- ggplot(cpih_vs_salary,aes(x=date,y=diff,colour=group))

p_diff+geom_line()+geom_smooth(method="gam")



#Fourth Data-Wrangling & Visualisation:
#Calculating effective money gained/lost from beginning to end of time sequence
#For each income of each doctor group

#Creating group_change to measure income lost per group, with each group being 148 long
for (i in c(1:nrow(cpih_vs_salary))){
  if (i==1){
    cpih_vs_salary$cumulative_income_lost[i] <- 0 #set beginning baseline
  }
  else if ((i-1)%%148==0){
    cpih_vs_salary$cumulative_income_lost[i] <- 0 #set baseline for each group
  }
  else{
    cpih_vs_salary$cumulative_income_lost[i] <- 
      -((mean_income*(cpih_vs_salary$diff[i]/100))-
      cpih_vs_salary$cumulative_income_lost[i-1])
  }
}

#Ordering the 'group' column in order of descending seniority (consultant->fy1)
cpih_vs_salary$group <- factor(cpih_vs_salary$group,
                               levels=c("Consultant",
                                        "Specialty Registrar",
                                        "Core Training",
                                        "Foundation Doctor Year 2",
                                        "Foundation Doctor Year 1"))

#Plotting the above, with changes to make it aesthetically pleasing
p_group_cumulative <- ggplot(cpih_vs_salary,aes(x=date,
                                                y=cumulative_income_lost,
                                                colour=group))

p_group_cumulative+
  geom_line(size=1)+
  theme_light()+
  scale_y_continuous(labels=dollar_format(prefix="£"),
                     breaks=seq(0,50000,5000),
                     minor_breaks=seq(-2000,50000,1000),
                     limits=c(-2000,48000),
                     expand=c(0,0))+
  scale_x_date(date_breaks="1 year",
               date_minor_breaks="3 months",
               date_labels="%Y",
               limits=as.Date(c("2009-07-01","2022-06-15")),
               expand=c(0,0))+
  labs(title="Cumulative Salary Lost to Inflation by NHS Doctors",
       subtitle="From Sept 2009 to Dec 2021, per Stage of Training",
       colour="Stage of Training")+
  theme(plot.title=element_text(colour="#56B4E9",size=15),
        panel.grid.major=element_line(size=1,colour="grey85"),
        panel.grid.minor=element_line(colour="grey90"),
        axis.ticks=element_line(size=1))+ #to match major grid size
  xlab("Date")+
  ylab("Cumulative Salary Loss")+
  scale_colour_manual(values=c("#E69F00",
                               "#56B4E9",
                               "#009E73",
                               "#F0E442",
                               "#D55E00"))+ #compatible for colour-blind people
  theme(axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(vjust=2.5)) #shifting axis titles to give space
