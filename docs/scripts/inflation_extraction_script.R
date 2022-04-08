#I used this to test my inflation data extraction script prior to implementing it into my project


#Loading all necessary libraries
library(tidyverse)
library(here)

#Extracting data into data frame
cpih_df <- read_csv(here("data","raw","csv","cpih01-time-series-v20.csv"))

#Removing unneeded columns and renaming remaining ones to be code-friendly
cpih_df <- cpih_df[,c(1,2,7)]
names(cpih_df) <- c("cpih","date","code")

#Filtering in the Overall CPIH index data, then removing since it is now not needed
cpih_df <- cpih_df %>% filter(code=="Overall Index")
cpih_df <- cpih_df[,c(2,1)]

#Editing date format to be code-friendly, adding 01 to the front to be compatible with as.Date
cpih_df$date <- paste("01",cpih_df$date,sep="-")
cpih_df$date <- as.Date(cpih_df$date,format="%d-%b-%y")
cpih_df <- cpih_df[order(cpih_df$date),]
cpih_df$cpih <- as.numeric(cpih_df$cpih)

#Removing dates before 2009-09-30, i.e. when the NHS salary data starts
cpih_df <- filter(cpih_df,date >= "2009-09-01")

#Removing cpih dates from 2022-01-01 and 2022-02-01 i.e. when NHS salary data ends
cpih_df <- filter(cpih_df,date <= "2021-12-01")

#Reformatting cpih so that the index is not 2015=100 but 2009-09-01=100
for (i in c(1:nrow(cpih_df))){
  if (i!=nrow(cpih_df)){
    cpih_df$cpih[i+1] <- as.numeric(100+(cpih_df[i+1,2]-cpih_df[1,2]))
  }
  else{
    cpih_df$cpih[1] <- 100
  }
}

#Plotting over time to see change
p_cpih <- ggplot(cpih_df,aes(x=date,y=cpih))

p_cpih+geom_line()

#Saving data to 'refined' data folder
write.csv(cpih_df,here("data","refined","cpih_data.csv"))
