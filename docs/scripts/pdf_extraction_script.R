#I used this to test my pdf extraction prior to implementing it into my project


#Loading all necessary libraries
library(tidyverse)
library(here)
library(pdftools)
library(data.table)


#Creating function to extract salary data from .pdf file
data_extract <- function(raw){
  
  #Splits single pages
  raw <- map(raw,~str_split(.x,"\\n") %>% unlist())
  
  #Concatenate split pages
  raw <- reduce(raw,c)
  
  #Specifying start and end of desired table
  table_start <- stringr::str_which(tolower(raw),"foundation yr 1")-1
  table_end <- stringr::str_which(tolower(raw),"source")-1
  table_end <- table_end[min(which(table_end>table_start))] #this is to make sure I get the right table
  
  #Creating a table & removing the special characters
  table <- raw[(table_start):(table_end)]
  table <- table %>% str_replace_all("\\s{2,}","|") %>%
    str_replace_all("£","") %>% 
    str_replace_all(",","")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con,sep="|")
  
  #Extracting desired column and making numerical
  data_table <- data_table[c(1,4)]
  data_table <- data_table %>% transform(Salary.3=as.numeric(Salary.3))
  
  #Merging consultant role salaries into one row; they occupy rows 4&5
  data_table[c(4,5),2] <- mean(data_table[c(4,5),2])
  data_table <- data.frame(data_table[c(1:4,6:7),c(1,2)])
  
  #Renaming the rows to be code-friendly
  data_table[1] <- c("fy1","fy2","reg","con","spec","staff")
  colnames(data_table) <- c("role","median_salary")
  
  #Adding a new column for the date and rearranging
  data_table$date <- c(NA,NA,NA,NA,NA,NA)
  data_table <- data_table[c(1,3,2)]
  
  #outputting end data frame, so it is use-able by map_df
  data_table
  #end of function
}


#Creating a vector that contains a list of all the files in my pdf folder
pdf_list <- list.files(here("data","raw","pdf"))

#creating df as a master df for all data from all pdfs
pdf_data <- data.frame(role=c("fy1","fy2","reg","con","spec","staff"),
                       date=c(NA,NA,NA,NA,NA,NA),
                       median_salary=c(NA,NA,NA,NA,NA,NA))

#For loop that works through the pdf files in my raw data
for (i in c(1:length(pdf_list))){
  
  #Extracting pdf from folder and turns it into a string of text
  raw_table <- map(here("data","raw","pdf",pdf_list[i]),pdf_text)
  
  #Applying data_extract function on raw data
  data_table <- map_df(raw_table,data_extract)
  
  #adding date to data table
  data_table$date <- sub(".pdf","",pdf_list[i])
  
  #Adding the looped data_table to pdf_data
  pdf_data <- rbind(pdf_data,data_table)
  
  if(i==length(pdf_list)){
    pdf_data <- pdf_data[-c(1:6),]
    rownames(pdf_data) <- c(1:length(pdf_data[,1]))
  }
  #End of loop
}

#Editing date format to be code-friendly, adding 01 to the front to be compatible with as.Date
pdf_data$date <- paste(pdf_data$date,"01",sep="-")
pdf_data$date <- as.Date(pdf_data$date,format="%Y-%m-%d")
pdf_data <- pdf_data[order(pdf_data$date),]


#Plotting the data
p_pdf <- ggplot(pdf_data,aes(x=date,y=median_salary,color=role))

p_pdf+geom_point()+
  geom_line(aes(group=role))


#Saving data to 'refined' data folder
write.csv(pdf_data,here("data","refined","pdf_data.csv"))
