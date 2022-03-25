#I used this to test all my scripts prior to implementing them into my project


#Loading all necessary libraries
library(tidyverse)
library(here)
library(pdftools)
library(data.table)


#Creating function to extract salary data from .pdf file
pdf_extract <- function(raw){
  
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
  
  #Now I've merged the consultant salaries and will rename the rows, I don't need column 1
  salary <- data_table[c(2)]
  
  #Changing row and column titles to be code-friendly
  rownames(salary) <- c("fy1","fy2","reg","con","spec","staff")
  colnames(salary) <- "median_salary"
  
  #outputting end data frame, so it is use-able by map_df
  salary
  #end of function
}


#Creating a vector that contains a list of all the files in my pdf folder
pdf_list <- list.files(here("data","raw","pdf"))

#creating arbitrary df as a master df for all data from all pdfs
all_pdf_data <- data.frame(median_salary=c(NA,NA,NA,NA,NA,NA))
rownames(all_pdf_data) <- c("fy1","fy2","reg","con","spec","staff")


#For loop that works through the pdf files in my raw data
for (i in c(1:length(pdf_list))){
  
  #Extracting pdf from folder and turns it into a string of text
  raw_table <- map(here("data","raw","pdf",pdf_list[i]),pdf_text)
  
  #Applying pdf_extract function on raw data
  salary_data <- map_df(raw_table,pdf_extract)
  
  #Adding the looped salary_data to all_pdf_data
  all_pdf_data[,i] <- salary_data[,1]
  
  #changing the column names to match the data
  colnames(all_pdf_data)[i] <- sub(".pdf","",pdf_list[i])
}

#Saving data to 'refined' data folder
write.csv(all_pdf_data,here("data","refined","pdf_data.csv"),row.names=TRUE)


