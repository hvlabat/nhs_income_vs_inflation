#I used this to test my shiny ui script prior to implementing it into my project


#Loading all necessary libraries
library(tidyverse)
library(here)
library(scales)
library(shiny)
library(shinyWidgets)
library(later)
library(profvis) #used this to optimise my graph to allow for better animation

#Note: This needs income_vs_inflation_script to be run first.

#Creating a shiny ui;
#Mapping p_group_cumulative alongside bar charts showing salary and ciph change

#Creating df with columns for change, date, and groups only, adding cpih to 'group' column
col_df <- data.frame(cpih_vs_salary$date,cpih_vs_salary$group,cpih_vs_salary$change)
colnames(col_df) <- c("date","group","change")

temp_cpih_df <- data.frame(cpih_vs_salary$date,cpih_vs_salary$cpih)
temp_cpih_df <- temp_cpih_df[c(1:148),]
temp_cpih_df$group <- "CPIH"
temp_cpih_df <- temp_cpih_df[,c(1,3,2)]
colnames(temp_cpih_df) <- c("date","group","change")

col_df <- rbind(col_df,temp_cpih_df)

#Testing the column graph, to later be added into shiny plot alongside p_group_cumulative
ggplot()+
  geom_col(data=col_df,aes(x=group,y=change,fill=group))+
  scale_fill_manual(values=c("#E69F00",
                             "#56B4E9",
                             "#009E73",
                             "#F0E442",
                             "#D55E00",
                             "#888888"))+
  theme_light()+
  labs(title="Change in Salary and CPIH",
       subtitle="From Sept 2009 to Dec 2021",
       fill="Group")+
  theme(plot.title=element_text(face="bold"),
        panel.grid.major=element_line(size=1,colour="grey85"),
        panel.grid.minor=element_line(colour="grey90"),
        axis.ticks=element_line(size=1))+
  xlab("Group")+
  ylab("Percentage Change")+
  scale_x_discrete(labels=c("CCT","ST","CT","FY2","FY1","CPIH"))+
  scale_y_continuous(limits=c(-8,38),
                     breaks=seq(-5,35,5),
                     minor_breaks=seq(-8,38,1),
                     expand=c(0,0))+
  theme(axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(vjust=2.5))





#Making the shinyApp for above geom_col

#Creating a list of months from the 148 available
choices_month <- as.Date(col_df$date[c(1:148)])

#User Interface defined as ui_col, with settings as desired
ui_col <- fluidPage(
  titlePanel(tags$span(style="color:#56B4E9",
                       "Comparing NHS Doctor Salary to CPIH")),
  titlePanel(tags$h4("From Sept 2009 to Dec 2021")),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput(inputId="date",
                  label="Select Date:",
                  choices=choices_month,
                  selected=choices_month[148],
                  animate=animationOptions(interval=500))
    ),
    mainPanel(
      plotOutput(outputId="col_plot")
    )
  )
)

#Server function to define plot
server_col <- function(input,output){
  
  #Reactive expression to streamline shiny
  dataInput <- reactive({
    req(input$date)
  })
  
  #Plotting, using geom_col
  output$col_plot <- renderPlot({
    ggplot()+
      geom_col(data=col_df %>% filter(date==input$date),aes(x=group,y=change,fill=group))+
      scale_fill_manual(values=c("#E69F00",
                                 "#56B4E9",
                                 "#009E73",
                                 "#F0E442",
                                 "#D55E00",
                                 "#888888"))+
      theme_light()+
      labs(title="Change in Salary and CPIH",
           subtitle=paste("Date:",format(as.Date(input$date),"%B %Y")),
           fill="Group")+
      theme(plot.title=element_text(face="bold"),
            panel.grid.major=element_line(size=1,colour="grey85"),
            panel.grid.minor=element_line(colour="grey90"),
            axis.ticks=element_line(size=1))+
      xlab("Group")+
      ylab("Percentage Change")+
      scale_x_discrete(labels=c("CCT","ST","CT","FY2","FY1","CPIH"))+
      scale_y_continuous(limits=c(-8,38),
                         breaks=seq(-5,35,5),
                         minor_breaks=seq(-8,38,1),
                         expand=c(0,0))+
      theme(axis.title.x = element_text(vjust=-0.35),
            axis.title.y = element_text(vjust=2.5))
  })
}

#Creating the app
shinyApp(ui=ui_col,server=server_col)




#Measuring what takes long for the app to load; it was the short interval time
profvis({
runApp(shinyApp(ui=ui_col,server=server_col))
})
#Changing interval time to 500 was sufficient

