#I used this to test my final shiny ui script prior to implementation


#Loading all necessary libraries
library(tidyverse)
library(here)
library(scales)
library(shiny)
library(shinyWidgets)
library(later)
library(ggpubr)


#Making the shinyApp for combined col_df and cpih_vs_salary data frames

#Creating df with columns for change, date, and groups only, adding cpih to 'group' column
col_df <- data.frame(cpih_vs_salary$date,cpih_vs_salary$group,cpih_vs_salary$salary_change)
colnames(col_df) <- c("date","group","change")

temp_cpih_df <- data.frame(cpih_vs_salary$date,cpih_vs_salary$cpih)
temp_cpih_df <- temp_cpih_df[c(1:148),]
temp_cpih_df$group <- "CPIH"
temp_cpih_df <- temp_cpih_df[,c(1,3,2)]
colnames(temp_cpih_df) <- c("date","group","change")

col_df <- rbind(col_df,temp_cpih_df)

#Creating a list of months from the 148 available
choices_month <- as.Date(col_df$date[c(1:148)])

#User Interface defined as ui, with settings as desired
ui <- fluidPage(
  titlePanel(tags$span(style="color:#56B4E9",
                       "Comparing NHS Doctor Salary to CPIH")),
  titlePanel(tags$h4("From Sept 2009 to Dec 2021")),
  wellPanel(
    sliderTextInput(inputId="date",
                    label="Select Date:",
                    choices=choices_month,
                    selected=choices_month[148],
                    animate=animationOptions(interval=800))
  ),
  mainPanel(plotOutput(outputId="plot",inline=TRUE)
  )
)

#Server function to define plot
server <- function(input,output){
  
  #Reactive expression to streamline shiny
  dataInput <- reactive({
    req(input$date)
  })
  
  #Plotting both cpih_vs_salary and col_df onto output$plot using ggarrange
  output$plot <- renderPlot({
    
    
    #Plotting cumulative salary loss, using cpih_vs_salary & geom_line
    loss_plot <- ggplot()+
      geom_line(data=cpih_vs_salary %>% filter(date<=input$date),
                aes(x=date,
                    y=cumulative_income_lost,
                    colour=group),size=1)+
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
      labs(title="Cumulative Salary Lost to Inflation",
           subtitle=paste("Date:",format(as.Date(input$date),"%b %Y"),"; Sept 2009 = 0"))+
      theme(plot.title=element_text(colour="#56B4E9",face="bold"),
            panel.grid.major=element_line(size=1,colour="grey85"),
            panel.grid.minor=element_line(colour="grey90"),
            axis.title.x = element_text(vjust=-0.35),
            axis.title.y = element_text(vjust=2.5),
            legend.position="none", #remove legend, since col_plot has one already
            axis.ticks=element_line(size=1))+ #to match major grid size
      xlab("Date")+
      ylab("Cumulative Salary Loss")+
      scale_colour_manual(values=c("#E69F00",
                                   "#56B4E9",
                                   "#009E73",
                                   "#F0E442",
                                   "#D55E00")) #compatible for colour-blind people
      
    
    
    #Plotting monthly % changes in salary and inflation, using col_df & geom_col
    col_plot <- ggplot()+
      geom_col(data=col_df %>% filter(date==input$date),
               aes(x=group,y=change,fill=group))+
      scale_fill_manual(values=c("#E69F00",
                                 "#56B4E9",
                                 "#009E73",
                                 "#F0E442",
                                 "#D55E00",
                                 "#888888"))+
      theme_light()+
      labs(title="Change in Salary and CPIH",
           subtitle=paste("Date:",format(as.Date(input$date),"%b %Y"),"; Sept 2009 = 0"),
           fill="Group")+
      theme(plot.title=element_text(colour="#56B4E9",face="bold"),
            panel.grid.major=element_line(size=1,colour="grey85"),
            panel.grid.minor=element_line(colour="grey90"),
            axis.ticks=element_line(size=1),
            axis.title.x = element_text(vjust=-0.35),
            axis.title.y = element_text(vjust=2.5))+
      xlab("Group")+
      ylab("Percentage Change")+
      scale_x_discrete(labels=c("CCT","ST","CT","FY2","FY1","CPIH"))+
      scale_y_continuous(limits=c(-8,38),
                         breaks=seq(-5,35,5),
                         minor_breaks=seq(-8,38,1),
                         expand=c(0,0))
    
    
    
    #Plotting real vs inflation-matched salary, using cpih_vs_salary & geom_line
    match_plot <- ggplot()+
      geom_line(data=cpih_vs_salary %>% filter(date<=input$date),
                aes(x=date,
                    y=amount,
                    colour=group),
                size=1)+
      geom_line(data=cpih_vs_salary,
                aes(x=date,
                    y=cpih_matched_salary,
                    colour=group),
                size=1,
                linetype=2)+
      theme_light()+
      scale_y_continuous(labels=dollar_format(prefix="£"),
                         breaks=seq(2000,9000,1000),
                         minor_breaks=seq(1000,10000,250),
                         limits=c(1000,10000),
                         expand=c(0,0))+
      scale_x_date(date_breaks="1 year",
                   date_minor_breaks="3 months",
                   date_labels="%Y",
                   limits=as.Date(c("2009-07-01","2022-06-15")),
                   expand=c(0,0))+
      labs(title="Inflation-Matched vs Real Salary",
           subtitle=paste("Date:",format(as.Date(input$date),"%b %Y"),
                          ";Dashed = Inflation-Matched, Solid = Real Salary"),
           colour="Stage of Training")+
      theme(plot.title=element_text(colour="#56B4E9",face="bold"),
            panel.grid.major=element_line(size=1,colour="grey85"),
            panel.grid.minor=element_line(colour="grey90"),
            axis.title.x = element_text(vjust=-0.35),
            legend.position="none", #remove legend, since col_plot has one already
            axis.title.y = element_text(vjust=2.5), #shifting axis titles to give space
            axis.ticks=element_line(size=1))+ #to match major grid size
      xlab("Date")+
      ylab("Monthly Salary")+
      scale_colour_manual(values=c("#E69F00",
                                   "#56B4E9",
                                   "#009E73",
                                   "#F0E442",
                                   "#D55E00")) #compatible for colour-blind people
    
    
    
    #Using ggarrange to merge the plots into one
    ggarrange(loss_plot,match_plot,col_plot,ncol=3,nrow=1)
  })
}

#Creating the app
shinyApp(ui,server)
