#Doing the same but for cpih_vs_salary

#User Interface defined as ui_line, with settings as desired
ui_line <- fluidPage(
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
      plotOutput(outputId="line_plot")
    )
  )
)

#Server function to define plot
server_line <- function(input,output){
  
  #Reactive expression to streamline shiny
  dataInput <- reactive({
    req(input$date)
  })
  
  #Plotting, using geom_line
  output$line_plot <- renderPlot({
    ggplot()+
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
      labs(title="Cumulative Salary Lost to Inflation by NHS Doctors",
           subtitle=paste("Date:",format(as.Date(input$date),"%B %Y")),
           colour="Stage of Training")+
      theme(plot.title=element_text(face="bold"),
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
  })
}

#Creating the app
shinyApp(ui=ui_line,server=server_line)
