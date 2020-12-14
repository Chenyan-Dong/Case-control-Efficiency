#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(rsconnect)


case50 <- read_csv("case50.csv")[, -1]
case100 <- read_csv("case100.csv")[, -1]
case150 <- read_csv("case50.csv")[, -1]
case200 <- read_csv("case200.csv")[, -1]
case250 <- read_csv("case250.csv")[, -1]
case300 <- read_csv("case300.csv")[, -1]
case350 <- read_csv("case350.csv")[, -1]
case400 <- read_csv("case400.csv")[, -1]
case450 <- read_csv("case450.csv")[, -1]
case500 <- read_csv("case500.csv")[, -1]
case550 <- read_csv("case550.csv")[, -1]
case600 <- read_csv("case600.csv")[, -1]
case650 <- read_csv("case650.csv")[, -1]
case700 <- read_csv("case700.csv")[, -1]
case750 <- read_csv("case750.csv")[, -1]
case800 <- read_csv("case800.csv")[, -1]
case850 <- read_csv("case850.csv")[, -1]
case900 <- read_csv("case900.csv")[, -1]
case950 <- read_csv("case950.csv")[, -1]
case1000 <- read_csv("case1000.csv")[, -1]
case <- read_csv("case.csv")


# Define UI for slider demo app ----
ui1 <- fluidPage(
    
    # App title ----
    titlePanel("Case-control efficiency (Number of cases)"),
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
        
        p("Select the number of case in a 10,000 population."),
        
        # Input: Number of case interval ----
        sliderInput("ncase", "Number of case:",
                    min = 50, max = 1000,
                    step = 50, value = 500),
        
        h3("Variables"),
        
        p(strong("Beta"), "is the coefficient of variable x."),
        p(strong("Type"), "is the type of estimator used where `MLE` represents maximum likelihood 
      estimator and `IPW` represents sampling weights estimator."),
        p(strong("ScaledRMSE"), "is the root median squared error of variable x scaled by datasize"),
        p(strong("StandardError"), "is the estimated standard error (median)."),
        p(strong("Bias"), "is difference between the true coefficient of variable x (`Beta`) 
      and the predicted coefficient of variable x."),
        p(strong("Estimates_se"), "is the median of standard error."),
        p(strong("Simulation_se"), "is the simulation standard error."),
        
        # Input: Choose dataset ----
        selectInput("dataset", "Choose a dataset with different case numbers:",
                    choices = c("case50", "case100", "cars150", "case200", "case250", "cars300",
                                "case350", "case400", "cars450", "case500", "case550", "cars600",
                                "case650", "case700", "cars750", "case800", "case850", "cars900",
                                "case950", "case1000")),
        
        # Button
        downloadButton("downloadData", "Download"),
        
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
        
        tabsetPanel(
            
            tabPanel("Data Table", 
                     p("Below is the table for relative performance of the maximum likelihood 
               estimator (MLE) and design-based estimator (IPW)"),               
                     p(textOutput("table_intr")),
                     tableOutput("data_table")
            ),
            
            tabPanel("Visualize",
                     p(textOutput("plot_intr")),
                     fluidRow(
                         
                         column(10, align = "center",
                                plotOutput("Bias", height = 200)
                         ),
                         
                         column(10, align = "center",
                                plotOutput("RMSE", height = 200)
                         ),
                         
                         column(10, align = "center",
                                plotOutput("Var_estimate", height = 200)
                         ))),
            
            tabPanel("Compare",
                     fluidRow(
                         
                         column(10, align = "center",
                                plotOutput("Bias_c", height = 300)
                         ),
                         
                         column(10, align = "center",
                                plotOutput("RMSE_c", height = 200)
                         ),
                         
                         column(10, align = "center",
                                plotOutput("Var_estimate_c", height = 200)
                         )))
        )  
    )
)


server1 <- function(input, output) {
    
    # Return the requested dataset ----
    datasetInput <- reactive({
        if (input$ncase == 50) return(case50)
        if (input$ncase == 100) return(case100)
        if (input$ncase == 150) return(case150)
        if (input$ncase == 200) return(case200)
        if (input$ncase == 250) return(case250)
        if (input$ncase == 300) return(case300)
        if (input$ncase == 350) return(case350)
        if (input$ncase == 400) return(case400)
        if (input$ncase == 450) return(case450)
        if (input$ncase == 500) return(case500)
        if (input$ncase == 550) return(case550)
        if (input$ncase == 600) return(case600)
        if (input$ncase == 650) return(case650)
        if (input$ncase == 700) return(case700)
        if (input$ncase == 750) return(case750)
        if (input$ncase == 800) return(case800)
        if (input$ncase == 850) return(case850)
        if (input$ncase == 900) return(case900)
        if (input$ncase == 950) return(case950)
        if (input$ncase == 1000) return(case1000)
    })
    
    output$table_intr <- renderText({ 
        
        paste("The case number in a population with 10000 observation is", input$ncase, 
              "and the case-control subset with n =", 2*(input$ncase), ".")
        
    })
    
    # Generate a summary of the dataset ----
    output$data_table <- renderTable({
      dataset <- datasetInput()
      df <- dataset[, c(1:2, 9, 6)]
      data.frame(Beta = seq(2, -2, -0.5), ScaledRMSE_MLE = df$RMSE_scale[1:9],
                 StandardError_MLE = df$Estimates_se[1:9],
                 ScaledRMSE_IPW = df$RMSE_scale[10:18], StandardError_IPW = df$Estimates_se[10:18])

    }, digits = 3)
    
    output$plot_intr <- renderText({ 
        
        paste("The case number in a population with 10000 observation is", input$ncase, ", 
    we random select the same control number as the case number and the case-control subset 
    with n =", 2*(input$ncase), ".")
        
    })  
    
    output$Bias <- renderPlot({
      no <- 2
      mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
      
        dataset <- datasetInput()
        ggplot(data = dataset, aes(x = Beta, y = Bias, colour = Type)) + geom_point() + geom_line() +
            scale_colour_manual(values = c("#D8A8A8", "#883028")) + theme_light() +
            xlab("Beta (coeffificent of x)") + ylab("Bias")
    })
    
    output$RMSE <- renderPlot({
      no <- 2
      mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
      
        dataset <- datasetInput()
        ggplot(data = dataset, aes(x = Beta, y = RMSE_scale, colour = Type)) + geom_point() + geom_line() +
            scale_colour_manual(values = c("#D8A8A8", "#883028")) + theme_light() +
            xlab("Beta (coeffificent of x)") + ylab("Scaled Root Median Square Error")
    })
    
    output$Var_estimate <- renderPlot({
      no <- 2
      mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
      
        dataset <- datasetInput()
        ggplot(data = dataset, aes(x = Beta, y = (Estimates_se)^2, colour = Type)) + geom_point() + geom_line() +
            scale_colour_manual(values = c("#D8A8A8", "#883028")) + theme_light() +
            xlab("Beta (coeffificent of x)") + ylab("Estimated variance")
    })
    
    
    output$Bias_c <- renderPlot({
        no <- 20
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        ggplot(data = case, aes(x = Beta, y = Bias, color = as.factor(case), linetype = Type, 
                                group = interaction(case, Type))) + geom_point() + geom_line() + 
            theme_light() + scale_color_manual(values = mycolors) + labs(color = "ncases") +
            scale_linetype_manual(values = c(1, 2)) + theme(legend.position = "top")
        
    })
    
    output$RMSE_c <- renderPlot({
        no <- 20
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        ggplot(data = case, aes(x = Beta, y = RMSE_scale, color = as.factor(case), linetype = Type, 
                                group = interaction(case, Type))) + geom_point() + geom_line() + 
            theme_light() + scale_color_manual(values = mycolors) + ylab("Scaled Root Median Square Error") +
            scale_linetype_manual(values = c(1, 2)) + theme(legend.position = "none") +
        xlab("Beta (coeffificent of x)")
        
        
    })
    
    output$Var_estimate_c <- renderPlot({
        no <- 20
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        ggplot(data = case, aes(x = Beta, y = (Estimates_se)^2, color = as.factor(case), linetype = Type, 
                                group = interaction(case, Type))) + geom_point() + geom_line() + 
            theme_light() + scale_color_manual(values = mycolors) + ylab("Estimated variance") + 
            scale_linetype_manual(values = c(1, 2)) + theme(legend.position = "none") +
          xlab("Beta (coeffificent of x)")
        
    })

    
}


shinyApp(ui1, server1)