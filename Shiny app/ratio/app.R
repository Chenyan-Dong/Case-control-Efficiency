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

ratio1 <- read_csv("ratio11.csv")[,-1]
ratio2 <- read_csv("ratio12.csv")[,-1]
ratio4 <- read_csv("ratio14.csv")[,-1]
ratio5 <- read_csv("ratio15.csv")[,-1]
ratio <- read_csv("ratio.csv")[,-1]

ui2 <- fluidPage(
    
    # App title ----
    titlePanel("Case-control efficiency (Case-control ratio)"),
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
        
        p("Select the case-control ratio in a 10,000 population (assume control = 1000)."),
        
        # Input: Number of case interval ----
        sliderInput("ratio", "Case-control ratio = 1:",
                    min = 1, max = 5,
                    value = 1, step = 1),
        
        h3("Variables"),
        
        p(strong("Beta"), "is the coefficient of variable X."),
        p(strong("Type"), "is the type of estimator used where `MLE` represents maximum likelihood estimator
        and `IPW` represents sampling weights estimator."),
        p(strong("ScaledRMSE"), "is the root median squared error of variable x scaled by datasize"),
        p(strong("StandardError"), "is the estimated standard error (median)."),
        
        # Input: Choose dataset ----
        selectInput("dataset", "Choose a dataset with different case-control ratio:",
                    choices = c("1:1", "1:2", "1:4", "1:5")),
        
        # Button
        downloadButton("downloadData", "Download"),
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        
        tabsetPanel(
            
            tabPanel("Data Table", 
                     
                     p("Below is the table for relative performance of the maximum likelihood estimator (MLE) and
               design-based estimator (IPW)"),               
                     p(textOutput("table_intr")),
                     tableOutput("data_table")
            ),
            
            tabPanel("Visualize",
                     p(textOutput("plot_intr")),
                     plotOutput("RMSE", height = 300),
                     plotOutput("Var_estimate", height = 300)),
                     
            tabPanel("Compare",
                     plotOutput("RMSE_c", height = 320),
                     plotOutput("Var_estimate_c", height = 300)),
            
            tabPanel("Overall",
                     plotOutput("Var_estimate_o", height = 300)
            )
        ) 
    )
    
)

server2 <- function(input, output) {
    
    # Return the requested dataset ----
    datasetInput <- reactive({
        if (input$ratio == 1) return(ratio1)
        if (input$ratio == 2) return(ratio2)
        if (input$ratio == 3) return()
        if (input$ratio == 4) return(ratio4)
        if (input$ratio == 5) return(ratio5)
    })
    
    output$table_intr <- renderText({ 
        
        paste("The control number in a population with 10000 observation is 1000 and the case-control ratio should be
          1:", input$ratio, ". The case-control ratio subset should have", (input$ratio)*1000 + 1000,
              "observation with number of case", 1000/(input$ratio), ".")
        
    })
    
    # Generate a summary of the dataset ----
    output$data_table <- renderTable({
        dataset <- datasetInput()
        df <- dataset[, c(1:2, 9, 6)]
        data.frame(Beta = seq(2, -2, -0.5), ScaledRMSE_MLE = df$RMSE_scale[1:9],
                   StandardError_MLE = df$Estimates_se[1:9], ScaledRMSE_IPW = df$RMSE_scale[10:18],
                   StandardError_IPW = df$Estimates_se[10:18])
    }, digits = 3)
    
    output$plot_intr <- renderText({ 
        
        paste("The case number in a population with 10000 observation is", 1000/(input$ratio), ", 
    we random select the
    control number with case-cotrol ratio and the case-control subset with n =", (input$ratio)*1000 + 1000,
              ".")
        
    })  
    
    
    output$RMSE <- renderPlot({
        no <- 2
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        dataset <- datasetInput()
        ggplot(data = dataset, aes(x = Beta, y = RMSE_scale, colour = Type)) + geom_point() + geom_line() +
            scale_colour_manual(values = c("#D8A8A8", "#883028")) + theme_light() + theme(legend.position = "top") +
            xlab("Beta (coeffificent of x)") + ylab("Scaled Root Median Square Error")
    })
    
    output$Var_estimate <- renderPlot({
        no <- 2
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        dataset <- datasetInput()
        ggplot(data = dataset, aes(x = Beta, y = (Estimates_se)^2, colour = Type)) + geom_point() + geom_line() +
            scale_colour_manual(values = c("#D8A8A8", "#883028")) + theme_light() + theme(legend.position = "top") +
            xlab("Beta (coeffificent of x)") + ylab("Estimated variance")
    })


    
    output$RMSE_c <- renderPlot({
        
        no <- 4
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        ggplot(data = ratio, aes(x = Beta, y = RMSE_scale, color = as.factor(ratio), linetype = Type, 
                                 group = interaction(ratio, Type))) + geom_point() + geom_line() + 
            theme_light() + scale_colour_manual(values = mycolors) + scale_linetype_manual(values = c(1, 2)) + 
            theme(legend.position = "top") + xlab("Beta (coeffificent of x)") + 
            ylab("Scaled Root Median Square Error") + labs(color = "Case-control ratio")
        
    })
    
    output$Var_estimate_c <- renderPlot({
        
        no <- 4
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        ggplot(data = ratio, aes(x = Beta, y = (Estimates_se)^2, color = as.factor(ratio), linetype = Type, 
                                 group = interaction(ratio, Type))) + geom_point() + geom_line() + 
            theme_light() + scale_colour_manual(values = mycolors) + 
            scale_linetype_manual(values = c(1, 2)) + theme(legend.position = "top") + 
            xlab("Beta (coeffificent of x)") + ylab("Estimated variance") + labs(color = "Case-control ratio")
        
    })

    
    
    output$Var_estimate_o <- renderPlot({
        
        no <- 4
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        data_ipw <- ratio %>% filter(Type == "IPW")
        data_mle <- ratio %>% filter(Type == "MLE")
        df <- data.frame(Beta = seq(2,-2, -0.5), Estimates_se = data_ipw$Estimates_se/data_mle$Estimates_se,
                         ratio = rep(paste("1:", c(1, 2, 4, 5), sep = ""), each = 9))
        
        ggplot(data = df, aes(x = Beta, y = Estimates_se, color = as.factor(ratio))) + geom_point() + 
            geom_line(size = 0.8) + theme_light() + theme(legend.position = "top") +
            scale_color_manual(values = mycolors) + labs(color = "Case-control ratio") + 
            xlab("Beta (coeffificent of x)") + ylab("Standard error ratio")
        
    })
    
}

# Run the application 
shinyApp(ui = ui2, server = server2)
