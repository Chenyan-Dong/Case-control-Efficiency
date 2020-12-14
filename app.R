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


uniform <- read_csv("uniform.csv")[,-1]
positive <- read_csv("positive.csv")[,-1]
negative1 <- read_csv("negative1.csv")[,-1]
negative2 <- read_csv("negative2.csv")[,-1]
data <- read_csv("barplot.csv")[,-1]

# Define UI for application 
ui <- fluidPage(
    
    # App title ----
    titlePanel("Case-control efficiency (Control distribution)"),
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
        
        p("Select the case-control ratio in a 10,000 population (assume case = 500)."),
        
        selectInput("pattern", "Control distribution:",
                    choices = c("uniform" = "uniform", 
                                "negatively-skewed" = "negatively-skewed",
                                "positively-skewed" = "positively-skewed",
                                "positively-skewed (strong)" = "positively-skewed (strong)"), 
                    selected = c("Uniform" = "Uniform")),
        
        p("Select the value of beta"),
        
        sliderInput("beta", "Beta1",
                    min = -1, max = 1,
                    step = 0.5, value = 1),
        
        h3("Variables"),
        
        p(strong("Beta"), "is the coefficient of variable X."),
        p(strong("Type"), "is the type of estimator used where `MLE` represents maximum likelihood estimator
        and `IPW` represents sampling weights estimator."),
        p(strong("RMSE"), "is the root median squared error of variable X"),
        p(strong("StandardError"), "is the estimated standard error (median)."),
        
        # Input: Choose dataset ----
        selectInput("dataset", "Choose a dataset with different control distribution (& case number):",
                    choices = c("uniform", "negatively-skewed", "positively-skewed", "positively-skewed (strong)")),
        
        # Button
        downloadButton("downloadData", "Download"),
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        
        tabsetPanel(
            
            tabPanel("Data Table", 
                     p("Below is the barplot which displayed the pattern of data for case and control."),
                     p(textOutput("barplot_intr")),
                     plotOutput("barplot", height = 300),
                     p("Below is the table for relative performance of the maximum likelihood estimator (MLE) and
               design-based estimator (IPW)"),               
                     p(textOutput("table_intr")),
                     tableOutput("data_table")
            ),
            
            tabPanel("Visualize",
                     plotOutput("Var_estimate", height = 300),
                     plotOutput("RMSE", height = 300)
            ),
            
            tabPanel("Overall",
                     plotOutput("Var_estimate_c", height = 310),
                     plotOutput("sd_estimate_o", height = 350))
        )
    )
)

# Define server 
server <- function(input, output) {
    
    # Return the requested dataset ----
    datasetInput <- reactive({
        if (input$pattern == "uniform") return(uniform)
        if (input$pattern == "negatively-skewed") return(positive)
        if (input$pattern == "positively-skewed") return(negative1)
        if (input$pattern == "positively-skewed (strong)") return(negative2)
    })
    
    
    output$barplot_intr <- renderText({ 
        
        paste("Now, the control distribution is", input$pattern, "distribution.")
        
    })
    
    output$table_intr <- renderText({ 
        
        paste("The case number in a population with 10000 observation is 500 and the control distribution 
          is", input$pattern, ".The case-control sampling subset should have 1000 observations.")
        
    })
    
    # Generate a summary of the dataset ----
    output$data_table <- renderTable({
        dataset <- datasetInput()
        df <- dataset[, c(1:2, 4, 6)]
        data.frame(Beta = seq(1, -1, -0.25), RMSE_MLE = df$RMSE[1:9],
                   StandardError_MLE = df$Estimates_se[1:9], RMSE_IPW = df$RMSE[10:18],
                   StandardError_IPW = df$Estimates_se[10:18])
    }, digits = 3)
    
    
    output$barplot <- renderPlot({
        
        if (input$pattern == "uniform") {
            beta <- input$beta
            control_prob <- rep(1/4, 4)
            case_prob <- control_prob * exp(beta * 1:4)
            scale_case_prob <- case_prob/sum(case_prob)
            df <- data.frame(type = rep(c("1.control", "2.case"), each = 4), 
                             x = rep(1:4, 2), prob = c(control_prob, scale_case_prob))
            return(ggplot(df, aes(x = x, y = prob, fill = type)) + geom_bar(stat = "identity", 
                                                                            position = position_dodge()) +
                       scale_fill_manual(values = c("#D8A8A8", "#883028")) + theme_minimal() + xlab("") + ylab(""))
        }
        
        if (input$pattern == "negatively-skewed") {
            beta <- input$beta
            control_prob <- seq(0.1, 0.4, 0.1)
            case_prob <- control_prob * exp(beta * 1:4)
            scale_case_prob <- case_prob/sum(case_prob)
            df <- data.frame(type = rep(c("1.control", "2.case"), each = 4), x = rep(1:4, 2),
                             prob = c(control_prob, scale_case_prob))
            return(ggplot(df, aes(x = x, y = prob, fill = type)) + geom_bar(stat = "identity", 
                                                                            position = position_dodge()) +
                       scale_fill_manual(values = c("#D8A8A8", "#883028")) + theme_minimal() + xlab("") + ylab(""))
        }
        
        if (input$pattern == "positively-skewed") {
            beta <- input$beta
            control_prob <- seq(0.4, 0.1, -0.1)
            case_prob <- control_prob * exp(beta * 1:4)
            scale_case_prob <- case_prob/sum(case_prob)
            df <- data.frame(type = rep(c("1.control", "2.case"), each = 4), x = rep(1:4, 2),
                             prob = c(control_prob, scale_case_prob))
            return(ggplot(df, aes(x = x, y = prob, fill = type)) + geom_bar(stat = "identity", 
                                                                            position = position_dodge()) +
                       scale_fill_manual(values = c("#D8A8A8", "#883028")) + theme_minimal() + xlab("") + ylab(""))
        }
        
        if (input$pattern == "positively-skewed (strong)") {
            beta <- input$beta
            control_prob <- exp(seq(0.4, 0.1, -0.1)) - 1
            case_prob <- control_prob * (beta * 1:4)
            scale_case_prob <- case_prob/sum(case_prob)
            df <- data.frame(type = rep(c("1.control", "2.case"), each = 4), x = rep(1:4, 2),
                             prob = c(control_prob, scale_case_prob))
            return(ggplot(df, aes(x = x, y = prob, fill = type)) + geom_bar(stat = "identity", 
                                                                            position = position_dodge()) +
                       scale_fill_manual(values = c("#D8A8A8", "#883028")) + theme_minimal() + xlab("") + ylab(""))
        }
        
        
    })
    
    output$Var_estimate <- renderPlot({
        no <- 2
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        dataset <- datasetInput()
        ggplot(data = dataset, aes(x = Beta, y = (Estimates_se)^2, colour = Type)) + geom_point() + geom_line() +
            scale_colour_manual(values = c("#D8A8A8", "#883028")) + theme_light() + theme(legend.position = "top") +
            xlab("Beta (coeffificent of x)") + ylab("Estimated variance")
    })
    
    output$RMSE <- renderPlot({
        no <- 2
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        dataset <- datasetInput()
        ggplot(data = dataset, aes(x = Beta, y = RMSE, colour = Type)) + geom_point() + geom_line() +
            scale_colour_manual(values = c("#D8A8A8", "#883028")) + theme_light() + theme(legend.position = "top") +
            xlab("Beta (coeffificent of x)") + ylab("Scaled Root Median Square Error")
        
    })
    
    output$Var_estimate_c <- renderPlot({
        
        no <- 4
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        data <- data
        ggplot(data = data, aes(x = Beta, y = (Estimates_se)^2, color = as.factor(group), linetype = Type,
                                group = interaction(group, Type))) + 
            geom_point(size = 2) + geom_line(size = 0.8) + theme_light() + theme(legend.position = "none") + 
            scale_colour_manual(values = c("#F7B698", "#A7CFE4", "#67001F", "#053061")) + ylab("Estimated variance")
        
    })
    
    output$sd_estimate_o <- renderPlot({
        
        no <- 4
        mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(no)
        
        data <- data
        data_ipw <- data %>% filter(Type == "IPW")
        data_mle <- data %>% filter(Type == "MLE")
        df <- data.frame(Beta = data_ipw$Beta,
                         Estimates_se = data_ipw$Estimates_se/data_mle$Estimates_se,
                         group = rep(c("uniform", "negatively-skewed", "positively-skewed", "positively-skewed (strong)"), each = 9))
        ggplot(data = df, aes(x = Beta, y = Estimates_se, color = as.factor(group))) + 
            geom_point(size = 2) + geom_line(size = 0.8) + theme_light() + theme(legend.position = "top") + 
            scale_colour_manual(values = mycolors) + labs(color = "Control distribution") + 
            ylab("Standard error ratio")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
