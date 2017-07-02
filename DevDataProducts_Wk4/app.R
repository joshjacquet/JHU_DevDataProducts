library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

data <- mtcars
data$cyl <- as.factor(data$cyl)
data$am <- as.factor(data$am)
data$gear <- as.factor(data$gear)
data$makemodel <- rownames(data)
data$make <- gsub( " .*$", "", data$makemodel)

ui <- navbarPage("Car Exploration",
                 tabPanel("Instructions", fluidPage(
                         htmlOutput("instructions"))),
                 
                 tabPanel("App", fluidPage(
        titlePanel(h1("Car Dataset Exploration")),
        sidebarLayout(
                sidebarPanel(
                        selectInput("make", "Car Make (Select as many as desired)"
                                    , choices = data$make, multiple = TRUE
                                    , selected = c("Mazda", "Toyota", "Ferrari", "Honda"))
                ),
                mainPanel(
                        tableOutput("cars"),
                        br(),
                        plotOutput("counts"),
                        br(),
                        plotOutput("MPGvHP"))
                )
        )
)
)

server <- function(input, output) {
        output$MPGvHP <- renderPlot({
                filtered <- data %>%
                        filter(make %in% input$make)
                ggplot(filtered, aes(x = hp, y = mpg, color = cyl, shape = am)) + geom_point(size = 7) + ggtitle("Horsepower vs MPG") + xlab("Horsepower") + ylab("Miles Per Gallon")
        })
        
        output$counts <- renderPlot({
                filtered <- data %>%
                        filter(make %in% input$make)
                ggplot(filtered, aes(hp, fill = cyl)) + geom_histogram(bins = 20) + xlab("Horsepower") + ylab("Count of Cars") + ggtitle("Count of Cars by Horsepower")
        })
        
        output$cars <- renderTable({
                filtered <- data %>%
                        filter(make %in% input$make) %>%
                        group_by(make) %>%
                        summarise(Avg_MPG = mean(mpg),
                                  Median_Cylinders = median(as.numeric(cyl)),
                                  Avg_Quarter_Mile = mean(qsec),
                                  Avg_Weight = mean(wt)) %>%
                        arrange(make)
                        print(filtered)
        })
        
        output$instructions <- renderText({
                readLines("instructions.html")
        })
        
}

shinyApp(ui = ui, server = server)