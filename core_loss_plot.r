#GOALS FOR THIS WEEK
#Change the plot to a scatterplot, with guage as the x axis and core loss as the y axis
#Some Manufacturers are showing up without a gauge, and thus are not plotted (WISCO, first numbers of product name will be gauge)
#WISCO having issues with their Gauge's as well. Seems like the whole wisco bunch may just have N/A for our Gauge values
library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("TableauTest.csv")

for(row in data){
  if(is.na(data$Gauge) && data$Manufacturer == "WISCO"){
    data$Gauge <- as.numeric("."+ substr(data$Product, start = 1, stop = 2))
  }
}

#frequencies: 50, 60, 100, 120, 150, 200, 400,, 500, 600, 700, 800, 1000, 1600, 2000, 2500, 4000, 5000
#10000, 20000, 50000


#shiny Ui stuff follows here
ui <- fluidPage(
  titlePanel("Core Loss"),
  sidebarLayout(
    sidebarPanel(checkboxGroupInput("gaugeInput", "Desired Guages", 
                                    choices = list( "Gauge: .2 mm" = .2,
                                                    "Gauge: .27" = .27,
                                                    "Gauge: .3" = .3,
                                                    "Gauge: .35" = .35,
                                                    "Gauge: .5" = .5,
                                                    "Gauge: .65" = .65)
    ),
                selectInput("frequencyInput", label = "Frequency Hz",
                             choices = list("50" = 50, 
                                            "60" = 60,
                                            "100" = 100,
                                            "120" = 120,
                                            "150" = 150,
                                            "200" = 200,
                                            "400" = 400,
                                            "500" = 500,
                                            "600" = 600,
                                            "700" = 700,
                                            "800" = 800,
                                            "1000" = 1000,
                                            "1600" = 1600,
                                            "2000" = 2000,
                                            "2500" = 2500,
                                            "4000" = 4000,
                                            "5000" = 5000,
                                            "10000" = 10000,
                                            "20000" = 20000,
                                            "50000" = 50000,
                                            "All" = "All"),
                            selected = "All"
                            ),
    
                selectInput("manufacturerInput", label = "Manufacturer",
                            choices = c("Thyssenkrupp",
                                        "China Steels",
                                        "Posco",
                                        "WISCO",
                                        "Arnold Magnetics Cogent",
                                        "AK Steel", 
                                        "Voestelpine",
                                        "JFE",
                                        "ArcelorMittal",
                                        "Nippon Steels",
                                        "All"),
                            selected = "All"
                            ),
    
                sliderInput("teslaInput", "Teslas", min = 0.00, max = 2.0, value = c(.5, 1.73), step = .01)
                 
      
    ),
    mainPanel(plotOutput("coreLossPlot"),
              br(),
              br(),
              plotOutput("coreLossScattr"))
  )
)

server <- function(input, output){
  output$coreLossPlot <- renderPlot({
    filtered <- 
      data %>%
      filter(Gauge %in% input$gaugeInput,
             B..Tesla. >= input$teslaInput[1],
             B..Tesla. <= input$teslaInput[2],
             if(input$manufacturerInput != "All"){
               Manufacturer == input$manufacturerInput
             }
             else{
               Manufacturer == data$Manufacturer
             },
             Frequency..Hz. == input$frequencyInput
             )
    ggplot(filtered, aes(Loss..W.kg.), step = .1)+
      geom_histogram(aes(fill = as.factor(Manufacturer)))+ scale_x_discrete(name = "Core Loss in Kg")
  })
  
  output$coreLossScattr <- renderPlot({
    filtered <-
      data %>%
      filter(B..Tesla. >= input$teslaInput[1],
             B..Tesla. <= input$teslaInput[2],
             if(input$manufacturerInput != "All"){
               Manufacturer == input$manufacturerInput
             }
             else{
               Manufacturer == data$Manufacturer
             },
             if(input$frequencyInput != "All"){
               Frequency..Hz. == input$frequencyInput
             }
             else{
               Frequency..Hz. == data$Frequency..Hz #Problems here, find better way to do this
             }
      )
    ggplot(filtered, aes(x = Gauge, y = Loss..W.kg., color = Manufacturer, shape = Manufacturer)) +
      geom_point()+
      xlab("Sample Gauge")+
      ylab("Core Loss in kg")
  })
}

shinyApp(ui = ui, server = server)
