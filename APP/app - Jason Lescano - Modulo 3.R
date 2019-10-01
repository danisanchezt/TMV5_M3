library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("ENEMDU - Empleo y Desempleo"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition = 'input.conditionalPanels=="Tabla"',
            numericInput("n", "Ingrese el numero de registro", value = "5", 
                         min = 2)
            ),
            conditionalPanel(condition = 'input.conditionalPanels=="Estadística"',
                             selectInput("df", "Seleccione una opción", 
                                         choices = c("Población", "Empleo", "Desempleo", "Satisfacción laboral"),
                                         selected = "Población")
            ),
            conditionalPanel(condition = 'input.conditionalPanels=="Histograma"',
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel( # crear pestanas
                tabPanel("Histograma",
                         plotOutput("distPlot")
                ),
                tabPanel("Estadística",
                         plotOutput("distPlotEst")
                ),
                tabPanel("Tabla",
                         DTOutput("tabla")
                ), id = "conditionalPanels"
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$tabla <- renderDT( base[1:input$n,])
            
    output$distPlotEst <- renderPlot({
        opc <- if(input$df=="Población"){
            base %>% 
                group_by(jovenes) %>% 
                summarise( Total = n()) %>% 
                mutate(Porc =  Total/sum(Total)*100) %>% 
                ggplot(aes(x=jovenes, y=Porc, fill=jovenes)) +
                geom_col() +
                theme_minimal() +
                geom_text(aes( y = round(Porc),
                               label =  paste(round(Porc), "%", sep = "")),
                          position = position_dodge(width = 0.9),
                          size = 3.5,
                          vjust = -0.5)
        } else{
            if(input$df=="Empleo"){    
        base %>% 
            group_by(jovenes, empleo) %>% 
            summarise(Empleados = n()) %>% 
            mutate(Porcentaje = Empleados/sum(Empleados)*100) %>% 
            ggplot(aes(x=empleo, y=Porcentaje, fill=empleo)) +
            geom_col() +
            theme_minimal() +
            facet_wrap(~jovenes) +
            geom_text(aes( y = round(Porcentaje),
                           label =  paste(round(Porcentaje), "%", sep = "")),
                      position = position_dodge(width = 0.9),
                      size = 3.5,
                      vjust = -0.5)
            
            } else{
                if(input$df=="Desempleo"){ 
                base %>% 
                    filter(desempleo=="Desempleado") %>% 
                    group_by(jovenes, sexo) %>% 
                    summarise(Desempleados = n()) %>% 
                    mutate(Porcentaje = Desempleados/sum(Desempleados)*100) %>% 
                    ggplot(aes(x=sexo, y=Porcentaje, fill=sexo)) +
                    geom_col() +
                    theme_minimal() +
                    facet_wrap(~jovenes) +
                    geom_text(aes( y = round(Porcentaje),
                                   label =  paste(round(Porcentaje), "%", sep = "")),
                              position = position_dodge(width = 0.9),
                              size = 3.5,
                              vjust = -0.5) 
                } else{
                    base %>% 
                        filter(satisf>0) %>% 
                        group_by(jovenes, sat_lab) %>% 
                        summarise(Total = n()) %>% 
                        ungroup() %>% 
                        group_by(jovenes) %>% 
                        mutate(Porcentaje = round(Total/sum(Total)*100,1)) %>% 
                        ggplot(aes(x=sat_lab, y=Porcentaje, fill=sat_lab)) +
                        geom_col() +
                        facet_wrap(~jovenes) +
                        theme_minimal() +
                        theme(axis.text.x = element_blank() ) +
                        geom_text(aes( y = round(Porcentaje, 1),
                                       label =  paste(round(Porcentaje, 1), "%", sep = "")),
                                  position = position_dodge(width = 0.9),
                                  size = 3.5,
                                  vjust = -0.5)
            }
          }
        }
        opc
    })    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        Edad    <- base$edad
        bins <- seq(min(Edad), max(Edad), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(Edad, breaks = bins, col = 'blue', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
