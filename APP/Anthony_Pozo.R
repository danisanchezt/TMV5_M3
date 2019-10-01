library(shiny)
library(DT)
library(readxl)
Deuda_Externa <- read_excel("Deuda Externa.xlsx")
Deuda_Total <- read_excel("Deuda Total.xlsx")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Deuda Publica del Ecuador"),
    
    # Sidebar with a slider input for number of bins
    
    fluidRow(
        column(3,
               
               
               #sidebarLayout(
               #sidebarPanel(
               
               conditionalPanel(condition = 'input.ConditionalPanels=="Texto"',
                                textInput("texto","Ingrese un Comentario"),
                                actionButton(inputId = "Boton",label = "Ejecutar")
                                ),
               
               
               conditionalPanel(condition = 'input.ConditionalPanels=="Tabla"',
                                selectInput(inputId = "df",label = "Seleccione el escenario",choices = c("Deuda Total","Deuda Externa"),
                                            selected = "Deuda Total"),numericInput(inputId = "n",label = "Ingrese el numero de registros",
                                                                              value = 1,min = 1,max = 20)),
               
               conditionalPanel(condition = 'input.ConditionalPanels=="Tendencia"')
               #),
        ),    
        
        column(7,
               # Show a plot of the generated distribution
               #mainPanel(
               tabsetPanel(
                   tabPanel(title = "Texto",
                            #conditionalPanel(condition = 'is.na(as.numeric(input.texto))',
                            textOutput("texto_salida"),
                            textOutput("texto_salida2"),
                            verbatimTextOutput("nchar")),
                   
                   tabPanel(title ="Tabla",DTOutput("tabla")
                   ),
                   
                   tabPanel("Documento",
                            htmlOutput("Documento")
                   ),
                   
                   tabPanel(title = "Tendencia",plotOutput("distPlot")
                   ), id="ConditionalPanels"
               )
        ),
        column(2,
               img(src="images.jpg",height=180,width=185)
               
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    texto_salida<-eventReactive(input$Boton,{
        paste("El texto ingresado es",input$texto) 
    })
    
  
    output$texto_salida2<-renderText({
        paste("Bien",texto_salida())
        
    })
    
    output$nchar<- renderText(nchar(input$texto))
    
    output$tabla<-renderDT({
        tmp <- if(input$df=="Deuda Total"){
            Deuda_Total 
        } else{Deuda_Externa}
        
        tmp[1:input$n,]
    },
    options = list(lengthChange = TRUE,
                   scrollY = 360,
                   scroller = TRUE,
                   pageLength = 20)
    )
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- Deuda_Total$`% PIB`
        y   <- Deuda_Total$Periodos
        # draw the histogram with the specified number of bins
        plot(y,x,type = "l", col = 'red',xlab = "Periodo",ylab = "% PIB",main = "Deuda Publica Total")
    })
    
    output$Documento<-renderText({
        return(paste('<iframe style="height:600px;width:100%"src="',
                     "Deuda Publica 2018.pdf",'"></iframe>',sep = ""))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
