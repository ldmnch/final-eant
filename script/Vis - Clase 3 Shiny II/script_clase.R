install.packages("shinythemes")
  library(shiny)
  library(ggplot2)
  library(gapminder)
#  library(shinythemes)
#  library(vembedr)
rm(list=ls())

df=gapminder

ui <- fluidPage(
#  themeSelector(),
#  theme = shinytheme("darkly"),
  titlePanel('AN�LISIS DE POBLACI�N'),
  sidebarLayout(
    sidebarPanel(
    
      radioButtons(inputId =  'continente',
                   label=h3('selecci�n de continente'),
                   choices = unique(df$continent),
                   selected=unique(df$continent)[1]),
      checkboxInput(inputId='masinfo',
                         label=strong('M�s info'),
                         value= FALSE),
      
      sliderInput(inputId='fecha',
                  label=h3('Per�odo'),
                  min = min(df$year),
                  max = max(df$year),
                  value= c(1990,2007)),
#      selectInput(inputid='pais',
#                 label= h3(strong('Elegir pa�s')),
#                 choices = NULL,
#                 selected= NULL),
      uiOutput("combo_pais"),
      helpText('Observaciones:')
),
 
    mainPanel(
      plotOutput("output_g1", click="plot_click"),
      verbatimTextOutput("output_coor"),
      textOutput("output_masinfo"),
      br(),
      verbatimTextOutput("sumario")
#      embed_youtube("video")
     
    )
  )
  
)

  
server <- function(input, output) {
    df.filt <- reactive({
      df.filt = df[
        df$country == input$pais  &
        df$year >= input$fecha [1] &
        df$year <= input$fecha [2],]
      df.filt
    })

    
    output$combo_pais <- renderUI ({
      selectInput(inputId = "pais", h3("Seleccionar el pa�s"),
                  choices=unique(
                    df[df$continent==input$continente,'country']),
                  selected=1)
  
    })

    
  output$output_g1 <- renderPlot({
    df.filt=df[
      df$country == input$pais  &
        df$year >= input$fecha [1] &
        df$year <= input$fecha [2],]
    
    ggplot(df.filt, aes(x=lifeExp,y=pop))+ geom_point(color="blue")
      labs(x="Expectativa de vida",y="Poblaci�n")
  })  
    
      } 

shinyApp(ui = ui, server = server)



