library(shiny)
library(ggplot2)
library(gapminder)
library(shinythemes)
library(vembedr)
rm(list=ls())

df=gapminder

ui <- fluidPage(
  #themeSelector(),
  # theme = shinytheme("____________"),
  titlePanel('TITULO'),
  sidebarLayout(
    
    sidebarPanel(
      
      # radioButtons(inputId='input_continente', .....),
      
      # checkboxInput(inputId='input_masinfo', ..... ),
      
      # sliderInput("input_fecha", label = h3("____________") .....),
      
      
      # uiOutput("combo_pais"),
      # helpText()
      
      
      ),
    mainPanel(
      # plotOutput('output_g1', click = "plot_click"),
      # verbatimTextOutput("output_coor"),
      # textOutput("output_masinfo"),
      # br(),  #Esto es un espacio
      # verbatimTextOutput('summary'),
      # embed_youtube("_____________")
      
    )
    )
  
  )


server <- function(input, output) {
#  df.filt <- reactive({
#    df.filt= ......
#    df.filt
#  })
  
#  output$combo_pais <- renderUI({
#    selectInput(inputId="output_pais", .... )
#  })
  
  
#  output$output_g1=____________({
#    ggplot(df.filt(),aes(x=_______,y=_________))+
#      geom_point(size=______,color=________)+
#      geom_line(color=___________)+
#      labs(x='__________', y='___________',
#           title='_____________________',
#           caption='_____________________' )
#    
#  })
  
#  output$output_coor <- _______________({
#    paste0()
#  })
  
#  output$output_masinfo <- _____________({
#    
#    if(____________==TRUE){
#      prom=mean(__________________)
#      paste0('GdpPercap primedio es: ',round(prom,2))
#    }
#    
#  })
  
  
#  output$summary=renderPrint({
#    summary(__________________________)
#  })
  
  
} 

shinyApp(ui = ui, server = server)



