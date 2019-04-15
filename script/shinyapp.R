#shiny

rm(list=ls())

library(ggplot2)
library(plotly)
library(shiny)
setwd("C:/Users/perla/OneDrive/Documents/ssyr/data")
datosMujeres=read.table("ENSSyR_Mujeres_BaseUsuario.txt", header=T, sep="|")
END=read.table("END.csv", header=T, sep=",")
END_ADULT=read.table("END_ADULT.csv", header=T, sep=",")
END_EDAD=read.table("END_GRUPEDAD.csv", header=T, sep=",")
colnames(END_EDAD)=c("id","grupo","cantgrupo","cantidad total")
END_RELIG=read.table("END_RELIG.csv", header=T, sep=",")
END_SECTOR=read.table("END_SECTOR.csv", header=T, sep=",")
#'Uso de anticonceptivos',
#plotlyOutput('grafusoA')

ui=fluidPage (
  titlePanel("Trabajo final EANT"),
  tabsetPanel(
    tabPanel(strong('Introducción'),
             p(h5("En los últimos años se puso en la mesa el debate sobre la despenalización del aborto y la incidencia que tendría en el país. Las mujeres de sectores populares de la población, frente un embarazo no deseado, recurren a un aborto clandestino en condiciones que probablemente les conlleven complicaciones médicas o la muerte. Que el Estado sea garante de un aborto en condiciones seguras y de sanidad se corresponde a la obligación de este de garantizar el derecho a la salud de toda la población.")),
             p(h5("Ahora, cuando unx habla de un embarazo no deseado, generalmente se imagina el caso de una adolescente (que no se cuidó a la hora de tener relaciones), que es demasiado joven para ser madre y no tiene los recursos para criar un hijo. Una mujer adulta, con el capital económico suficiente para criar un hijo, está lista para ser madre y por lo tanto debería serlo. El objetivo de este trabajo es mostrar que esta ideal de maternidad impuesto a las mujeres no se corresponde a la realidad.")),
             p(h5("Los datos fueron tomados de la Encuesta de Salud Sexual y Reproductiva del Ministerio de Salud realizada en 2013. La pregunta clave fue: [Respecto al último hijo nacido vivo]")),
             p(h5(strong("Cuando quedó embarazada, ¿quería tener ese hijo, quería esperar más tiempo o no quería tener ese hijo? ")))
             ),
    tabPanel(strong('Gráficos'),
             navlistPanel('Analisis según:',
                          tabPanel('Edad',
                                   p(strong("Acá va a ir texto")),
                                   tableOutput('tablaEdad'),
                                   plotlyOutput('grafEdad')
                          ),
                          tabPanel('Clase',
                                   tableOutput('tablaClase'),
                                   plotlyOutput('grafClase')
                          ),
                          tabPanel('Uso de anticonceptivos',
                            plotOutput('grafusoA')

                          ),
                          tabPanel('Religión',
                                   plotlyOutput('grafRelig')
                          )
             ))
    )
 
  )



server=function(input,output){
  
  output$grafEdad=renderPlotly({
      plot_ly(END_EDAD, labels = ~grupo, values = ~cantgrupo, type = 'pie') %>%
      layout(title = "¿Los embarazos no deseados son solo una problemática adolescente?",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  output$grafClase=renderPlotly({
    plot_ly(END_SECTOR, labels = ~sector, values = ~tasa, type = 'pie') %>%
      layout(title = "¿Los embarazos no deseados son solo una problemática de mujeres pobres?",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$grafusoA=renderPlot({
    g3<-ggplot(END,aes(x="",y=MEP11, fill=factor(MEP14)))+geom_bar(width=1,stat="identity")
    g3<- g3 + coord_polar("y", start=0)
    g3    
    })
  
  output$grafRelig=renderPlotly({
    
    plot_ly(END_RELIG, labels = ~Categoria, values = ~Tasa, type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  output$tablaEdad=renderTable({END_EDAD})
  output$tablaClase=renderTable({END_SECTOR})

  
  # 6.15) El ultimo paso es el boton que escribe el archivo!

#  output$download_data <- downloadHandler(
#    filename = "pelis_data.csv",
#    content = function(file) {
#      data <- base()
#      write.csv(data, file, row.names = FALSE)
#    }
#  )
  
}


shinyApp(ui = ui, server = server)
