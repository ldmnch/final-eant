#shiny

rm(list=ls())

library(ggplot2)
library(plotly)
library(shiny)
setwd("C:/Users/perla/OneDrive/Documents/ssyr/data")
datosMujeres=read.table("ENSSyR_Mujeres_BaseUsuario.txt", header=T, sep="|")
END=read.table("END.csv", header=T, sep=",")
END$MEP14= ifelse(END$MEP14 == 1, "Sí",
                           ifelse(END$MEP14 == 2, "No", NA))
END_ADULT=read.table("END_ADULT.csv", header=T, sep=",")
END_EDAD=read.table("END_GRUPEDAD.csv", header=T, sep=",")
colnames(END_EDAD)=c("id","grupo","cantgrupo","total")
END_RELIG=read.table("END_RELIG.csv", header=T, sep=",")
colnames(END_RELIG)[1]=c("id")
END_SECTOR=read.table("END_SECTOR.csv", header=T, sep=",")
colnames(END_SECTOR)[1]=c("id")
END_UA<- read.table("END_UA.csv", header=T, sep=",")
colnames(END_UA) <- c("Cat","Cant","Total")
END_UA$Cat <- ifelse(END_UA$Cat == "ua", "Usaba anticonceptivos","No usaba anticonceptivos")
datosMujeresAdult <- read.table("datosMujeresAdult.csv", header=T, sep=",")
#'Uso de anticonceptivos',
#plotlyOutput('grafusoA')

ui=fluidPage (
  titlePanel("Trabajo final EANT"),
  tabsetPanel(
    tabPanel(strong('Introducción'),
             p(h5("En los últimos años se puso en la mesa el debate sobre la despenalización del aborto y la incidencia que tendría en el país. Las mujeres de sectores populares de la población, frente un embarazo no deseado, recurren a un aborto clandestino en condiciones que probablemente les conlleven complicaciones médicas o la muerte. Que el Estado sea garante de un aborto en condiciones seguras y de sanidad se corresponde a la obligación de este de garantizar el derecho a la salud de toda la población.")),
             p(h5("Ahora, cuando unx habla de un embarazo no deseado, generalmente se imagina el caso de una adolescente (que no se cuidó a la hora de tener relaciones), que es demasiado joven para ser madre y no tiene los recursos para criar un hijo. Una mujer adulta, con el capital económico suficiente para criar un hijo, está lista para ser madre y por lo tanto debería serlo. El objetivo de este trabajo es mostrar que esta ideal de maternidad impuesto a las mujeres no se corresponde a la realidad.")),
             p(h5("Los datos fueron tomados de la Encuesta de Salud Sexual y Reproductiva del Ministerio de Salud realizada en 2013. La pregunta clave fue: [Respecto al último hijo nacido vivo]")),
             p(h5(strong("Cuando quedó embarazada, ¿quería tener ese hijo, quería esperar más tiempo o no quería tener ese hijo? ")),
             p(h5("Las opciones de respuesta eran:")),
             p(h5("1)	Quería tenerlo")),
             p(h5("2)	Quería esperar")),
             p(h5("3)	No quería hijos/Más hijos")),
             p(h5("Clasifiqué las unidades de análisis en Embarazo deseado si habían contestado 1, y en Embarazo no deseado si habían contestado 2 o 3. ")))
             ),
    tabPanel(strong('Gráficos'),
             navlistPanel('Analisis según:',
                          tabPanel('Edad',
                                   h2(strong("¿Los embarazos no deseados son solo una problemática adolescente?")),
                                   p("El primer punto es la edad. En base a las preguntas de edad de la encuestada y año de nacimiento de su último hijo vivo, calculé la edad a la que habían quedado embarazadas y clasifiqué en cuatro grupos:"),
                                   br(),
                                   tableOutput('tablaEdad'),
                                   br(),
                                   plotOutput('grafEdad'),
                                   br(),
                                   p("Del total de embarazos no deseados, el grupo que tiene más peso es el de las mujeres que quedaron embarazadas entre los 20 y 29 años. ")
                          ),
                          tabPanel('Clase',
                                   h2(strong("¿Los embarazos no deseados son solo una problemática de mujeres pobres?")),
                                   p(""),
                                   tableOutput('tablaClase'),
                                   plotlyOutput('grafClase'),
                                   plotOutput('grafClase1')
                          ),
                          tabPanel('Uso de anticonceptivos',
                            plotlyOutput('grafusoA')
                          ),
                          tabPanel('Religión',
                                   plotlyOutput('grafRelig')
                          )
             ))
    )
 
  )



server=function(input,output){
  
  output$grafEdad=renderPlot({
    
    ggplot(END_EDAD, aes(x= reorder(grupo,-cantgrupo),y=cantgrupo))+geom_bar(stat="identity")
    
  })
  
  
  output$grafClase=renderPlotly({
    plot_ly(END_SECTOR, labels = ~sector, values = ~tasa, type = 'pie') %>%
      layout(
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$grafClase1 = renderPlot ({
    ggplot(datosMujeresAdult, aes(sector))+geom_bar(aes(fill=factor(MEP14)),position = 'dodge')

  })
  
  output$grafusoA=renderPlotly({
    plot_ly(END_UA, labels = ~Cat, values = ~Cant, type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
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
