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
#END_EDAD$grupo <- factor(END_EDAD$grupo, levels = unique(END_EDAD$grupo)[order(END$cantgrupo, decreasing = TRUE)])
END_RELIG=read.table("END_RELIG.csv", header=T, sep=",")
colnames(END_RELIG)[1]=c("id")
END_SECTOR=read.table("END_SECTOR.csv", header=T, sep=",")
colnames(END_SECTOR)[1]=c("id")
END_UA<- read.table("END_UA.csv", header=T, sep=",")
colnames(END_UA) <- c("Cat","Cant","Total")
END_UA$Cat <- ifelse(END_UA$Cat == "ua", "Usaba anticonceptivos","No usaba anticonceptivos")
END_UA$Tasa <- round(END_UA$Cant*100/END_UA$Total,2)
datosMujeresAdult <- read.table("datosMujeresAdult.csv", header=T, sep=",")
END_CRELIG <- read.table("END_R_E.csv", header=T, sep=",")




#'Uso de anticonceptivos',
#plotlyOutput('grafusoA')

ui=fluidPage (
  titlePanel("Trabajo final EANT"),
  tabsetPanel(
    tabPanel(strong('Ficha Técnica'),
             p(h4('Los datos fueron tomados de la Encuesta de Salud Sexual y Reproductiva del Ministerio de Salud realizada en 2013.')),
             a(h4(href="https://www.indec.gov.ar/bases-de-datos.asp?solapa=2","En este link se encuentran las bases de datos y documentos metodológicos")),
             p(h4('Usé la base de datos de mujeres, que recoge información acerca de la salud sexual y reproductiva de 5092 mujeres entre 13 a 49 años en centros urbanos de 2.000 o más habitantes.')),
             p(h4('La encuesta recoge por un lado datos demográficos de las mujeres entrevistadas: edad, tipo de hogar y características de la vivienda, máximo nivel educativo, situación conyugal, situación laboral, etc. ')),
             p(h4('Por el otro, recoge información sobre su actividad sexual y conocimiento/uso de métodos de anticoncepción, conocimiento y prácticas preventivas en torno a las infecciones de transmisión sexual, y embarazos y partos.'))
             ),
    tabPanel(strong('Preguntas'),
             p(h4("En base a los datos que proporcionaba esta encuesta quise trabajar alrededor de las imágenes de género existentes sobre de la maternidad. Culturalmente, relacionamos la situación de un embarazo no deseado a adolescentes que presumiblemente no se cuidaron, y no tienen los recursos económicos para criar un hijo.")),
             p(h4("Una mujer adulta, con el capital económico suficiente para criar un hijo, está lista para ser madre y por lo tanto debería serlo. El objetivo de este trabajo es mostrar que esta ideal de maternidad no se corresponde a la realidad.")),
             p(h4("Las preguntas que en principio me planteé son:")),
             p(h4(strong("-¿Los embarazos no deseados son solo una problemática adolescente?"))),
             p(h4(strong("-¿Los embarazos no deseados son solo una problemática de mujeres pobres?"))),
             p(h4(strong("-¿Los embarazos no deseados ocurren únicamente cuando no se toman medidas anticonceptivas?"))),
             p(h4(strong("-Ser parte de un grupo con un código cultural que -en principio- pone en primer lugar a la familia, ¿evita los embarazos no deseados?"))),
             p(h4("Para responder estas preguntas creé la variable tipo de embarazo, que clasifiqué en Deseado/No deseado."))
             ),
    tabPanel(strong('Respuestas'),
             navlistPanel('Analisis según:',
                          tabPanel('Edad',
                                   h2(strong("¿Los embarazos no deseados son solo una problemática adolescente?")),
                                   p("El primer punto es la edad. En base a las preguntas de edad de la encuestada y año de nacimiento de su último hijo vivo, calculé la edad a la que habían quedado embarazadas y clasifiqué en cuatro grupos etarios: de 13 a 19 años, 20 a 29 años, 30 a 39 años y 40 a 49 años."),
                                   br(),
                                   plotOutput('grafEdad'),
                                   br(),
                                   #tableOutput('tablaEdad'),
                                   br(),
                                   p("Del total de embarazos no deseados, el grupo que tiene más peso es el de las mujeres que quedaron embarazadas entre los 20 y 29 años. ")
                          ),
                          tabPanel('Clase',
                                   h2(strong("¿Los embarazos no deseados son solo una problemática de mujeres pobres?")),
                                   p(h2("Para responder esta pregunta, clasifiqué el sector económico en base a el máximo nivel educativo alcanzado por las mujeres (esto implicó dejar de lado a las adolescentes). De educación secundaria completa para abajo clasifiqué en Populares, y el resto Medio/Alto.")),
                                   p(h2("Después, creé una tasa de embarazos no deseados para cada grupo.")),
                                   plotOutput('grafClase')                                   #tableOutput('tablaClase'),
                          ),
                          tabPanel('Uso de anticonceptivos',
                            h2(strong("¿Los embarazos no deseados son solo resultado de no tomar medidas anticonceptivas?")),
                            p(h2()),
                            plotOutput('grafusoA')
                            ),
                          tabPanel('Religión',
                                   p(h2()),
                                   plotOutput('grafRelig'),
                                   plotOutput('grafRelig1')
                          )
             ))
    )
 
  )



server=function(input,output){
  
  output$grafEdad=renderPlot({
    
    gre<- ggplot(END_EDAD, aes(x= reorder(grupo,-cantgrupo),y=cantgrupo))+geom_bar(stat="identity",fill="salmon3")
    gre <- gre + geom_label(aes(x = reorder(grupo,-cantgrupo), y = cantgrupo, label = round(cantgrupo, 0)),
                       hjust = 0.5, 
                       vjust = 1, 
                       colour = "white", 
                       fill = NA, 
                       label.size = NA, 
                       size = 6)
    gre
    
      })
  
  
  output$grafClase=renderPlot({
    
    gr<-ggplot(END_SECTOR, aes(x=sector, y=tasa))+geom_bar(stat="identity",fill="salmon3",width=0.7)
    gr <- gr + scale_y_continuous(labels = function(x) paste0(x, "%"))
    gr <- gr+ geom_label(aes(x = sector, y = tasa, label = paste0(round(tasa, 2),"%")),
                 hjust = 0.5, 
                 vjust = 1, 
                 colour = "white", 
                 fill = NA, 
                 label.size = NA, 
                 size = 6)
    gr
                        
                        
  })
  
  output$grafClase1 = renderPlotly ({
    gr1<-ggplot(datosMujeresAdult, aes(sector))+geom_bar(aes(fill=factor(MEP14)),position = 'fill')+ theme(legend.position = "none")
    ggplotly(gr1)

  })
  
  output$grafusoA=renderPlot({
    
    graf<- ggplot(END_UA, aes(x= Cat,y= Tasa))+geom_bar(stat="identity",fill="salmon3")
    graf <- graf + scale_y_continuous(labels = function(x) paste0(x, "%"))+geom_label(aes(x = Cat, y = Tasa, label = paste0(round(Tasa, 0),"%")),
                            hjust = 0.5, 
                            vjust = 1, 
                            colour = "white", 
                            fill = NA, 
                            label.size = NA, 
                            size = 6)
    graf    
        })
  

  
  output$grafRelig=renderPlot({
    
ggplot(END_RELIG, aes(x=Categoria, y=Tasa))+geom_bar(stat="identity") + scale_y_continuous(labels = function(x) paste0(x, "%"))+geom_label(aes(x = Categoria, y = Tasa, label = paste0(round(Tasa, 0),"%")),
                                                                                                                                           hjust = 0.5, 
                                                                                                                                           vjust = 1, 
                                                                                                                                           colour = "white", 
                                                                                                                                           fill = NA, 
                                                                                                                                           label.size = NA, 
                                                                                                                                           size = 6)
    
  })
  
  
  output$grafRelig1=renderPlot({
    
grel1<-ggplot(END_CRELIG, aes(x=reorder(religion,-tasa),y=tasa))+geom_bar(stat='identity')
grel1<-grel1+scale_y_continuous(labels = function(x) paste0(x, "%"))
grel1<- grel1+geom_label(aes(x = religion, y = tasa, label = paste0(round(tasa, 0),"%")),
                         hjust = 0.5, 
                         vjust = 1, 
                         colour = "white", 
                         fill = NA, 
                         label.size = NA, 
                         size = 6)
grel1
  })
#  output$tablaEdad=renderTable({END_EDAD})
#  output$tablaClase=renderTable({END_SECTOR})

  
}


  shinyApp(ui = ui, server = server)
  
  