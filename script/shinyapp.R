#shiny

rm(list=ls())

library(ggplot2)
library(plotly)
library(shiny)
setwd("C:/Users/perla/OneDrive/Documents/ssyr/data")
datosMujeres=read.table("ENSSyR_Mujeres_BaseUsuario.txt", header=T, sep="|")
END=read.table("END.csv", header=T, sep=",")
END$MEP14= ifelse(END$MEP14 == 1, "S�",
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
    tabPanel(strong('Ficha T�cnica'),
             #HACER DE NUEVO
             p(h4('Encuesta de Salud Sexual y Reproductiva del Ministerio de Salud.')),
             p(h4("A�o: 2013")),
             a(h4(href="https://www.indec.gov.ar/bases-de-datos.asp?solapa=2","Link")),
             p(h4('Muestra: 5092 mujeres entre 13 a 49 a�os de centros urbanos de 2.000 o m�s habitantes a nivel nacional.')),
             p(h4('Datos demogr�ficos (edad, tipo de hogar y caracter�sticas de la vivienda, m�ximo nivel educativo, situaci�n conyugal, situaci�n laboral, etc.) y sobre su actividad sexual y conocimiento/uso de m�todos de anticoncepci�n, conocimiento y pr�cticas preventivas en torno a las infecciones de transmisi�n sexual, y embarazos y partos.'))
             ),
    tabPanel(strong('Preguntas'),
             p(h4("En base a los datos que proporcionaba esta encuesta quise trabajar alrededor de las im�genes de g�nero existentes sobre de la maternidad. Culturalmente, relacionamos la situaci�n de un embarazo no deseado a adolescentes que presumiblemente no se cuidaron, y no tienen los recursos econ�micos para criar un hijo.")),
             p(h4("Una mujer adulta, con el capital econ�mico suficiente para criar un hijo, est� lista para ser madre y por lo tanto deber�a serlo. El objetivo de este trabajo es mostrar que esta ideal de maternidad no se corresponde a la realidad.")),
             p(h4("Las preguntas que en principio me plante� son:")),
             p(h4(strong("-�Los embarazos no deseados son solo una problem�tica adolescente?"))),
             p(h4(strong("-�Los embarazos no deseados son solo una problem�tica de mujeres pobres?"))),
             p(h4(strong("-�Los embarazos no deseados ocurren �nicamente cuando no se toman medidas anticonceptivas?"))),
             p(h4(strong("-Ser parte de un grupo con un c�digo cultural que -en principio- pone en primer lugar a la familia, �evita los embarazos no deseados?"))),
             p(h4("Para responder estas preguntas cre� la variable tipo de embarazo, que clasifiqu� en Deseado/No deseado."))
             ),
    tabPanel(strong('Respuestas'),
             navlistPanel('Analisis seg�n:',
                          tabPanel('Edad',
                                   h2(strong("�Los embarazos no deseados son solo una problem�tica adolescente?")),
                                   p("El primer punto es la edad. En base a las preguntas de edad de la encuestada y a�o de nacimiento de su �ltimo hijo vivo, calcul� la edad a la que hab�an quedado embarazadas y clasifiqu� en cuatro grupos etarios: de 13 a 19 a�os, 20 a 29 a�os, 30 a 39 a�os y 40 a 49 a�os."),
                                   br(),
                                   plotOutput('grafEdad'),
                                   br(),
                                   #tableOutput('tablaEdad'),
                                   br(),
                                   p("Del total de embarazos no deseados, el grupo que tiene m�s peso es el de las mujeres que quedaron embarazadas entre los 20 y 29 a�os. ")
                          ),
                          tabPanel('Clase',
                                   h2(strong("�Los embarazos no deseados son solo una problem�tica de mujeres pobres?")),
                                   p("Para responder esta pregunta, clasifiqu� el sector econ�mico en base a el m�ximo nivel educativo alcanzado por las mujeres (esto implic� dejar de lado a las adolescentes). De educaci�n secundaria completa para abajo clasifiqu� en Populares, y el resto Medio/Alto."),
                                   p("Despu�s, cre� una tasa de embarazos no deseados para cada grupo."),
                                   plotOutput('grafClase')                                   #tableOutput('tablaClase'),
                          ),
                          tabPanel('Uso de anticonceptivos',
                            h2(strong("�Los embarazos no deseados son solo resultado de no tomar medidas anticonceptivas?")),
                            p("A las mujeres se les pregunt� directamente sobre el uso de anticonceptivos en el momento que quedaron embarazadas. Entonces, cre� las variables en base a esa pregunta."),
                            plotOutput('grafusoA'),
                            p("Si bien m�s de la mitad respondi� que no estaba usando anticonceptivos, casi un 43%, osea, 326 mujeres respondieron que el anticonceptivo fall� y tuvieron un END.")
                            ),
                          tabPanel('Religi�n',
                                   p(h2("�La valoraci�n cultural de la familia tradicional evita los embarazos no deseados?")),
                                   p("Para responder esta pregunta, us� la variable religi�n. Presumiblemente, las mujeres que afirman que son religiosas (sobre todo las cat�licas y evang�licas), son parte de grupos culturales con una valoraci�n tradicional de familia."),
                                   plotOutput('grafRelig'),
                                   p("Al tomar �nicamente la clasficiaci�n atea/religiosa, las mujeres ateas son un 9% m�s propensas a tener un END que las mujeres religiosas. Pero al incorporar religiones:"),
                                   plotOutput('grafRelig1'),
                                   p("Vemos que las mujeres evang�licas tienen una tasa bastante parecida de END que las mujeres ateas. Si bien las mujeres cat�licas siguen teniendo una menor tasa de END, esta se reduce a un 8% de diferencia con las mujeres ateas.")
                          )
             )),
    tabPanel(strong('Conclusiones'),
             br(),
             h4(p("-La mayor parte de las mujeres de la muestra que tuvieron un END no eran adolescentes, ten�an entre 20 y 39 a�os.")),
             h4(p("-Las mujeres de sectores bajos tienen una tasa de embarazos muy similar a las mujeres de sectores altos. No hay una diferencia sustancial del deseo de tener hijxs entre sectores sociales.")),
             h4(p("-Aunque predominan los casos donde hubo END por no utilizar anticonceptivos, un n�mero importante de mujeres quedaron embarazadas a�n usando anticonceptivos. Es decir, estos �ltimos pueden fallar, y bastante.")),
             h4(p("-Las mujeres que pertenecen a grupos que tienen una valoraci�n tradicional de la familia, a�n as� tienen una tasa bastante relevante de END. Si bien es menor a la de las mujeres ateas, en el caso de las evang�licas no lo es por tanta diferencia. Incluso, tienen una tasa similar a las mujeres seg�n clasificaci�n por sectores socioecon�micos. Por lo que no puede hablarse de que los ideales subjetivos de maternidad de las mujeres se correspondan necesariamente con los de su grupo de pertenencia."))),
    tabPanel(strong('Anexo tablas'),
             navlistPanel('Tablas de:',
                          tabPanel('Edad',
                                   br(),
                                   tableOutput('tablaEdad')),
                          tabPanel('Sector',
                                   br(),
                                   tableOutput('tablaClase')),
                          tabPanel('Religi�n',
                                   br(),
                                   p("Clasificaci�n seg�n si eran ateas o religiosas:"),
                                   tableOutput('tablaRelig'),
                                   br(),
                                   p("Clasificaci�n seg�n religiones espec�ficas y ateas:"),
                                   tableOutput('tablaRelig1'))
                          )
             )
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
    
    graf<- ggplot(END_UA, aes(x= Cat,y= Tasa))+geom_bar(stat="identity",fill="salmon3",width=0.7)
    graf <- graf + scale_y_continuous(labels = function(x) paste0(x, "%"))
    graf <- graf + geom_label(aes(x = Cat, y = Tasa, label = paste0(round(Tasa,2),"%")),
                            hjust = 0.5, 
                            vjust = 1, 
                            colour = "white", 
                            fill = NA, 
                            label.size = NA, 
                            size = 6)
    graf    
        })
  

  
  output$grafRelig=renderPlot({
    
ggplot(END_RELIG, aes(x=Categoria, y=Tasa))+geom_bar(stat="identity",fill="salmon3",width=0.7) + scale_y_continuous(labels = function(x) paste0(x, "%"))+geom_label(aes(x = Categoria, y = Tasa, label = paste0(round(Tasa, 0),"%")),
                                                                                                                                           hjust = 0.5, 
                                                                                                                                           vjust = 1, 
                                                                                                                                           colour = "white", 
                                                                                                                                           fill = NA, 
                                                                                                                                           label.size = NA, 
                                                                                                                                           size = 6)
    
  })
  
  
  output$grafRelig1=renderPlot({
    
grel1<-ggplot(END_CRELIG, aes(x=reorder(religion,-tasa),y=tasa))+geom_bar(stat='identity',fill="salmon3",width=0.7)
grel1<-grel1+scale_y_continuous(labels = function(x) paste0(x, "%"))
grel1<- grel1+geom_label(aes(x = religion, y = tasa, label = paste0(round(tasa, 2),"%")),
                         hjust = 0.5, 
                         vjust = 1, 
                         colour = "white", 
                         fill = NA, 
                         label.size = NA, 
                         size = 6)
grel1
  })
  output$tablaEdad=renderTable({END_EDAD})
  output$tablaClase=renderTable({END_SECTOR})
  output$tablaRelig=renderTable({END_RELIG})
  output$tablaRelig1=renderTable({END_CRELIG})
  
  
}


    shinyApp(ui = ui, server = server)
  
