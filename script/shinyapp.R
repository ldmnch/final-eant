#shiny

library(ggplot2)
library(plotly)
library(shiny)
setwd("C:/Users/perla/OneDrive/Documents/ssyr/data")
datosMujeres=read.table("ENSSyR_Mujeres_BaseUsuario.txt", header=T, sep="|")
END=read.table("END.csv", header=T, sep=",")
END_ADULT=read.table("END_ADULT.csv", header=T, sep=",")
END_EDAD=read.table("END_EDAD.csv", header=T, sep=",")
END_RELIG=read.table("END_RELIG.csv", header=T, sep=",")
END_SECTOR=read.table("END_SECTOR.csv", header=T, sep=",")


ui=fluidPage (
  titlePanel("Trabajo final EANT"),
  tabsetPanel(
    tabPanel('Gráficos',
             navlistPanel('Analisis según:',
                          tabPanel('Edad',
                                   dataTableOutput('tablaEdad'),
                                   plotlyOutput('grafEdad'),
                          ),
                          tabPanel('Clase',
                                   dataTableOutput('tablaClase'),
                                   plotlyOutput('grafClase')
                          ),
                          tabPanel('Uso de anticonceptivos',
                                   plotlyOutput('grafusoA')
                          ),
                          tabPanel('Religión',
                                   plotlyOutput('grafRelig')
                          )
             )
    ),
    
    # 4.1) Cambiar el nombre del panel y colocar las otro ej "Mejores Criticas en peliculas"
    # 4.2) Coloca el id al widget de combo "SelectCritica", las opciones del combo son los 
    # valores unicos de la columns genre, mientras que el valor por default es el primero de la
    # lista generada anteriormente.
    # 4.3) La tabla a generarse va a tener el id "TablaCriticas" y la funcion para su generacion
    # del lado UI es dataTableOutput
    tabPanel('Tablas', #4.1
             selectInput(inputId = "SelectCritica", # 4.2
                         label = "Seleccionar tabla:",
                         choices = unique(movies$genre),
                         selected = unique(movies$genre)[1]),
             dataTableOutput("TablaCriticas")
    ),
    
    # 5.1) Genere un nuevo tabPanel y coloque una denominacion como ej "Grafico por clasificacion"
    # 5.2) Vamos a crear un selectInput, con el id "clasif", las opciones son los valores unicos
    # de la columna mpaa_rating y recuerde que el primer valor es por default, pero la seleccion 
    # en este caso es multiple por lo cual coloque TRUE en el primer parametro.
    # valores unicos de la columns genre, mientras que el valor por default es el primero de la
    # lista generada anteriormente.
    # 5.3) Necesitamos cambiar los colores a nuestros graficos por lo cual vamos a crear dos objetos
    # colorInput, el primero su id es "col1" y el segundo "col2", el valor por default del primero 
    # es "white" y el valor por default del segundo es "blue"
    # 5.4) Agreguemos un boton para descargar informacion con el id "download_data"
    
    tabPanel('Grafico por clasificacion', #5.1
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "clasif",  
                             label = "Seleccionar la clasificacion:",
                             choices =  unique(movies$mpaa_rating), 
                             selected = unique(movies$mpaa_rating)[1],
                             multiple = TRUE), 
                 colourInput("col1",label= "Color interno",value='white'), # 5.3
                 colourInput("col2",label= "Color de la linea",value='blue'),# 5.3
                 downloadButton("download_data")
               ),
               mainPanel(  plotOutput('GraficoGenero2'))
               
             )
             
    )
  ))



server=function(input,output){
  
  # 6.1) El primer objeto que vamos a realizar a retornar el top de la cantidad de elemento que 
  #  seleccione el usurio por lo cual utilizaremos input$num en la funcion top_n
  # 6.2) Realizaremos un grafico de scatter con x=critics_score,y=audience_score y color=genre,  adicionalmente
  #  cada punto tendra un tamaño de rating imdb * 0.05
  
  
  output$ratingImdb=renderPlot({
    RatingImdb=
      movies%>%
      top_n(input$num,imdb_rating)%>%
      select(title,genre,imdb_rating,critics_score,audience_score)
    
    
    RatingImdb%>%ggplot(aes(x=critics_score,y=audience_score,color=genre,
                            size=imdb_rating*0.05))+
      geom_point()
    
  })
  
  # 6.3) El primer objeto que vamos a realizar a retornar el top de la cantidad de elemento que 
  #  seleccione el usurio por lo cual utilizaremos input$num1 en la funcion top_n
  # El grafico es un plotly por lo cual colocar la funcion renderPlotly
  # 6.4) Realizaremos un grafico de scatter con x=critics_score,y=audience_score y color=genre,  adicionalmente
  #  cada punto tendra un tamaño de rating imdb * 0.05
  # 6.5) Eliminar las leyendas colocando "FALSE" en el parametro show.legend
  # 6.6) El objeto p que contiene el grafico debe invocarse como un plotly por eso utilizo la funcion
  # ggplotly
  
  output$ratingVotos=renderPlotly({
    RatingImdb=
      movies%>%
      top_n(input$num1,imdb_num_votes)%>%
      select(title,genre,imdb_rating,critics_score,audience_score,imdb_num_votes)
    
    p=  ggplot(RatingImdb,aes(x=critics_score,y=audience_score,color=genre,
                              size=imdb_rating*0.05))+
      geom_point(show.legend = FALSE)
    ggplotly({p}) #6.6
    
    
  })
  
  # 6.7) Vamos a realizar una tabla por lo cual se utiliza la funcion renderDataTable
  # 6.8) El output donde se genera esta tabla en la UI se denomina "TablaCriticas"
  # 6.9) El grafico se debe filtrar por el combo que ingresa como input y se denomina "SelectCritica"
  
  output$TablaCriticas=renderDataTable({ # 6.7 y 6.8
    movies%>%filter(genre==input$SelectCritica)%>%select(title,runtime,mpaa_rating,
                                                         studio,critics_rating,critics_score,
                                                         audience_rating,audience_score)
  })
  
  # 6.10) Vamos a generar una objeto base que sea reactivo y que se utiliza posteriormente para 
  # generar un grafico, por lo tanto la funcion que utilizamos es reactive
  # 6.11) El dataset se debe filtrar por el combo que ingresa como input y se denomina "clasif"
  
  base=reactive({ #6.10
    base=movies%>%filter(mpaa_rating %in% input$clasif)%>%
      select(title,mpaa_rating,imdb_rating,imdb_num_votes,genre)
  })
  
  
  # 6.12) Ahora vamos a utilizar el dataset anterior que se filtra en forma dinamica recuede que
  # hay que colocar el nombre del dataset seguido de ()
  # 6.13) Como es un grafico utilizar la funcion de render que corresponda y el nombre del output
  # del UI es "GraficoGenero2"
  # 6.14) Se utilizaran los filtros de color que se crearon en la ui el cual para fill es col2 
  # y para las lineas es col1
  
  
  output$GraficoGenero2=renderPlot({
    ggplot(base(),aes(x=mpaa_rating,y=imdb_num_votes))+
      geom_boxplot(fill=input$col2,color=input$col1)+
      scale_y_continuous(limits = c(0, 150000))+
      labs(x='Clasificacion', y='Nro. Votos',
           title='Clasificacion y Nro de votos')
  })
  
  
  # 6.15) El ultimo paso es el boton que escribe el archivo!
  
  output$download_data <- downloadHandler(
    filename = "pelis_data.csv",
    content = function(file) {
      data <- base()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}
