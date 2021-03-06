library(ggplot2)
library(plotly)
library(shiny)
rm(list = ls())
#pobres, trolas y maleducadas
setwd("C:/Users/perla/OneDrive/Documents/ssyr/data")
datosMujeres=read.table("ENSSyR_Mujeres_BaseUsuario.txt", header=T, sep="|")
datosMujeres$MEP11 <- ifelse (datosMujeres$MEP11== 1, "Embarazo deseado","Embarazo no deseado" )
datosMujeres= datosMujeres[!is.na(datosMujeres$MEP11),]

df_corte <- datosMujeres[which (datosMujeres$MEP11=="Embarazo no deseado"),]


datosMujeres$MITS20 <- ifelse(datosMujeres$MITS20 == 1, "Cat�lica",
                              ifelse(datosMujeres$MITS20 == 2, "Evang�lica", 
                                     ifelse(datosMujeres$MITS20 == 3, "Jud�a",
                                            ifelse(datosMujeres$MITS20 == 4, "Testigo de Jeohv�",
                                                   ifelse(datosMujeres$MITS20 == 5, "Morm�n",
                                                          ifelse(datosMujeres$MITS20==7, "Adventista", "Otro"))))))



datosMujeres$MEP05 <- ifelse(datosMujeres$MEP05 == 1, "Si",
                             ifelse(datosMujeres$MEP05 == 2, "No", NA))

datosMujeres$MEP14 <- ifelse(datosMujeres$MEP14 == 1, "Si",
                             ifelse(datosMujeres$MEP14 == 2, "No", NA))

head(datosMujeres$MASA17_01)
datosMujeres$GRUPEDAD <- ifelse(datosMujeres$GRUPEDAD==1, "14 a 19 a�os",
                                ifelse(datosMujeres$GRUPEDAD == 2, "20 a 29 a�os",
                                       ifelse(datosMujeres$GRUPEDAD==3, "30 a 39 a�os",
                                              ifelse(datosMujeres$GRUPEDAD == 4, "40 a 49 a�os","50 a 59 años"))))


#Clasifico sector popular seg�n nivel educativo (!!!!!!!!!!!!!!!!!)
datosMujeresAdult <- datosMujeres[which(datosMujeres$GRUPEDAD != 1),]

datosMujeresAdult$sector <- ifelse(datosMujeresAdult$NIVEL_INSTRUCCION_AGRUPADO < 3, "Popular","Medio/Alto")
datosMujeresAdult= datosMujeresAdult[!is.na(datosMujeresAdult$MEP11),]


datosMujeresAdult$MEP11 <- ifelse (datosMujeresAdult$MEP11== 1, "Embarazo deseado","Embarazo no deseado" )


totalEND<- nrow(datosMujeres[datosMujeres$MEP11 == "Embarazo no deseado",])

totalEND1 <- nrow(datosMujeres[datosMujeres$MEP11 == "Embarazo no deseado" & datosMujeres$GRUPEDAD == "14 a 19 a�os",])
totalEND2 <- nrow(datosMujeres[datosMujeres$MEP11 == "Embarazo no deseado" & datosMujeres$GRUPEDAD == "20 a 29 a�os",])
totalEND3 <- nrow(datosMujeres[datosMujeres$MEP11 == "Embarazo no deseado" & datosMujeres$GRUPEDAD == "30 a 39 a�os",])
totalEND4 <- nrow(datosMujeres[datosMujeres$MEP11 == "Embarazo no deseado" & datosMujeres$GRUPEDAD == "40 a 49 a�os",])

#hago tasas
g1 <- nrow(datosMujeres[datosMujeres$GRUPEDAD=="14 a 19 a�os",])
g2 <- nrow(datosMujeres[datosMujeres$GRUPEDAD=="20 a 29 a�os",])
g3 <- nrow(datosMujeres[datosMujeres$GRUPEDAD=="30 a 39 a�os",])
g4 <- nrow(datosMujeres[datosMujeres$GRUPEDAD=="40 a 49 a�os",])

embarazosND <-rbind(totalEND1,totalEND2,totalEND3,totalEND4)
embarazosND <- as.data.frame(embarazosND)
embarazosND$"GrupEdad"<- c("14 a 19 a�os","20 a 29 a�os","30 a 39 a�os","40 a 49 a�os")
embarazosND$total <- c(g1,g2,g3,g4)
embarazosND$tasa <- round(embarazosND$V1*100/embarazosND$total,2)
colnames(embarazosND)<- c("Cant_END","Grupedad","Cant_total","Tasa")
  
#�las adolescentes son las q tienen mas embarazos no deseados? 
#Porcentaje representativo de la tasa de embarazos no deseados de cada grupo sobre los embarazos no deseados totales. 

p <- plot_ly(embarazosND, labels = ~Grupedad, values = ~Tasa, type = 'pie') %>%
  layout(title = "�Los embarazos no deseados son solo una problem�tica adolescente?",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#(!!!!!!!!!!!!!!) fijarme cuales del segundo grupo quedaron embarazadas durante la adolescencia (primer grupo)
#pregunta de a�o de nacimiento: MEP10

df_corte$canta <- 2013-df_corte$MEP10
summary(df_corte$canta)
df_corte$edadUH <- df_corte$EDAD-df_corte$canta
summary(df_corte$edadUH)

df_corte$GRUPEDAD_UH <- ifelse(df_corte$edadUH<20,"13 a 19 a�os",
                               ifelse(df_corte$edadUH<30, "20 a 29 a�os",
                                      ifelse(df_corte$edadUH<40,"30 a 39 a�os",
                                             ifelse(df_corte$edadUH<50,"40 a 49 a�os"))))


end_1 <- nrow(df_corte[df_corte$GRUPEDAD_UH=="13 a 19 a�os",])
end_2 <- nrow(df_corte[df_corte$GRUPEDAD_UH=="20 a 29 a�os",])
end_3 <- nrow(df_corte[df_corte$GRUPEDAD_UH=="30 a 39 a�os",])
end_4 <- nrow(df_corte[df_corte$GRUPEDAD_UH=="40 a 49 a�os",])

agrup <- c("13 a 19 a�os","20 a 29 a�os","30 a 39 a�os","40 a 49 a�os")
edades <- c(end_1,end_2,end_3,end_4)

end_grupE <- as.data.frame(cbind(agrup,edades,totalEND))

p <- plot_ly(end_grupE, labels = ~agrup, values = ~edades, type = 'pie') %>%
  layout(title = "�Los embarazos no deseados son solo una problem�tica adolescente?",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#�Los embarazos no deseados son una problem�tica de mujeres pobres? �Se puede atribuir solo a la falta de educaci�n? 

s1 <- nrow(datosMujeresAdult[datosMujeresAdult$sector == "Medio/Alto",])
s2 <- nrow(datosMujeresAdult[datosMujeresAdult$sector == "Popular",])

END_S_1 <- nrow(datosMujeres[datosMujeresAdult$sector == "Medio/Alto" & datosMujeresAdult$MEP11== "Embarazo no deseado",])
END_S_2 <- nrow(datosMujeres[datosMujeresAdult$sector == "Popular" & datosMujeresAdult$MEP11== "Embarazo no deseado",])

embarazosnd_sector <- as.data.frame(rbind(s1,s2)) 
colnames(embarazosnd_sector)<-c("Cantidad")
embarazosnd_sector$Cant_END <- c(END_S_1,END_S_2)
embarazosnd_sector$tasa <- round(embarazosnd_sector$Cant_END*100/embarazosnd_sector$Cantidad,2)
embarazosnd_sector$total <- c(100)
embarazosnd_sector$sector <- c("Medio/Alto","Popular")

g2<-ggplot(embarazosnd_sector, aes(x="", y=tasa, fill=sector))+geom_bar(width=1,stat="identity")+coord_polar("y", start=0)


plot_ly(embarazosnd_sector, labels = ~sector, values = ~tasa, type = 'pie') %>%
  layout(title = "�Los embarazos no deseados son solo una problem�tica de mujeres pobres?",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#�Que porcentaje de las mujeres que tuvieron un embarazo no deseado utilizaban anticonceptivos? 

g3<-ggplot(df_corte,aes(x="",y=MEP11, fill=factor(MEP14)))+geom_bar(width=1,stat="identity")
g3<- g3 + coord_polar("y", start=0)

plot_ly(embarazosND, labels = ~MEP14, values = , type = 'pie') %>%
  layout(title = "�Es suficiente educar sobre m�todos anticonceptivos para prevenir embarazos no deseados?",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#�Las mujeres religiosas tienen m�s deseos de tener hijos? 




