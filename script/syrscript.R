#Encuesta salud sexual y reproductiva
#Para el uso de la base usuario se sugiere la consulta del cuestionario que  encontrar√° al final de este documento. Se advierte que en caso de campos en blanco, en general, los mismos se corresponden con saltos en la secuencia de preguntas. Los c√≥digos 9, 99, 999 y 9999 que en general son el valor m√°ximo de cada escala, se corresponden con datos que el encuestado no sab√?a o no quer√?a contestar.
setwd("C:/Users/perla/Desktop/R/directorio/sexual y rep")
datosMujeres=read.table("ENSSyR_Mujeres_BaseUsuario.txt", header=T, sep="|")
datosHombres=read.table("ENSSyR_Varones_BaseUsuario.txt", header=T, sep="|")


datosMujeres$REGION <- ifelse(datosMujeres$REGION == 1, "GBA",
                        ifelse(datosMujeres$REGION == 2, "Pampeana",
                               ifelse(datosMujeres$REGION == 3, "Noroeste",
                                      ifelse(datosMujeres$REGION == 4, "Noreste", 
                                          ifelse(datosMujeres$REGION == 5, "Cuyo", "Patagonia")))))

head(datosMujeres$REGION)
datosHombres$REGION <- ifelse(datosHombres$REGION == 1, "GBA",
                              ifelse(datosHombres$REGION == 2, "Pampeana",
                                     ifelse(datosHombres$REGION == 3, "Noroeste",
                                            ifelse(datosHombres$REGION == 4, "Noreste", 
                                                   ifelse(datosHombres$REGION == 5, "Cuyo", "Patagonia")))))

datosMujeres$MASA17_01 <- ifelse(datosMujeres$MASA17_01 == 1, "Pastilla anticonceptiva",
                                 ifelse(datosMujeres$MASA17_01== 2, "DIU", 
                                        ifelse(datosMujeres$MASA17_01==3, "Preservativo",
                                               ifelse(datosMujeres$MASA17_01==4, "Diafragma",
                                                      ifelse(datosMujeres$MASA17_01==5, "Inyectable",
                                                             ifelse(datosMujeres$MASA17_01==6,"Implantes",
                                                                    ifelse(datosMujeres$MASA17_01==7, "Espumas",
                                                                           ifelse(datosMujeres$MASA17_01==8, "Ligadura de trompas",
                                                                                  ifelse(datosMujeres$MASA17_01==9, "Metodo de Billings",
                                                                                         ifelse(datosMujeres$MASA17_01==10,"Ciclo menstrual",
                                                                                                ifelse(datosMujeres$MASA17_01==11, "Retiro", 
                                                                                                       ifelse(datosMujeres$MASA17_01==12, "VasectomÌa",
                                                                                                              ifelse(datosMujeres$MASA17_01==13, "Yuyos, yerbas, tÈs",
                                                                                                                     ifelse(datosMujeres$MASA17_01==15, "Pastilla del dÌa despuÈs",
                                                                                                                            ifelse(datosMujeres$MASA17_01==16, "Otro", NA)))))))))))))))

                                 
                          



datosHombres$VASA16_01 <- ifelse(datosHombres$VASA16_01 == 1, "Pastilla anticonceptiva",
                                 ifelse(datosHombres$VASA16_01== 2, "DIU", 
                                        ifelse(datosHombres$VASA16_01==3, "Preservativo",
                                               ifelse(datosHombres$VASA16_01==4, "Diafragma",
                                                      ifelse(datosHombres$VASA16_01==5, "Inyectable",
                                                             ifelse(datosHombres$VASA16_01==6,"Implantes",
                                                                    ifelse(datosHombres$VASA16_01==7, "Espumas",
                                                                           ifelse(datosHombres$VASA16_01==8, "Ligadura de trompas",
                                                                                  ifelse(datosHombres$VASA16_01==9, "Metodo de Billings",
                                                                                         ifelse(datosHombres$VASA16_01==10,"Ciclo menstrual",
                                                                                                ifelse(datosHombres$VASA16_01==11, "Retiro", 
                                                                                                       ifelse(datosHombres$VASA16_01==12, "VasectomÌa",
                                                                                                              ifelse(datosHombres$VASA16_01==13, "Yuyos, yerbas, tÈs",
                                                                                                                     ifelse(datosHombres$VASA16_01==15, "Pastilla del dÌa despuÈs",
                                                                                                                            ifelse(datosHombres$VASA16_01==16, "Otro", NA)))))))))))))))






head(datosMujeres$MASA17_01)


#Ver que hacer con los NA. En la parte de casa pr√°cticamente no hay, aparecen m√°s desp. 
datosMujeres[is.na(datosMujeres$HO02),]

#Crear columnas de variables de NBI 
#http://www.estadistica.ec.gba.gov.ar/dpe/index.php/sociedad/condiciones-de-vida/necesidades-basicas-insatisfechas/177-metodologia-necesidades-basicas-insatisfechas/230-metodologia-necesidades-basicas-insatisfechas


#datosMujeres$CV01 = as.numeric(datosMujeres$CV01)

#el dato num√©rico me lo toma como un integrer y esto hace que no me funque bien esto, pasarlo a num√©rico con as.numeric. preguntar si se puede hacer m√°s facil directo cdo lee el df

datosMujeres$viviendaNBI = ifelse(datosMujeres$CV01 == 1, FALSE, 
                                  ifelse (datosMujeres$CV01 == 3, FALSE,
                                          TRUE))

datosMujeres$materialpiso=ifelse(datosMujeres$CV03 != 1, TRUE, FALSE)

datosMujeres$hacinamiento= ifelse(datosMujeres$CANTCOMPONENTES/datosMujeres$HO02 > 3, TRUE, FALSE)

datosMujeres$banoNBI= ifelse(datosMujeres$CV09 == 1, FALSE,
                             ifelse(datosMujeres$CV10 != 1, TRUE, FALSE))

datosMujeres$materialtecho= ifelse(datosMujeres$CV04 >= 5, TRUE, ifelse(datosMujeres$CV05 == 2, TRUE, FALSE))

datosHombres$viviendaNBI = ifelse(datosHombres$CV01 == 1, FALSE, 
                                  ifelse (datosHombres$CV01 == 3, FALSE,
                                          TRUE))

datosHombres$materialpiso=ifelse(datosHombres$CV03 != 1, TRUE, FALSE)

datosHombres$hacinamiento= ifelse(datosHombres$CANTCOMPONENTES/datosHombres$HO02 > 3, TRUE, FALSE)

datosHombres$banoNBI= ifelse(datosHombres$CV09 == 1, FALSE,
                             ifelse(datosHombres$CV10 != 1, TRUE, FALSE))

datosHombres$materialtecho= ifelse(datosHombres$CV04 >= 5, TRUE, ifelse(datosHombres$CV05 == 2, TRUE, FALSE))


#para ir chequeando si las cosas me dan bien: 

datosMujeres[which(datosMujeres$CANTCOMPONENTES>3), c("CANTCOMPONENTES", "HO02","hacinamiento")]

datosMujeres[which(datosMujeres$CANTCOMPONENTES>3), c("CANTCOMPONENTES", "HO02","hacinamiento")]

datosMujeres[which(datosMujeres$CANTCOMPONENTES>3), c("CANTCOMPONENTES", "HO02","hacinamiento")]

datosMujeres[which(datosMujeres$NBI == TRUE & datosMujeres$hacinamiento == FALSE & datosMujeres$viviendaNBI == FALSE & datosMujeres$materialpiso == FALSE & datosMujeres$materialtecho == FALSE & datosMujeres$banoNBI == FALSE),]



#Crear columna de si tiene NBI o no:

datosMujeres$NBI= ifelse(datosMujeres$viviendaNBI == TRUE, "Tiene", 
                         ifelse(datosMujeres$materialpiso == TRUE, "Tiene", 
                                ifelse(datosMujeres$hacinamiento == TRUE, "Tiene",
                                       ifelse(datosMujeres$banoNBI == TRUE, "Tiene",
                                              ifelse(datosMujeres$materialtecho == TRUE, "Tiene", "No tiene")))))

datosHombres$NBI= ifelse(datosHombres$viviendaNBI == TRUE, "Tiene", 
                         ifelse(datosHombres$materialpiso == TRUE, "Tiene", 
                                ifelse(datosHombres$hacinamiento == TRUE, "Tiene",
                                       ifelse(datosHombres$banoNBI == TRUE, "Tiene",
                                              ifelse(datosHombres$materialtecho == TRUE, "Tiene", "No tiene")))))


head(datosMujeres)
library(ggplot2)

#Cantidad de NA's de las columnas.

sapply(datosMujeres, function(x) sum(is.na(x)))

#saco NA's de banoNBI y NBI (son 28 y 6)

datosMujeres= datosMujeres[!is.na(datosMujeres$banoNBI),]
datosMujeres= datosMujeres[!is.na(datosMujeres$MASA17_01),]
datosHombres= datosHombres[!is.na(datosHombres$VASA16_01),]

qplot(datosMujeres$NBI)

#quiero sacar si llegan a la lÌnea de la pobreza
#http://wadmin.uca.edu.ar/public/20180426/1524770671_INDIGENCIA_Y_POBREZA_INFORME_PRENSA_abril_2014.pdf

flia_sh=datosMujeres[which(datosMujeres$TIPO_H %in% 2),]
flia_ch=datosMujeres[which(datosMujeres$TIPO_H %in% 4),]
fliasM <- rbind(flia_ch,flia_sh)
summary(flias$CANTCOMPONENTES)

flia_shH=datosHombres[which(datosHombres$TIPO_H %in% 2),]
flia_chH=datosHombres[which(datosHombres$TIPO_H %in% 4),]
fliasH <- rbind(flia_chH,flia_shH)

rm(list = c("flia_sh","flia_ch","flia_shH","flia_chH"))
a = quantile(datosMujeres$CANTCOMPONENTES, c(0,0.85)) #ac√° estoy calculando los valores que llegan al 25% y 75%

fliasM <- fliasM[which(fliasM$CANTCOMPONENTES <= 7),]
fliasH <- fliasH[which(fliasH$CANTCOMPONENTES <= 7),]

flias[which(flias$CANTCOMPONENTES == 2 & flias$RANGO_INGRESO < 5),]

fliasM$CBA <- ifelse(fliasM$CANTCOMPONENTES == 2 & fliasM$RANGO_INGRESO < 3, "No llega",
                ifelse(fliasM$CANTCOMPONENTES == 3 & fliasM$RANGO_INGRESO < 4, "No llega",
                  ifelse(fliasM$CANTCOMPONENTES == 4 & fliasM$RANGO_INGRESO < 5, "No llega",
                    ifelse (fliasM$CANTCOMPONENTES == 5 & fliasM$RANGO_INGRESO < 6, "No llega",
                      ifelse (fliasM$CANTCOMPONENTES == 6 & fliasM$RANGO_INGRESO < 7, "No llega",
                       ifelse (fliasM$CANTCOMPONENTES == 7 & fliasM$RANGO_INGRESO < 8, "No llega", "Llega"))))))

fliasM$CBT <- ifelse(fliasM$CANTCOMPONENTES == 2 & fliasM$RANGO_INGRESO < 4, "No llega",
                    ifelse(fliasM$CANTCOMPONENTES == 3 & fliasM$RANGO_INGRESO < 4, "No llega",
                           ifelse(fliasM$CANTCOMPONENTES == 4 & fliasM$RANGO_INGRESO < 5, "No llega",
                                  ifelse (fliasM$CANTCOMPONENTES == 5 & fliasM$RANGO_INGRESO < 6, "No llega",
                                          ifelse (fliasM$CANTCOMPONENTES == 6 & fliasM$RANGO_INGRESO < 7, "No llega",
                                                  ifelse (fliasM$CANTCOMPONENTES == 7 & fliasM$RANGO_INGRESO < 8, "No llega", "Llega"))))))

fliasM$lp <- ifelse(fliasM$CBT == "No llega", "No llega",
                   ifelse(fliasM$NBI == "Tiene", "No llega",
                          ifelse(fliasM$CBA == "No llega", "No llega","Llega")))


fliasH$CBA <- ifelse(fliasH$CANTCOMPONENTES == 2 & fliasH$RANGO_INGRESO < 3, "No llega",
                     ifelse(fliasH$CANTCOMPONENTES == 3 & fliasH$RANGO_INGRESO < 4, "No llega",
                            ifelse(fliasH$CANTCOMPONENTES == 4 & fliasH$RANGO_INGRESO < 5, "No llega",
                                   ifelse (fliasH$CANTCOMPONENTES == 5 & fliasH$RANGO_INGRESO < 6, "No llega",
                                           ifelse (fliasH$CANTCOMPONENTES == 6 & fliasH$RANGO_INGRESO < 7, "No llega",
                                                   ifelse (fliasH$CANTCOMPONENTES == 7 & fliasH$RANGO_INGRESO < 8, "No llega", "Llega"))))))

fliasH$CBT <- ifelse(fliasH$CANTCOMPONENTES == 2 & fliasH$RANGO_INGRESO < 4, "No llega",
                     ifelse(fliasH$CANTCOMPONENTES == 3 & fliasH$RANGO_INGRESO < 4, "No llega",
                            ifelse(fliasH$CANTCOMPONENTES == 4 & fliasH$RANGO_INGRESO < 5, "No llega",
                                   ifelse (fliasH$CANTCOMPONENTES == 5 & fliasH$RANGO_INGRESO < 6, "No llega",
                                           ifelse (fliasH$CANTCOMPONENTES == 6 & fliasH$RANGO_INGRESO < 7, "No llega",
                                                   ifelse (fliasH$CANTCOMPONENTES == 7 & fliasH$RANGO_INGRESO < 8, "No llega", "Llega"))))))

fliasH$lp <- ifelse(fliasH$CBT == "No llega", "No llega",
                    ifelse(fliasH$NBI == "Tiene", "No llega",
                           ifelse(fliasH$CBA == "No llega", "No llega","Llega")))


#saco NA

fliasM= fliasM[!is.na(fliasM$lp),]
fliasH= fliasH[!is.na(fliasH$lp),]


#øEn que regiones hay un mayor porcentaje de encuestadas que no llega a la linea de pobreza? 
library(ggplot2)

ggplot(fliasM, aes(lp))+geom_bar()+facet_grid(.~REGION)+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +labs(title="Distribucion de la pobreza segun region",y="Cantidad",x="Linea pobreza")
ggplot(fliasH, aes(lp))+geom_bar()+facet_grid(.~REGION)+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +labs(title="Distribucion de la pobreza segun region",y="Cantidad",x="Linea pobreza")


#øLas mujeres pobres tienen relaciones sexuales mas jovenes? masa03 primer relacion 

prM <- aggregate(MASA03 ~ lp, data=fliasM, FUN="median")
prH <- aggregate(VASA03 ~ lp, data=fliasH, FUN="median")

#menor uso de anticonceptivos eficientes? 

ggplot(fliasM,aes(MASA17_01))+geom_bar()+facet_grid(.~lp)+scale_y_continuous(limits= c(0,500))+theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(fliasH,aes(VASA16_01))+geom_bar()+facet_grid(.~lp)+scale_y_continuous(limits= c(0,500)) +theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#decisiÛn sobre metodo anticonceptivo
#quien
ggplot(fliasM,aes(MASA18))+geom_bar()+facet_grid(.~lp) 
ggplot(fliasH,aes(VASA17))+geom_bar()+facet_grid(.~lp)


#en adolescentes 
datosMujeresA<- datosMujeres[which(datosMujeres$GRUPEDAD==1),]
ggplot(datosMujeresA,aes(MASA17_01))+geom_bar()+facet_grid(.~REGION)

#xq

ggplot(datosMujeres,aes(MASA19))+geom_bar()+facet_grid(.~NBI)+scale_x_continuous(limits = c(0,10))

ggplot(fliasH,aes(VASA18))+geom_bar()+facet_grid(.~lp)



#mayor cantidad de hijxs?

aggregate(MEP03 ~ lp, data=fliasM, FUN=function(x) quantile(x,0.7))


#Agrupaciones seg˙n edad
datosMujeres$MASA07 <- ifelse(datosMujeres$MASA07 == 1, "Si", ifelse(datosMujeres$MASA07 == 2, "No", NA))
datosMujeres$MASA16 <- ifelse(datosMujeres$MASA16 == 1, "Si", ifelse(datosMujeres$MASA16 == 2, "No", NA))

head(datosMujeres$MASA16)

rangoEdadM <- datosMujeres[which(datosMujeres$MASA02 == 1),] 

ggplot(rangoEdadM, aes(MASA07))+geom_bar()+facet_grid(.~GRUPEDAD)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title="Uso de preservativo en primera relaciÛn sexual seg˙n grupo etario",y="Cantidad",x="Linea pobreza")
ggplot(rangoEdadM, aes(MASA16))+geom_bar()+facet_grid(.~GRUPEDAD)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title="Uso regular de anticonceptivos seg˙n grupo etario",y="Cantidad",x="Linea pobreza")

datosHombres$VASA07 <- ifelse(datosHombres$VASA07 == 1, "Si", ifelse(datosHombres$VASA07 == 2, "No", NA))
datosHombres$VASA15 <- ifelse(datosHombres$VASA15 == 1, "Si", ifelse(datosHombres$VASA15 == 2, "No", NA))

rangoEdadH <- datosHombres[which(datosHombres$VASA02 == 1),] 

ggplot(rangoEdadH, aes(VASA07))+geom_bar()+facet_grid(.~GRUPEDAD)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title="Uso de preservativo en primera relaciÛn sexual seg˙n grupo etario",y="Cantidad",x="Linea pobreza")
ggplot(rangoEdadH, aes(VASA15))+geom_bar()+facet_grid(.~GRUPEDAD)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title="Uso de preservativo en primera relaciÛn sexual seg˙n grupo etario",y="Cantidad",x="Linea pobreza")




#øLas mujeres religiosas son mas propensas a tener embarazos/mas hijxs? øy a tener relaciones sexuales mas tardÌas?

datosMujeres$MITS20 <- ifelse(datosMujeres$MITS20 == 1, "CatÛlica",
                              ifelse(datosMujeres$MITS20 == 2, "EvangÈlica", 
                                     ifelse(datosMujeres$MITS20 == 3, "JudÌa",
                                            ifelse(datosMujeres$MITS20 == 4, "Testigo de Jeohv·",
                                                   ifelse(datosMujeres$MITS20 == 5, "MormÛn",
                                                          ifelse(datosMujeres$MITS20==7, "Adventista", "Otro"))))))
datosMujeres$MITS19 <- ifelse(datosMujeres$MITS19 == 1, "SÌ", 
                              ifelse(datosMujeres$MITS19 == 2, "No", NA))
aggregate(MEP02 ~ MITS19, data= datosMujeres, FUN= 'median') #embarazos seg˙n religiosa/no
aggregate.data.frame(MEP02 ~ MITS20, data= datosMujeres, FUN= 'median') #embarazos segun tipo religion 
aggregate(MEP03 ~ MITS20, data= datosMujeres, FUN= 'median')

aggregate(MASA03 ~MITS19, data= datosMujeres, FUN='median')
aggregate(MASA03 ~MITS20, data= datosMujeres, FUN='median')


#conocimiento sobre ESI





#saco NA's de MEP03 (cantidad de hijxs nacidxs), son 1879, todavia me quedan m√°s de 3000 muestras
summary(datosMujeres$MEP03)
flias= flias[!is.na(flias$MEP03),]
#a = quantile(datosMujeres$MEP03, c(0.25,0.75)) #ac√° estoy calculando los valores que llegan al 25% y 75%
#maximo= a[2]+1.5*a[2] #ac√° vuela todo lo que est√© por encima del valor

#set.seed(1234)
#muestra <- floor(nrow(flias[which(flias$lp == "Llega"),])*0.79)
#reduccion <- sample(nrow(flias), muestra, replace=F)

#basereduccion <- flias[reduccion,]



qplot(datosMujeres$MEP03,data=datosMujeres,geom='histogram',facets = .~REGION, binwidth=0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(flias, aes(MEP13))+geom_bar(aes(fill=lp, position="stack"))+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title= "Deseo de la pareja de tener hijxs segun sector economico",x= "Respuesta", y= "Cantidad mujeres")+scale_x_continuous(limits = c(0, 4))
ggplot(flias, aes(MASA18))+geom_bar(aes(fill=lp, position="stack"))+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title= "Deseo de la pareja de tener hijxs segun sector economico",x= "Respuesta", y= "Cantidad mujeres")+scale_x_continuous(limits = c(0, 5))
ggplot(flias, aes(MASA19))+geom_bar(aes(fill=lp, position="stack"))+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title= "Deseo de la pareja de tener hijxs segun sector economico",x= "Respuesta", y= "Cantidad mujeres")+scale_x_continuous(limits = c(0, 10))
ggplot(flias, aes(MASA17_02_17))+geom_bar(aes(fill=lp, position="stack"))+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title= "Deseo de la pareja de tener hijxs segun sector economico",x= "Respuesta", y= "Cantidad mujeres")+scale_x_continuous(limits = c(0, 2))
ggplot(flias, aes(MASA09))+geom_bar(aes(fill=lp, position="stack"))+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title= "Deseo de la pareja de tener hijxs segun sector economico",x= "Respuesta", y= "Cantidad mujeres")+scale_x_continuous(limits = c(0, 2))

