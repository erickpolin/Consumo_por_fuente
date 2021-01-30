###################   2010 #####################

#ENIGH 2010
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH 2010/ENIGH2010")
Conc<-read.dbf("NCV_Concentrado_2010_concil_2010_DBF.dbf",as.is = T)

Conc<-Conc%>%
  select(folioviv,foliohog,tam_hog,factor,upm,est_dis,tam_loc,gasmon,alimentos,
         vestido_c,vivienda,limpieza,salud,transporte,educacion,personal,transfe)
Conc<-Conc%>%
  mutate(prueba=alimentos+vestido_c+vivienda+salud+transporte+educacion+personal+transfe+limpieza)

all.equal(Conc$gasmon,Conc$prueba)

Conc<-Conc%>%
  mutate(Small=ifelse(tam_loc==4,1,0))

Conc$folioviv<-as.numeric(Conc$folioviv)

Conc<-Conc%>%
  mutate(entidad=ifelse(folioviv<99999,substr(folioviv,1,1),substr(folioviv,1,2)))

Conc$entidad<-as.numeric(Conc$entidad)

summary(Conc$entidad)


############vamos a deflactar 
entidad<-c("1","2","3","4","5","6","7","8","9",
           "10","11","12","13","14","15","16","17","18","19","20",
           "21","22","23","24","25","26","27","28","29","30","31","32")

Deflactores<-c(74.47423155,74.27424964,76.64073005,72.07600445,74.7770528,73.50177598,71.51590513,
               75.65625486,71.63136432,71.40512455,71.73113624,72.43926373,70.3729947,72.41898051,
               71.9774887,71.87229217,74.85584543,73.11060425,76.49423725,73.61796506,72.41556468,
               71.02683439,76.24426196,73.49222269,77.8498841,76.10684879,73.54715857,76.14221348,
               71.90667063,73.09217084,72.98545756,72.83828721)

entidades<-c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
             "Colima","Chiapas","Chihuahua","Ciudad de Mexico","Durango","Guanajuato","Guerrero","Hidalgo",
             "Jalisco","Mexico","Michoaca¡n de Ocampo","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
             "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","YucatÃÂ¡n","Zacatecas")

Deflactor_2010<-data.frame(entidad,entidades,Deflactores)

Conc<-merge(Conc,Deflactor_2010,by=c("entidad"))

Conc <- Conc%>%
  mutate(gasmon=(gasmon/Deflactores)*100,alimentos=(alimentos/Deflactores)*100,vestido_c=(vestido_c/Deflactores)*100,
         vivienda=(vivienda/Deflactores)*100,salud=(salud/Deflactores)*100,transporte=(transporte/Deflactores)*100,
         educacion=(educacion/Deflactores)*100,personal=(personal/Deflactores)*100,transfe=(transfe/Deflactores)*100,
         limpieza=(limpieza/Deflactores)*100)

Conc<-Conc%>%
  mutate(gasmon=(gasmon/tam_hog),alimentos=(alimentos/tam_hog),vestido_c=(vestido_c/tam_hog),
         vivienda=(vivienda/tam_hog),salud=(salud/tam_hog),transporte=(transporte/tam_hog),
         educacion=(educacion/tam_hog),personal=(personal/tam_hog),transfe=(transfe/tam_hog),
         limpieza=(limpieza/tam_hog))


#apparently this is a "flag", IDK what is this shit yet
Conc$Nhog <- 1

########################################## DECILES 

#Attaching the data frame
attach(Conc) #this is for not writing the name of the data frame and the $ ever time

#Sort Conc according to gasmon, folioviv, foliohog
Conc<- orderBy (~+gasmon+folioviv+foliohog, data=Conc) #this give us the households sorted by total income

#Adding the values of the expansion factor. The sum is 34 million, which is the number of households in the country.
tot_hogares<-sum(Conc$factor,to.data.frame=TRUE)

#Dividing the number of households into 10 without decimals
tam_dec<-trunc(tot_hogares/10) #the result is 3.5 million housholds per decile

#Adding a variable to the data frame with this value
Conc$tam_dec<-tam_dec

############################# Creating Deciles of Income 

Conc$MAXT<-Conc$gasmon #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

Conc<-Conc[with(Conc, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

Conc$ACUMULA<-cumsum(Conc$factor) #aqu? creamos una variable de suma acumulada del factor de viviendas.



############################Ahora viene la creaci?n de los deciles

#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1,]$factor
  Conc<-rbind(Conc[1:(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),],
              Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1):dim(Conc[1])[1],])
  b1<-tam_dec*i-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
Conc$ACUMULA2<-cumsum(Conc$factor)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
Conc$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
Conc[(Conc$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:9)
{
  Conc[((Conc$ACUMULA2>tam_dec*i)&(Conc$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
Conc[Conc$DECIL%in%"0",]$DECIL<-10

# TOTAL HOGARES
x<-tapply(Conc$factor,Conc$Nhog,sum)
# DECILES
y<-tapply(Conc$factor,Conc$DECIL,sum)
# se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
gasmonmed_t<-tapply(Conc$factor*Conc$gasmon,Conc$Nhog,sum)/x
gasmonmed_d<-tapply(Conc$factor*Conc$gasmon,Conc$DECIL,sum)/y
########################## C U A D R O S 
# guardamos los resultados en un data frame
prom_rub <- data.frame (c(gasmonmed_t,gasmonmed_d))
# agregamos el nombre a las filas
Numdec<-c("Mean", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(prom_rub)<-Numdec

# GINI Nacional (sobre los 10 deciles) por hogar usando el promedio del ingreso corriente (gasmon)
deciles_hog_gasmon <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x),
                                 ingreso=c(gasmonmed_d[1],gasmonmed_d[2],gasmonmed_d[3],
                                           gasmonmed_d[4],gasmonmed_d[5],gasmonmed_d[6],
                                           gasmonmed_d[7],gasmonmed_d[8],gasmonmed_d[9],
                                           gasmonmed_d[10]))
# se efectua la función Gini y se guarda en nuestro vector a.
a<-gini(deciles_hog_gasmon$ingreso,weights=deciles_hog_gasmon$hogares)
# se renombran las variables (columnas)
names(prom_rub)=c("GASTO CORRIENTE")
names(a)="GINI"
##### Mostramos el resultado en pantalla 
round(prom_rub)
round(a,5)

setwd("C:/Users/Erick/OneDrive/GIC/Consumo_por_fuente")

write.dbf(Conc,file="Conc2010_consumo.dbf")

rm(list=ls())

################## Creadicón de las tablas de ingreso por fuente 
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/OneDrive/GIC/Consumo_por_fuente")
Conc2010<-read.dbf("Conc2010_consumo.dbf",as.is = T)



mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc2010,weights=~factor)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, salud, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el gasmon*factor/34744819
gasmonTot <- svyratio(~gasmon,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
gasmonDECIL <- svyby(~gasmon,denominator=~Nhog,by=~DECIL,mydesign,svyratio)


#     alimentos

alimentosTot <- svyratio(~alimentos,denominator=~Nhog,mydesign) # Total promedio
alimentosDECIL <- svyby(~alimentos,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil


###### vestido
vestidoTot <- svyratio(~vestido_c,denominator=~Nhog,mydesign) # Total promedio
vestidoDECIL <- svyby(~vestido_c,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil


###### vivienda
viviendaTot <- svyratio(~vivienda,denominator=~Nhog,mydesign) # Total promedio
viviendaDECIL <- svyby(~vivienda,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil


###### limpieza
limpiezaTot <- svyratio(~limpieza,denominator=~Nhog,mydesign) # Total promedio
limpiezaDECIL<- svyby(~limpieza,denominator=~Nhog,by=~DECIL,mydesign,svyratio) # por decil


###### Salud
saludTot <- svyratio(~salud,denominator=~Nhog,mydesign) # Total promedio
saludDECIL <- svyby(~salud,denominator=~Nhog,by=~DECIL ,mydesign,svyratio)#Por decil


###### transporte
transporteTot <- svyratio(~transporte,denominator=~Nhog,mydesign) # Total promedio
transporteDECIL <- svyby(~transporte,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil


###### educacion
educacionTot <- svyratio(~educacion,denominator=~Nhog,mydesign) # Total promedio
educacionDECIL <- svyby(~educacion,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Por decil


######## personal

personalTot <- svyratio(~personal,denominator=~Nhog,mydesign) # Total promedio
personalDECIL <- svyby(~personal,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### transfe 
transfeTot <- svyratio(~transfe,denominator=~Nhog,mydesign) # Total promedio
transfeDECIL <- svyby(~transfe,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil


######################################### Estimaciones 

ES_gasmonTot <- gasmonTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_gasmonDECIL <- gasmonDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_alimentosTot <- alimentosTot[[1]]
ES_alimentosDECIL <- alimentosDECIL[[2]]

ES_vestidoTot <- vestidoTot[[1]]
ES_vestidoDECIL <- vestidoDECIL[[2]]

ES_viviendaTot <- viviendaTot[[1]]
ES_viviendaDECIL <- viviendaDECIL[[2]]

ES_limpiezaTot <- limpiezaTot [[1]]
ES_limpiezaDECIL <- limpiezaDECIL [[2]]

ES_saludTot <- saludTot [[1]]
ES_saludDECIL <- saludDECIL [[2]]

ES_transporteTot <- transporteTot [[1]]
ES_transporteDECIL <- transporteDECIL [[2]]

ES_educacionTot <- educacionTot [[1]]
ES_educacionDECIL <- educacionDECIL [[2]]

ES_personalTot <- personalTot[[1]]
ES_personalDECIL <- personalDECIL[[2]]

ES_transfeTot <- transfeTot [[1]]
ES_transfeDECIL <- transfeDECIL [[2]]


########## Error Est?ndar 
SE_gasmonTot <- SE (gasmonTot)
SE_gasmonDECIL <- SE (gasmonDECIL)

SE_alimentosTot <- SE (alimentosTot)
SE_alimentosDECIL <- SE (alimentosDECIL)

SE_vestidoTot <- SE (vestidoTot)
SE_vestidoDECIL <- SE (vestidoDECIL)

SE_viviendaTot <- SE (viviendaTot)
SE_viviendaDECIL <- SE (viviendaDECIL)

SE_limpiezaTot <- SE (limpiezaTot)
SE_limpiezaDECIL <- SE (limpiezaDECIL)

SE_saludTot <- SE (saludTot)
SE_saludDECIL <- SE (saludDECIL)

SE_transporteTot <- SE (transporteTot)
SE_transporteDECIL <- SE (transporteDECIL)

SE_educacionTot <- SE (educacionTot)
SE_educacionDECIL <- SE (educacionDECIL)

SE_personalTot <- SE (personalTot)
SE_personalDECIL <- SE (personalDECIL)

SE_transfeTot <- SE (transfeTot)
SE_transfeDECIL <- SE (transfeDECIL)


########## Coeficiente de variaci?n 
CV_gasmonTot <- cv(gasmonTot)
CV_gasmonDECIL <- cv(gasmonDECIL)

CV_alimentosTot <- cv(alimentosTot)
CV_alimentosDECIL <- cv(alimentosDECIL)

CV_vestidoTot <- cv(vestidoTot)
CV_vestidoDECIL <- cv(vestidoDECIL)

CV_viviendaTot <- cv(viviendaTot)
CV_viviendaDECIL <- cv(viviendaDECIL)

CV_limpiezaTot <- cv(limpiezaTot)
CV_limpiezaDECIL <- cv(limpiezaDECIL)

CV_saludTot <- cv(saludTot)
CV_saludDECIL <- cv(saludDECIL)

CV_transporteTot <- cv(transporteTot)
CV_transporteDECIL <- cv(transporteDECIL)

CV_educacionTot <- cv(educacionTot)
CV_educacionDECIL <- cv(educacionDECIL)

CV_personalTot <- cv(personalTot)
CV_personalDECIL <- cv(personalDECIL)

CV_transfeTot <- cv(transfeTot)
CV_transfeDECIL <- cv(transfeDECIL)


########## Limite inferior 
LI_gasmonTot <- confint(gasmonTot,level=0.90)[,1]
LI_gasmonDECIL <- confint(gasmonDECIL,level=0.90)[,1]

LI_alimentosTot <- confint(alimentosTot,level=0.90)[,1]
LI_alimentosDECIL <- confint(alimentosDECIL,level=0.90)[,1]

LI_vestidoTot <- confint(vestidoTot,level=0.90)[,1]
LI_vestidoDECIL <- confint(vestidoDECIL,level=0.90)[,1]

LI_viviendaTot <- confint(viviendaTot,level=0.90)[,1]
LI_viviendaDECIL <- confint(viviendaDECIL,level=0.90)[,1]

LI_limpiezaTot <- confint(limpiezaTot,level=0.90)[,1]
LI_limpiezaDECIL <- confint(limpiezaDECIL,level=0.90)[,1]

LI_saludTot <- confint(saludTot,level=0.90)[,1]
LI_saludDECIL <- confint(saludDECIL,level=0.90)[,1]

LI_transporteTot <- confint(transporteTot,level=0.90)[,1]
LI_transporteDECIL <- confint(transporteDECIL,level=0.90)[,1]

LI_educacionTot <- confint(educacionTot,level=0.90)[,1]
LI_educacionDECIL <- confint(educacionDECIL,level=0.90)[,1]

LI_personalTot <- confint(personalTot,level=0.90)[,1]
LI_personalDECIL <- confint(personalDECIL,level=0.90)[,1]

LI_transfeTot <- confint(transfeTot,level=0.90)[,1]
LI_transfeDECIL <- confint(transfeDECIL,level=0.90)[,1]



########## Limite superior 
LS_gasmonTot <- confint(gasmonTot,level=0.90)[,2]
LS_gasmonDECIL <- confint(gasmonDECIL,level=0.90)[,2]

LS_alimentosTot <- confint(alimentosTot,level=0.90)[,2]
LS_alimentosDECIL <- confint(alimentosDECIL,level=0.90)[,2]

LS_vestidoTot <- confint(vestidoTot,level=0.90)[,2]
LS_vestidoDECIL <- confint(vestidoDECIL,level=0.90)[,2]

LS_viviendaTot <- confint(viviendaTot,level=0.90)[,2]
LS_viviendaDECIL <- confint(viviendaDECIL,level=0.90)[,2]

LS_limpiezaTot <- confint(limpiezaTot,level=0.90)[,2]
LS_limpiezaDECIL <- confint(limpiezaDECIL,level=0.90)[,2]

LS_saludTot <- confint(saludTot,level=0.90)[,2]
LS_saludDECIL <- confint(saludDECIL,level=0.90)[,2]

LS_transporteTot <- confint(transporteTot,level=0.90)[,2]
LS_transporteDECIL <- confint(transporteDECIL,level=0.90)[,2]

LS_educacionTot <- confint(educacionTot,level=0.90)[,2]
LS_educacionDECIL <- confint(educacionDECIL,level=0.90)[,2]

LS_personalTot <- confint(personalTot,level=0.90)[,2]
LS_personalDECIL <- confint(personalDECIL,level=0.90)[,2]

LS_transfeTot <- confint(transfeTot,level=0.90)[,2]
LS_transfeDECIL <- confint(transfeDECIL,level=0.90)[,2]


#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_gasmonTot,ES_gasmonDECIL),
             c(ES_alimentosTot,ES_alimentosDECIL),
             c(ES_vestidoTot,ES_vestidoDECIL),
             c(ES_viviendaTot,ES_viviendaDECIL),
             c(ES_limpiezaTot,ES_limpiezaDECIL),
             c(ES_saludTot,ES_saludDECIL),
             c(ES_transporteTot,ES_transporteDECIL),
             c(ES_educacionTot,ES_educacionDECIL),
             c(ES_personalTot,ES_personalDECIL),
             c(ES_transfeTot,ES_transfeDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_gasmonTot,SE_gasmonDECIL),
             c(SE_alimentosTot,SE_alimentosDECIL),
             c(SE_vestidoTot,SE_vestidoDECIL),
             c(SE_viviendaTot,SE_viviendaDECIL),
             c(SE_limpiezaTot,SE_limpiezaDECIL),
             c(SE_saludTot,SE_saludDECIL),
             c(SE_transporteTot,SE_transporteDECIL),
             c(SE_educacionTot,SE_educacionDECIL),
             c(SE_personalTot,SE_personalDECIL),
             c(SE_transfeTot,SE_transfeDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_gasmonTot,CV_gasmonDECIL),c(CV_alimentosTot,CV_alimentosDECIL),c(CV_vestidoTot,CV_vestidoDECIL),c(CV_viviendaTot,CV_viviendaDECIL)
             ,c(CV_limpiezaTot,CV_limpiezaDECIL),c(CV_saludTot,CV_saludDECIL),c(CV_transporteTot,CV_transporteDECIL),
             c(CV_educacionTot,CV_educacionDECIL),c(CV_personalTot,CV_personalDECIL),c(CV_transfeTot,CV_transfeDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_gasmonTot,LI_gasmonDECIL),c(LI_alimentosTot,LI_alimentosDECIL),c(LI_vestidoTot,LI_vestidoDECIL),
             c(LI_viviendaTot,LI_viviendaDECIL),c(LI_limpiezaTot,LI_limpiezaDECIL),c(LI_saludTot,LI_saludDECIL),c(LI_transporteTot,LI_transporteDECIL),c(LI_educacionTot,LI_educacionDECIL)
             ,c(LI_personalTot,LI_personalDECIL),c(LI_transfeTot,LI_transfeDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_gasmonTot,LS_gasmonDECIL),c(LS_alimentosTot,LS_alimentosDECIL),c(LS_vestidoTot,LS_vestidoDECIL),c(LS_viviendaTot,LS_viviendaDECIL)
             ,c(LS_limpiezaTot,LS_limpiezaDECIL),c(LS_saludTot,LS_saludDECIL),c(LS_transporteTot,LS_transporteDECIL),
             c(LS_educacionTot,LS_educacionDECIL),c(LS_personalTot,LS_personalDECIL),c(LS_transfeTot,LS_transfeDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
DECILES<-c("MEAN", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("gasto", "Food", "Clothes", "Housing","Cleaning", "Health care","Transportation", "Education", "Personal spending","Transferences")

names(c_DECIL_SE)=c("gasto", "Food", "Clothes", "Housing","Cleaning", "Health care","Transportation", "Education", "Personal spending","Transferences")

names(c_DECIL_CV)=c("gasto", "Food", "Clothes", "Housing","Cleaning", "Health care","Transportation", "Education", "Personal spending","Transferences")

names(c_DECIL_LI)=c("gasto", "Food", "Clothes", "Housing","Cleaning", "Health care","Transportation", "Education", "Personal spending","Transferences")

names(c_DECIL_LS)=c("gasto", "Food", "Clothes", "Housing","Cleaning", "Health care","Transportation", "Education", "Personal spending","Transferences")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

prueba<-c_DECIL_ES%>%
  mutate(prueba=Food2010+Clothes2010+Housing2010+
          Cleaning2010+Health care2010+Transportation2010+
           Education2010+Personal spending2010+transfe2010)
            
            
all.equal(c_DECIL_ES$GASTO2010,prueba$prueba)



write.dbf(c_DECIL_ES,file = "CONSUMO Nacional por fuente por DECIL estimaciones 2010.dbf")
write.dbf(c_DECIL_SE,file = "CONSUMO Nacional por fuente por DECIL errores standard 2010.dbf")
write.dbf(c_DECIL_CV,file = "CONSUMO Nacional por fuente por DECIL CV 2010.dbf")
write.dbf(c_DECIL_LI,file = "CONSUMO Nacional por fuente por DECIL LI 2010.dbf")
write.dbf(c_DECIL_ES,file = "CONSUMO Nacional por fuente por DECIL LS 2010.dbf")

rm(list = ls())

########### Grafica
library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)


setwd("C:/Users/Erick/OneDrive/GIC/Consumo_por_fuente")
Consumo_por_decil_2010<-read.dbf("CONSUMO Nacional por fuente por DECIL estimaciones 2010.dbf",as.is = T)

base_grafica<-Consumo_por_decil_2010%>%
  mutate(Food=(Food/gasto)*100,
         Clothes=(Clothes/gasto)*100,
         Housing=(Housing/gasto)*100,
         Cleaning=(Cleaning/gasto)*100,
         Health.car=(Health.car/gasto)*100,
         Transporta=(Transporta/gasto)*100,
         Education=(Education/gasto)*100,
         Others=((Personal.s+Transferen)/gasto)*100)

prueba<-base_grafica%>%
  mutate(Food+Clothes+Housing+Cleaning+Health.car+Transporta+Education+Others)

prueba$`+...`


base_grafica$gasto<-NULL

base_grafica<-base_grafica%>%
  mutate(Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

base_grafica<-base_grafica%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

base_grafica<-melt(base_grafica)












ggplot(base_grafica, aes(fill=variable, y=value, x=Deciles)) + 
  geom_bar(position="stack", stat="identity")


GIC<-base_grafica%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles, y=value , fill= variable),position= "dodge")+
  geom_col()
GIC
