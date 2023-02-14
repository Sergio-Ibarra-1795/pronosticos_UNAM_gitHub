

Fallos=read.table("C:\\Users\\Jair\\Desktop\\Labos_R\\Descriptiva\\Datos\\Fallos.txt")
Acciones=read.table("C:\\Users\\Jair\\Desktop\\Labos_R\\Descriptiva\\Datos\\Acciones.txt")
Importaciones=read.table("C:\\Users\\Jair\\Desktop\\Labos_R\\Descriptiva\\Datos\\Importaciones.txt",
                         encoding = "UTF-8")

MATPREP = read.table("C:\\Users\\Jair\\Desktop\\Labos_R\\Descriptiva\\Datos\\MatPREP.txt", header=TRUE)


###  read.csv2 datos con csv2 (excel)

MATPREP=read.table("C:\\Users\\Jair\\Desktop\\Labos_R\\Descriptiva\\Datos\\MatPREP.txt", header=TRUE)
 
is(MATPREP)

is(Importaciones)

summary(MATPREP)

MATPREP= data.frame(MATPREP)

## Matrix
MATREs = matrix (c(4,55,7,9,8,10), nrow=2, ncol = 3)
MATREs
## as.vector

VecUNO= c(2,4,7)
VecUNO

head(MATPREP)
MATPREP_F = subset(MATPREP, Sexo =="F")
tail(MATPREP_F)
help("subset")
ColSexo=MATPREP$Sexo

MatSexo <- as.factor(MATPREP$Sexo)  ## <- = son equivalentes
MatSexo = as.factor(MATPREP$Sexo)

summary(MatSexo)
names(MATPREP)
head(MATPREP)
dim(MATPREP)
tail(MATPREP)
MATPREP[1,1]

MATPREP[1:10,1]

MATPREP[,1:10]

MATPREP[c(1,3,5),6]

MATPREP[c(1,3,5),1:2]

MATPREP = as.data.frame(MATPREP)

is(MATPREP)

summary(MATPREP)



EARTHQUAKE<-read.table("C:\\Users\\Hp\\Desktop\\Labos_R\\Descriptiva\\Datos\\EARTHQUAKE.txt")




## Cuando el primer registro es la etiqueta de la variable
###EARTHQUAKE<-read.table("F:\\Labos_R\\Descriptiva\\Datos\\EARTHQUAKE.txt", header="TRUE")


install.packages("foreign")
####library(foreign)

#drt<-read.csv2("E:\\Datos\\Libro1.csv", sep=",", header="TRUE")
#write.csv(drt,"C:\\Escritorio\\Libro1.csv")
#write.table(drt,"C:\\Escritorio\\Libro1.txt")

Fallos

names(Fallos)

summary (Fallos)

dim(Fallos)

a<-array(Fallos$V1, dim=c(10,4) )

## data.frame  modelos est.
# matrix -- as.matrix
## vector c()
####

AD=c(1,8,7,18)

DT=matrix(AD, nrow=2 , ncol =2 )
DT=matrix(c(0), nrow=10 , ncol =4 )
DT=matrix(c(1:10), nrow=10 , ncol =4 )
DT=matrix(c(2,7,8,5), nrow=2 , ncol =2 )

DT
is(DT)


DTm=as.data.frame(DT)

is(DT)
is(DTm)

help(summary)
help

lt<-c(1,2,34,6)


a= Fallos$V1
hista=hist(a)

hista$breaks ### clases
hista$counts #### Freq absoluta
sum(hista$counts) #### Total de Observaciones

#hista$intensities
hista$counts/sum(hista$counts) ### Freq relativa

###Tablas de Frecuencias####

Tabla= table(a)
Tabla ### Freq.abs
Tabla/margin.table(Tabla) ### Freq.rel
cumsum(Tabla) ### Freq.abs Acumulada
cumsum(Tabla)/margin.table(Tabla) ### Freq.rel Acumulada


#######################


b=Acciones$V1
#b<-array(Acciones$V1,dim=c(4,5) )
b=Acciones$V1

clases=5  ### Indica Número de Intervalos de Clase
int=(max(b)-min(b))/clases #### Tamaño de Intervalo de Clase

###hist(datos, seq( mas dats, min datos, by=(tam int)))

####   utilizar <- o = de forma indistinta 

hist(b, seq(min(b),max(b), by=(int)))

histb=hist(b, seq(min(b),max(b), by=(int)))

names(histb)
summary(histb)



histb$breaks ### Extremos de los Intervalos de Clase 
histb$counts/sum(histb$counts) #### Frecuencia relativa
histb$counts ### Frecuencia absoluta
histb$mids #### Marcas de Clase

### Histograma de Frecuencias Absolutas
hist(b, seq(min(b),max(b), by=(int)), 
     xlab="Precio de Acciones", 
     ylab="Frecuencia Absoluta", main="Histograma", axes=FALSE)

axis(1,seq(min(b),max(b), by=(int)))
axis(2, seq(0,5, by=(1)))
#######



clases=5  ### Indica Número de Intervalos de Clase
int=(max(b)-min(b))/clases #### Tamaño de Intervalo de Clase


### Histograma de Frecuencias Relativas
d<- hist(b,seq(min(b),max(b), by=(int)), plot=F)
d$counts/length(b)
d$density<-d$counts/length(b)


plot(d,  xlab="Precio de acciones", ylab="Frecuencia Relativa", 
     main="Histograma",  axes=FALSE, freq=F)

axis(1,seq(min(b),max(b), by=(int)))
axis(2)

help(plot)



#######

### Poligonos de Frecuencias Relativas
colordf<-rgb(.25,.15,.70)

d<- hist(b,seq(min(b),max(b), by=(int)), plot=F)
d$counts/length(b)
d$density<-d$counts/length(b)
plot(d, xlab="Precio de acciones", 
     ylab="Frecuencia Relativa", main="Histograma",  axes=FALSE, 
     freq=F)

lines(c(min(d$breaks),d$mids,max(d$breaks)),
      c(0,d$density,0),type="l",col=colordf)
axis(1,seq(min(b),max(b), by=(int)))
axis(2)
#######



##### Diagrama de Ojivas####
clases<-5  ### Indica Número de Intervalos de Clase
int<-(max(b)-min(b))/clases #### Tamaño de Intervalo de Clase
d<-hist(b, seq(min(b),max(b), by=(int)), plot=F)
frq.ac<-d$counts/sum(d$counts) ### freq rel acu
d$mids #### Marcas de Clase
plot(d$mids, cumsum(frq.ac), axes=FALSE, main="Diagrama de Ojiva",
     xlab="Precio de Acciones", ylab="Frecuencia acumulada") 
axis(1,c(0,d$mids))
axis(2,seq(0,1,by=.1))
lines(d$mids, cumsum(frq.ac), col="red")  
##############################

####### Diagrama de Barras####


Imp=Importaciones
Imp

colors()

colordf<-rgb(.25,.13,.30)
help(par)

Imp$V1
Imp$V2

par (las =3)

barplot(Imp$V2, names.arg=Imp$V1, 
        col=c("beige","azure3","bisque3", "bisque4", "black", 
              "blanchedalmond", "blue", colordf),
        main="Importaciones", 
        xlab="País", ylab="Porcentaje", cex.names=.7,width=.9, 
        ylim=c(0,70) )


help("barplot")

###3  Fallos

colordf<-rgb(.25,.13,.30)

table(Fallos)

Tabla<- table(a)
Tabla ### Freq.abs
Tabla/margin.table(Tabla) ### Freq.rel
cumsum(Tabla) ### Freq.abs Acumulada
cumsum(Tabla)/margin.table(Tabla) ### Freq.rel Acumulada


FreRel =  100*(Tabla/margin.table(Tabla) )
NamesClases= c("1", "2", "3", "4", "6")

#levels(DT$namecolumn) para clases

par (las =1)
barplot(FreRel, names.arg=NamesClases, 
        col=c("beige","azure3","bisque3", "bisque4", 
              "blanchedalmond"), main="Frecuencia Fallos Diarios", 
        xlab="Número de Fallos", ylab="Frecuencia Relativa %", cex.names=.7,width=.9, 
        ylim=c(0,35) )



CO = rep(100,100)

Dcolors=as.vector(colors())


barplot(CO, col=Dcolors)

colors()


# 
# library (gplots)

install.packages("gplots")
library (gplots)

# help(barplot2)

## colors()

### rgb #####

namesdos<-c("USA", "Jpn", "Alem", "Ca", "Chi", "Cor", "Tai", "Itl", "Otros")


## revisar par
##

par(las=3) 

barplot(Imp$V2, names.arg=namesdos, 
        col=c(7,2,3,4,5,6,8,9,10), main="Importaciones", 
        xlab="País", ylab="Porcentaje",  cex.names=.7,width=.9, ylim=c(0,70)) 

####################333

table(Fallos)






####### Diagrama de Pastel####

lbls<-c("USA", "Jpn", "Alm", "Ca", "Chi", "Cor", "Tai", "Itl", "Otros")

lbls <- paste(lbls, Imp$V2) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(Imp$V2, labels=lbls, col=c("bisque4", "black", "blanchedalmond", "blue", "blue1", colordf, "red")
    ,main="Importaciones", cex=.5, cex.main=.8) 
####################

help(pie)

##### Medidas Centrales####

dr<-c(9,2,7,11,14,7,2,7)
summary(dr)
mean(dr)
median(dr)

##############

### Boxplot
dr<-c(9,2,7,11,14,7,2,7)

summary(dr)

boxplot(dr, horizontal=TRUE, main= "Box Plot", xlab="", 
        ylab="Fallos")  # horizontal box plot 

help("boxplot")

##### Medidas Dispersion####
dr<-c(9,2,7,11,14,7,2,7)
range(dr)[2]-range(dr)[1] ### Rango
var(dr)#### varianza
sd(dr)### desv est
sd(dr)/mean(dr) #### Coeficiente de variación

quantile(dr, .5, type=1) ### cuantil o percentil
quantile(dr, .5, type=2) ### con metodo lineal
##############




PRACTICA


MATPrep<-read.table("C:\\Users\\Jair\\Desktop\\Labos_R\\Descriptiva\\Datos\\MATPrep.txt", header=TRUE)

read.csv2 ##  archivos csv

smeIDEO<-read.csv2("C:\\Users\\Jair\\Desktop\\smeIDEO.csv",sep=",", header=TRUE)

MATPrep<-read.csv2("C:\\Users\\Jair\\Desktop\\Labos_R\\Descriptiva\\Datos\\MATPrep.txt",sep=",", header=TRUE)

MAT<-read.csv2("C
VarA

install.packges("foreign")

library("foreign")


head(MATPrep)

MATPrepBEN=subset(MATPrep, Parque=="ALV" )  ## Filtrar

MATPrepBM=subset(MATPrep, Sexo=="M" )  ## Filtrar

MATPrepALV_M=subset(MATPrep, Parque=="ALV" & Sexo=="M")  ## Filtrar

summary(MATPrepALV_M)

Parque=as.factor(MATPrep$Parque)

summary(Parque)


MATPrep$Edad


names(MATPrep)

DRT<-cbind(MATPrep$Sexo, MATPrep$Parque, MATPrep$Edad)

DRR<-rbind(MATPrep$Sexo, MATPrep$Parque) 

DRT<-data.frame(MATPrep$Sexo, MATPrep$Parque, MATPrep$Edad)

summary(DRT)

LK<-subset(MATPrep, Parque=="BEN")

MatPREPBENSEx<-subset(MATPrep, Parque=="BEN" &  Sexo=="F")

# Estudiar la asistencia a los parques (U_12) por Edad



##

Los datos de la tabla MatPREP, son los resultados de una encuesta 
en parques de la ciudad de México;


Estudiar las variables Parque, Sexo y Edad, y Frencuencia en su uso: U_12

cinco o más días      5
tres a cuatro días    3
una a dos días        1
nunca                 0

summary(as.factor(MATPrep$Parque))
summary(as.factor(MATPrep$U_12))
summary(as.factor(MATPrep$Sexo))
summary((MATPrep$Edad))




Los resultados deben servir para conocer el perfil de los que visitan 
los parques, así como  la frecuencia con la que lo hacen

A partir del análisis descriptivo, 

MATPrep<-read.table("G:\\Labos_R\\Descriptiva\\Datos\\MATPrep.txt")





x= c(-2,-1,0,1,2)
y = c(0,0,1,1,3)

dataxy=data.frame(cbind(x,y))

names(dataxy)

plot(x,y, type="l")

ajuste1=lm(y~ 1+x, data=dataxy)
summary(ajuste1)
plot(ajuste1)



help(plot)
help(lm)
