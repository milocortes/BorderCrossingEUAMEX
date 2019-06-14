#clear workspace
rm(list = ls(all = TRUE)) 

#### Descargue los datos de la siguiente liga:
# Fuente https://data.transportation.gov/api/views/keg4-3bc2/rows.csv?accessType=DOWNLOAD
# Descarga el archivo Border_Crossing_Entry_Data.csv
####   Cargamos la base de datos de cruces fronterizos
border_cross<-read.csv(".../Border_Crossing_Entry_Data.csv")

# Nos quedamos sólo con los cruces US-MEX
border_cross<-subset(border_cross,Border=="US-Mexico Border")

# Extraemos longitud y latitud
library(stringi)
library(stringr)

border_cross$Location<-as.character(border_cross$Location)


border_cross$LAT<-as.numeric(str_extract(border_cross$Location, "\\-*\\d+\\.*\\d*"))
border_cross$LONG<-as.numeric(str_extract(border_cross$Location, "[-]\\-*\\d+\\.*\\d*"))

## Haremos subset para quedarnos con los cruces correspondientes a los contenedores transportados por carril y camión

border_cross_trucks<-subset(border_cross,Measure=="Rail Containers Empty" | Measure=="Rail Containers Full" | Measure=="Trains" |Measure=="Truck Containers Empty" | Measure=="Truck Containers Full" | Measure=="Trucks")

## Generamos un dataframe con los campos Value, LONG y LAT
bctmelt<-border_cross_trucks[,c("Value","LONG","LAT")]
## Agregamos un campo año
bctmelt$year<-as.numeric(substr(border_cross_trucks$Date,7,10))

## Haremos una consulta sql para agregar por año y por cruce

library(sqldf)

options(sqldf.driver = "SQLite")

bctmelt<-sqldf("SELECT year,LONG,LAT ,SUM(value) FROM bctmelt GROUP BY year,LONG,LAT")

names(bctmelt)<-c("year","lon","lat","Miles")

bctmelt$Miles<-bctmelt$Miles/1000

### Usaremos ggmap para obtener el mapa de la frontera

library(ggmap)
us <- c(left = -120, bottom = 25, right = -95, top = 35)
frontera<-get_stamenmap(us, zoom = 6, maptype = "toner-lite") %>% ggmap()
#### Ahora haremos el gif

library(animation)
library(lubridate)
library(XML)
library(mosaic)

saveGIF({
  for (i in 1996:2018) {
    circle_scale_amt=0.01
    frontera_gif<-frontera+geom_point(aes(x=lon, y=lat,size=Miles), data=subset(bctmelt,year==i),col="orange", alpha=.9)
    anio=as.character(i)
    print(frontera_gif+labs( title= "Cruces fronterizos entre EUA y México de contenedores por carril y camión ", subtitle=anio,y="Latitud", x = "Longitud",caption = "Fuente: US.Departament of Transportation. Transportation.gob\nhttps://data.transportation.gov/api/views/keg4-3bc2/rows.csv?accessType=DOWNLOAD"))
    
  }
  
},
interval=.1, ani.width = 800, ani.height = 400)
