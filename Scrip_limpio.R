# Librerias:----

library(sp)
library(maptools)
library(leaflet)
library(raster)
library(sf)
library(units)
library(smoothr)
library(usedist)
library(geosphere)
library(dplyr)

# defino el lugar de trabajo ----
setwd("F:\\BACKUPPERSONAL\\LESZCZUK\\DiscoPosMorten\\Investigacion\\0-Propia\\TESIS_DOCTORAL\\3-Modelos\\Analsis_Datos_FW\\DatosSelvaSRL\\ProcesamientoVIdeo\\Procesados")

# Datos ----
datos <- read.csv("BBDD_all_ciclos.csv", sep = ";")
datos$Fecha <- as.Date(datos$Fecha, format = "%d/%m/%Y" ) # Especificacion que la fecha es tipo date
datos$Fecha <- as.character(datos$Fecha)
datos <- datos[order(datos$Fecha),]
unique(datos$Fecha)

# Creo una columna nueva y le asigno los valores de longitud y latitud en coordendas geograficas
# pasar los datos a lat y long posgar
for (i in 1:dim(datos)[1]){
  if (!is.null(datos[i,20])) {
    datos$Lat_GEO_sh[i] <- datos[i,20]
    datos$Lon_GEO_sh[i] <- datos[i,21]
    #print(datos$Lat_GEO_sh)
    
  } else if(is.na(datos[i,18])) {
    datos$Lat_GEO_sh[i] <- datos[i-1,20]
    datos$Lon_GEO_sh[i] <- datos[i-1,21]
  }
}

# relleno los datos vacios con valores fijos
for (j in 1:dim(datos)[1]){
  if (!is.na(datos[j,34])){
    

    datos$Lat_GEO_sh[j] <- datos[j,20]
    datos$Lon_GEO_sh[j] <- datos[j,21]
  } else if (is.na(datos[j,34])) {
    

    datos$Lat_GEO_sh[j] <- -26
    datos$Lon_GEO_sh[j] <- -54
  }
}



# Transformation of spacial information
# Creating a Data Frame to Spatial Objects
# the data should dont have empty data because de funtion dont work well

datos_sf <- st_as_sf(datos, coords = c("Lon_GEO_sh","Lat_GEO_sh"), crs = 4326)

#plot(datos_sf$geometry)

datos_sf <- st_transform(datos_sf, crs = 22177)
datos_sf <- datos_sf[order(datos_sf$Fecha),]
# Extraer las coordenadas transformadas
extraccion <- data.frame()
for (i in 1:length(datos_sf$geometry)){
  
  extraccion[i,1] <-  datos_sf$geometry[[i]][1]
  extraccion[i,2] <-  datos_sf$geometry[[i]][2]
}

datos_sf$Lon_Posgar <- round(extraccion[,1],0)
datos_sf$Lat_Posgar <- round(extraccion[,2],0)

names(datos_sf)

for (k in 1:dim(datos_sf)[1] ){
  if ( is.na(datos_sf$Lat_GEO[k])) {
    datos_sf$Lat_Posgar[k] <- NA
    datos_sf$Lon_Posgar[k] <- NA
  }
}

names(datos_sf)

fecha <- unique(datos$Fecha)
bloques <- unique(datos$bloque)

dato <- group_by(datos, Fecha, bloque) %>% summarise(n = n())
dato <- as.data.frame(dato)


x <- split(datos, datos$Fecha)
x2 <- list()
for (p in 1:6){ 
  x2[[p]] <- split(x[[p]], x[[p]]$bloque )
  }


  
# Funcion para separar objetos 
lapply(seq_along(x), 
       function(i,x) {assign(paste0("a",i),x[[i]], envir=.GlobalEnv)},
       x=x)

# for (l in 1:length(fecha) ) {
#   for (m in 1:length(bloques) ) {
#     
#     x[[l]] <- data.frame(datos_sf[datos_sf$Fecha == fecha[l],35],
#                        datos_sf[datos_sf$Fecha == fecha[l],36],
#                        datos_sf[datos_sf$Fecha == fecha[l],31],
#                        datos_sf[datos_sf$Fecha == fecha[l],3])
#   
#     x[[l]][,c(2,4,6,8)] <- NULL
#   }
# }
# names(x) <- c("Lon_Posgar", "Lat_Posgar", "Ciclo" ,"Id")


# # Necesito armar una base de datos con los datos que pide la Funcion
# data <- as.data.frame(x[[1]])
# 
# #data <- as.data.frame(datos_sf[,c(35:36,31,3)])
# 
# 
# data <- DF6[complete.cases(DF6$Lon_Posgar),]

names(data)
# Pasar de puntos a lineas ----
v_lines <- points_to_line(data = data ,long = "Lon_Posgar", lat =  "Lat_Posgar",id_field =  "Ciclo.1" , sort_field = "ID_General" )


data <- data.frame()

for (m in 1:length(x)) {
  assign(paste0("DF", m), data.frame(x[[m]]) )
}


plot(v_lines)


proj4string(v_lines) <- CRS("+proj=tmerc +lat_0=-90 +lon_0=-54 +k=1 +x_0=7500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
sf<- st_as_sf(v_lines)
st_write(sf, "point_to_line6.shp", driver="ESRI Shapefile")
sf

# Funciona pero no colabora mucho
# v_lines_s <- smooth(v_lines, method = "densify")

#v_lines_s <- st_simplify(sf, dTolerance = 3)
#plot(v_lines_s)

#Centroides <- st_centroid(v_lines_s)
#Centroides_in <- st_point_on_surface(v_lines_s)

# Para escribir como shapefile
#st_write(v_lines, "lines1.shp", driver="ESRI Shapefile")


#st_write(Centroides_in, "shapefile_centroides_in.shp", driver="ESRI Shapefile")


plot(v_lines)
plot(v_lines_s)
plot(Centroides)
plot(Centroides_in)


# Medicion entre las distancias de los centros geometricos de los centroides 
# Leemos el archivo de la zona de carga

Zona_carga <- st_read("F:/BACKUPPERSONAL/LESZCZUK/DiscoPosMorten/Investigacion/0-Propia/TESIS_DOCTORAL/2-GIS/0-CapI/0-vectorial/Zona_carga.shp")
ZC_centoid <- st_centroid(Zona_carga)

# Determinar las distancias entre los objetos
cordernadas_ZC <- ZC_centoid$geometry[[1]][1]

for (i in 1:length(Centroides_in[[1]])){
  #print(Centroides_in[[1]][i])
  #print(Centroides_in$geometry[[i]][2])
  
 distancias[i] <-  sqrt( (ZC_centoid$geometry[[1]][1] - Centroides_in$geometry[[i]][2] )^2 + (Centroides_in$geometry[[i]][1] - ZC_centoid$geometry[[1]][2] )^2)
}
distancias

unique(datos_sf$Actividad.1)
datos_sf[datos_sf$Actividad.1=="MOV",]

# Quiero hacer una linea que pase por el medio de una cantidad indeterminada de puntos!

install.packages("lubridate")

suavizado <- lowess(x = datos$lon_pos.1,y =datos$lat_pos.1, f = 0.01 )
plot(datos$lon_pos.1, datos$lat_pos.1)
plot(suavizado$x, suavizado$y)

ciclo1 <-datos[datos$Ciclo.1 == 1,]
ciclo1 <-ciclo1[ciclo1$Actividad_ajustada =="MOV",]
suav1 <- lowess(x = ciclo1$lon_pos.1, y = ciclo1$lat_pos.1, f = 0.8)
plot(suav1$x, suav1$y)
plot(ciclo1$lon_pos_red, ciclo1$lat_pos.1)






