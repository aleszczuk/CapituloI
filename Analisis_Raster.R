library("raster")
library("sf")
library("dplyr")
library("tidyverse")

# cargamos el DEM alos de 12,5m
dem <- raster("F:\\BACKUPPERSONAL\\LESZCZUK\\DiscoPosMorten\\Investigacion\\0-Propia\\TESIS_DOCTORAL\\2-GIS\\0-CapI\\1-raster\\Vuelo_Drone\\Entrega_Final\\DEM_final_80cm_recorte.tif")
# COnvertimos a pendientes
pend <- terrain(dem, opt = 'slope', unit = 'degrees')

#Lectura de todos los puntos
puntos <- st_read("F:\\BACKUPPERSONAL\\LESZCZUK\\DiscoPosMorten\\Investigacion\\0-Propia\\TESIS_DOCTORAL\\6-RepositorioGitHub\\CapituloI\\BBDD_all_ciclosV1_22177.csv.shp")
st_crs(puntos) <- 22177
puntos<- puntos[!is.na(puntos$Punto),]


extraccion <- raster::extract(pend,puntos , buffer = 0.8, fun = mean, df = T)

puntos$pend <- extraccion$slope

# st_write(puntos, "BBDD_allC_slope", driver="ESRI Shapefile")
# getwd()


# ANalisis de los datos mediante dplyr
# resumen <- group_by(puntos, Fecha, bloque, Ciclo_1) %>% summarise( media = mean(pend))
# resumen <- as.data.frame(resumen[,1:4])
# write.csv(resumen, "resumen.csv")

# Reclasificacion
hist(pend)
m <- matrix(c(0, 10, 1, 
              10, 25, 2, 
              25, Inf, 3), ncol=3, byrow=TRUE)
re_pend <- reclassify(pend, m )
plot(re_pend)

# Pendientes con Dem de Alos
alos_dem <- raster("F:\\BACKUPPERSONAL\\LESZCZUK\\DiscoPosMorten\\Investigacion\\0-Propia\\TESIS_DOCTORAL\\2-GIS\\0-CapI\\1-raster\\Alos_Dem\\AlosDem125_22177.tif")
alos_dem
plot(alos_dem)
alos_pend <- terrain(alos_dem, opt = 'slope', unit = 'degrees')
plot(alos_pend)

m <- matrix(c(0, 5, 1, 
              5, 20, 2, 
              20, Inf, 3), ncol=3, byrow=TRUE)
re_alos_pend <- reclassify(alos_pend, m )
plot(re_alos_pend)
extraccion_alos <- raster::extract(re_alos_pend,puntos , buffer = 0.8, fun = mean, df = T)

puntos$pend_reclas_alos <- extraccion_alos$slope
st_write(puntos, "BBDD_allC_slope_alos", driver="ESRI Shapefile")
getwd()

library(ggplot2)

names(puntos)
ggplot(puntos, aes(Activida_1, pend))+
  geom_boxplot()+
  facet_wrap(~Fecha, nrow = 2)

ggplot(puntos, aes(Activida_1, pend))+
  geom_boxplot()+
  facet_wrap(~Fecha, nrow = 2)

ggplot(puntos, aes(Activida_1, pend_reclas_alos))+
  geom_boxplot()+
  facet_wrap(~Fecha, nrow = 2)

puntos$Ciclo_1 <- factor(puntos$Ciclo_1)
str(puntos)

ggplot(puntos, aes(Ciclo_1, pend))+
  geom_boxplot(aes(color = Ciclo_1) )+
  facet_grid(Activida_1 ~Fecha)


# Analisis por promedios de actividad
data1 <- group_by(puntos,Fecha, Ciclo_1, Activida_1) %>% summarise( media = mean(pend))

ggplot(data1, aes(Ciclo_1, media))+
  geom_point(aes(color = Ciclo_1) )+
  facet_grid(Activida_1 ~Fecha)


# Asociación de varias tablas. 
# Generacion de un clave primaria 

BBDD <- read.csv("F:\\BACKUPPERSONAL\\LESZCZUK\\DiscoPosMorten\\Investigacion\\0-Propia\\TESIS_DOCTORAL\\6-RepositorioGitHub\\CapituloI\\BasePonsseV3.csv", sep = ";")
str(BBDD)

# Prueba para ver si se unen de forma natural
names(puntos)
tb_pto <- puntos %>% select(fecha,bloque, "Ciclo" = "Ciclo_1",ID_General, Lat_GEO, Lon_GEO, Distancia, Activida_1, pend)
injerto <- as_tibble(tb_pto) %>% left_join(as_tibble(BBDD), by = c("fecha", "bloque", "Ciclo"))

names(injerto)
injerto %>%  group_by(fecha, Ciclo, Activida_1, injerto[[,21:45]]) %>% summarise( media = mean(pend))
