#librerias 
require("dplyr")
require("ggplot2")

# wd
setwd("F:\\BACKUPPERSONAL\\LESZCZUK\\DiscoPosMorten\\Investigacion\\0-Propia\\TESIS_DOCTORAL\\6-RepositorioGitHub\\CapituloI")

# data
datos <- read.csv("BBDD.csv", sep = ";")

# data exploration
str(datos)
datos$Fecha <- as.Date(datos$Fecha, format = "%d/%m/%Y")
datos$Producto  <-  factor(datos$Producto, levels = c("pulpable", "8 pie", "10 pie", "12 pie", "16 pie") ) 

# 
names(datos)
ggplot(datos, aes(Vol_i, fcd,  color = Producto))+
  geom_boxplot()
ggplot(datos, aes(DE, fcd,  color = Equipo))+
  geom_point()
ggplot(datos, aes(DE, PEF,  color = Equipo))+
  geom_point()
ggplot(datos, aes(Vol_i, fcd,  color = Equipo))+
  geom_boxplot()


ggplot(datos, aes(Producto, pendiente))+
  geom_boxplot()

ggplot(datos, aes(Vol_i, PEF, color=Producto))+
  geom_boxplot()

ggplot(datos, aes(pendiente, PEF, color = Producto))+
  geom_point()+
  geom_smooth(method = "lm",formula = y ~ x, se = F)

ggplot(datos, aes(Producto,PEF))+
  geom_boxplot()+
  geom_point(aes( color = Producto))+
  theme(text = element_text(size = 20))+
  facet_wrap(~Equipo, nrow = 2)
  

names(datos)
datos[is.na(datos$Producto), 10] <- "pulpable"


ggplot(datos, aes(Producto, PEF ,color = Equipo))+
  geom_boxplot()+
  facet_wrap(~Equipo, nrow = 2)+
  theme(text = element_text(size = 20))
ggplot(datos, aes(DE, PEF ,color = Equipo))+
  geom_point()+
  geom_smooth(method = "lm", formula=y~log(x) )+
  facet_wrap(~Equipo, nrow = 2)+
  theme(text = element_text(size = 20))
ggplot(datos, aes(fcd, PEF ,color = Equipo))+
  geom_point()+
  geom_smooth(method = "lm", formula=y~log(x) )+
  facet_wrap(~Equipo, nrow = 2)+
  theme(text = element_text(size = 20))

ggplot(datos, aes(Producto,fcd))+
  geom_boxplot()+
  geom_point(aes( color = Producto))+
  theme(text = element_text(size = 20))+
  facet_wrap(~Equipo, nrow = 2)
# Pendiente
names(datos)
ggplot(datos, aes(pendiente, PEF))+
  geom_point()+
  geom_smooth(method = "lm", formula=y~(x) )+
  ggtitle("Productividad en f(pendiente)")+
  theme(text = element_text(size = 20))
ggplot(datos, aes(pendiente, TAE/3600))+
  geom_point()+
  geom_smooth(method = "lm", formula=y~(x) )+
  ggtitle("Productividad en f(tiempo por ciclo)")+
  theme(text = element_text(size = 20))
  
ggplot(datos, aes(pendiente, fcd))+
  geom_point()+
  geom_smooth(method = "lm", formula=y~(x) )+
  ggtitle("Productividad en f(tiempo por ciclo)")+
  theme(text = element_text(size = 20))
