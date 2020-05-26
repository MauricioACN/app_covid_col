##### Paquetes -----------------------------------------------------
library(tidyverse)
library(shinydashboard)
library(devtools)
library(htmltools)
library(fastmap)
library(shinyWidgets)
library(plotly)

##### Cargue de datos -----------------------------------------------------
datos_inter3 = readRDS("datos_inter3.rds")
enfermedad = data.frame(enfermedad = c("Enfermedades Cardiovasculares","Cancer","EPOC","Diabetes","Hipertension"),
                        riesgo = c(0.105,0.056,0.063,0.073,0.06))
enfermedad$enfermedad = as.character(enfermedad$enfermedad) 
mapa = read_rds("mapa.rds")
##


mapColmun <- function(mapa){ggplot(mapa,aes(label=Municipio,label2=recuperado,label3=fallecido)) +
  geom_polygon(aes(x=long, y=lat,group=group,
                   fill = Quantil), colour ="white", size = 0.1) +
  labs(title = "Distribución", fill = "") +
  labs(x="",y="",title="Contagios por Departamento") +
  scale_x_continuous(limits=c(min(mapa$long),max(mapa$long)))+
  scale_y_continuous(limits=c(min(mapa$lat),max(mapa$lat)))+
  scale_fill_manual(values = c("#3E7F3D","#359C33","#88E440","#A8F46D","#E2FF8E"), na.value = "#C2CE9D")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())}

