library(readxl)
library(tidyverse)
library(extrafont)
library(tidygraph)
library(ggraph)
library(ggflags)

#El Excel con los datos se descarga aquí: https://data.world/datagraver/eurovision-song-contest-scores-1975-2019
#Hay que estar registrado en data.world para ello

#Cargamos los datos
df <- read_excel(tf)

#Hacemos un preprocesamiento para obtener el grafo
df2 <- df %>%
  filter(`(semi-) final` == "f" & is.na(Duplicate)) %>%           #Quitamos resultados de semifinales y filas con mismo origen y destino
  filter(ifelse(Year >= 2016, 
                ifelse(`Jury or Televoting` == "T", TRUE, FALSE), #Quitamos también los resultados del jurado desde 2016
                TRUE)) %>%                                        #(nos quedamos sólo con televoto desde entonces)
  select(`From country`, `To country`, Points) %>%                #Nos quedamos ahora con tres variables: origen, destino y puntos
  group_by(`From country`, `To country`) %>%                      #Mediante un group_by() %>% summarise(), obtenemos el número medio de puntos por edición
  summarise(Points = mean(Points), ediciones = sum(n())) %>%      #de cada una de las parejas posibles de países, así como el número de ediciones en las que
                                                                  #se pudo formar cada pareja (es decir, núm. de ediciones en las que el país de origen pudo votar al de destino)
  filter(ediciones > 5                                            #Nos quedamos sólo con aquellas parejas que se dieron en más de 5 ediciones...
         & Points > 5                                             #...y cuyo número medio de puntos por edición en la pareja sea superior a 5.
         )                                                        #(Para obtener algunas estadísticas del hilo, habrá que eliminar la fila anterior)

#Dataset para crear las estadísticas de puntos por edición recibidos por cada país a lo largo de la historia de Eurovisión
puntxed <- df %>% filter(`(semi-) final` == "f" & is.na(Duplicate)) %>% 
  filter(ifelse(Year >= 2016, 
                ifelse(`Jury or Televoting` == "T", TRUE, FALSE),
                TRUE)) %>%
  group_by(`To country`, Year) %>%
  summarise(Points = sum(Points)) %>% ungroup() %>%
  group_by(`To country`) %>%
  summarise(Points = mean(Points),
            ediciones = sum(n())) %>%
  filter(ediciones > 5)

#Aquí empezamos a crear el grafo

#Primero creamos un objeto para el layout con create_layout
layout <- create_layout(as_tbl_graph(df2, directed = T), 
                        layout = 'fr', weights = df2$Points)

#Hacemos un vector con los nombres traducidos (en caso de que queráis añadir los nombres junto a las banderas de cada país)
#IMPORTANTE: cada país ha de coincidir con los nombres (en inglés) contenidos en layout$name elemento por elemento. Si no, o dará error o saldrá lo que no es.
nombres <- c("Albania", "Andorra",
             "Armenia", #"Australia",
             "Austria", "Azerbaiján",
             "Bielorrusia", "Bélgica",
             "Bosnia y Herzegovina", "Bulgaria",
             "Croacia", "Chipre",
             "Rep. Checa", "Dinamarca",
             "Estonia", "Macedonia",
             "Finlandia", "Francia",
             "Georgia", "Alemania",
             "Grecia", 
             "Islandia", "Irlanda",
             "Israel", "Italia",
             "Letonia", "Lituania",
             "Luxemburgo", "Malta",
             "Moldavia", "Monaco",
             "Montenegro", "Noruega",
             "Polonia", "Portugal",
             "Rumanía", "Rusia",
             "San Marino", "Serbia",
             "Eslovenia", "España",
             "Suecia", "Suiza",
             "Países Bajos", "Turquía",
             "Ucrania", "Reino Unido",
             "Yugoslavia","Hungría")

#Hacemos un vector también para el código de las banderas. Aplica la misma precaución que para los nombres.
flags <- c("al", "ad", "am", #"au",
           "at", "az", "by", "be",
           "ba", "bg", "hr", "cy",
           "cz", "dk", "ee", "mk",
           "fi", "fr", "ge", "de",
           "gr", "is",
           "ie", "il",
           "it",
           "lv", "lt", "lu", "mt",
           "md", "mc", "me", "no",
           "pl", "pt", "ro", "ru",
           "sm", "rs", "si", "ea",
           "se", "ch", "nl", "tr",
           "ua", "gb", NA, "hu")

#Y creamos el grafo.
ggraph(layout) +
  geom_edge_fan2(aes(width = Points^2), alpha = 0.5     #Nótese que he escalado el grosor de las líneas de forma cuadrática para que
                                                        #resalten más las conexiones más frecuentes
                 #, arrow = arrow(length = unit(0.25, "inches"), type = "closed")     #Por si queréis poner flechas en lugar de líneas (no lo aconsejo)
                 ) +   
  scale_edge_width(name = "Nº puntos", range = c(0.1,4)) +  #Con este comando añadimos las líneas
  geom_node_point(size = 15) +                              #Con este añadimos puntos, es muy importante que este vaya antes que geom_flag() y también que el argumento
                                                            #size sea el mismo aquí y en geom_flag para que no haya desnivel. Esto se hace principalmente por Yugoslavia,
                                                            #que al no tener bandera en ggflags lo tenemos que dejar como un punto negro.
  geom_flag(data = layout %>% mutate(country = flags),      #Añadimos las banderas. geom_flag funciona igual que geom_point pero con el argumento "country" donde pondremos
                     mapping = aes(x = x, y = y, country = country),  #el código de cada bandera
            size = 15, col = "black") +
  geom_node_label(aes(label = ifelse(nombres == "Yugoslavia", "Yugoslavia", NA)), 
                  repel = F, family = "Liberation Sans") +   
  theme_graph() + theme(legend.position = "none",
                        plot.background = element_rect("grey85")) +
  annotate("text", x = min(layout[,1])+0.5, y = max(layout[,2]), 
           label = "Twitter/Instagram:\n@Picanumeros",
           size = 6, family = "Forte") +
  labs(title = "Patrones de votación más repetidos entre países en Eurovisión desde 1975: cuanto más ancha sea una línea,\nmás ha votado (en promedio) un país al otro",
       subtitle = "Fuente: Datagraver.com accedido a través de data.world\nSe cuentan sólo aquellas votaciones que han ocurrido más de 5 veces, y las uniones cuyo promedio de puntos asignados por edición es superior a 5.\nA partir de 2016 (inclusive) se cuentan únicamente las puntuaciones otorgadas por televoto.") +
  ggsave("eurovision.png", dpi = 300, height = 12, width = 12)
