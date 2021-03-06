library(tidyverse)
library(ggthemes)
library(sf)
library(extrafont)

#Descargamos el shapefile de gadm.org
dat <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_ESP_2_sf.rds"))

#Líneas de código para acercar Canarias a la península ibérica sacadas de aquí: https://github.com/aaumaitre/tuto-R-iales/blob/main/mapa_es.R
canarias <- dat %>%
  filter(NAME_1 == "Islas Canarias")

canarias_new <- st_sf(st_drop_geometry(canarias),
                      geometry = st_geometry(canarias) + c(18, 7))

st_crs(canarias_new) <- st_crs(dat)

dat2 <- rbind(dat %>% filter(NAME_1 != "Islas Canarias"), canarias_new)

#Cargamos los datos de castillos por provincias (disponibles en esta misma carpeta)
cast <- read.csv("datos.csv")

#### GRÁFICO DE CASTILLOS POR HABITANTE ####

dat2 %>% left_join(cast %>% mutate(prov = str_trim(prov)), c("NAME_2" = "prov")) %>%    #Unimos el dataset del mapa y el de los datos de castillos usando el nombre de provincia
                                                                                        #(primero quitamos los espacios sobrantes a los lados mediante str_trim)
  ggplot() + geom_sf(aes(fill = pob_cast), col = "grey50") +                            #Iniciamos el ggplot vacío y metemos el shapefile con geom_sf, 
                                                                                        #indicando que rellene las provincias según los castillos por habitante
  scale_fill_viridis_c(name = "Habitantes\npor castillo",                               #Coloreamos esas provincias con viridis...
                       begin = 0.45, end = 1, option = "magma", trans = "log1p",        #...usando la opción magma, transformando la variable mediante logaritmos para suavizar las transiciones en la escala de colores...
                       breaks = c(0, 100, 1000, 10000)) +                               #...y le decimos que indique sólo una serie de valores en la leyenda
  geom_sf_text(aes(label = paste0("Uno cada\n", round(pob_cast,0), " hab."),            #Añadimos el texto informativo para cada provincia con geom_sf_text
                   size = ifelse(NAME_1 == "País Vasco", 2, 3)),                        #El tamaño de letra lo ponemos más pequeño en el País Vasco porque si no se solapan
               color = "black", family = "Franklin Gothic Demi") + 
  scale_size(range = c(2, 3), guide = "none") +                                         #Importante: con scale_size controlamos los tamaños de letra que nos sacará geom_sf_text
                                                                                        #Le indicamos un rango de 2 a 3 (tamaño 2 para provincias de la CAPV, tamaño 3 para el resto)
  theme_fivethirtyeight(base_size = 14) +                                               #Usamos el theme de FiveThirtyEight disponible en ggthemes
  scale_x_continuous(breaks = seq(-10, 4, by = 2), labels = rep("", length(seq(-10, 4, by = 2)))) +   #Con estas dos líneas quitamos las etiquetas de los ejes (indican coordenadas)
  scale_y_continuous(breaks = seq(34, 44, by = 2), labels = rep("", length(seq(34, 44, by = 2)))) +   #De normal no aportan mucho pero si estamos moviendo las Canarias, todavía menos
  theme(text = element_text(family = "Liberation Sans"),                                #Le decimos que todo el texto del gráfico sea con la tipografía de Liberation Sans
        legend.position = c(0.07, 0.5),                                                 #Importante: fijamos la posición de la leyenda con un vector para meterla DENTRO del gráfico
                                                                                        #Así aprovechamos el espacio que ocuparía Portugal (y que no aparece)
        legend.direction = "vertical") +                                                #Ponemos la leyenda en vertical también
  geom_segment(aes(x = -1, xend = -1, y = 34, yend = 36.5)) +                           #Estos dos geom_segment son para poner las líneas que separan Canarias del resto del mapa
  geom_segment(aes(x = -1, xend = 5, y = 36.5, yend = 36.5)) +
  annotate("text", x = 3, y = 38, label = "Twitter: @Picanumeros", size = 6, col = "grey50") +  #Marca de agua :-)
  labs(title = "Número de habitantes actuales por castillo en cada provincia",          #Títulos del gráfico...
       subtitle = "Fuentes: Wikipedia, gadm.org. Número de castillos por provincia obtenido de la publicación de @Heliosmaps\n(https://www.instagram.com/p/CL2EF1PHUqQ/)") +
  ggsave("castillos1.png", dpi = 300, height = 10, width = 10.25)                       #...y a guardar

#### GRÁFICO DE CASTILLOS POR KM2 ####

#El proceso es prácticamente el mismo al del gráfico anterior. Voy comentando sólo las líneas en las que hay cambios

dat2 %>% left_join(cast %>% mutate(prov = str_trim(prov)), c("NAME_2" = "prov")) %>%
  ggplot() + geom_sf(aes(fill = ifelse(NAME_2 %in% c("Ceuta", "Melilla"), NA, cast_sup)), col = "grey50") + #Le damos NAs a Ceuta y Melilla para no colorearlas
                                                                                                            #De lo contrario, tooooda la península luce con un mismo color porque
                                                                                                            #Ceuta y Melilla son dos outliers taaan gordos que no basta con transformar
  scale_fill_viridis_c(name = "Castillos\npor km2",
                       begin = 1, end = 0.45, option = "magma", 
                       #trans = "log1p",
                       trans = scales::pseudo_log_trans(sigma = 0.01)) +              #Aquí se puede ver que, para suavizar la escala de colores, uso pseudo_log_trans que hace una transformación más severa
  geom_sf_text(aes(label = paste0("Uno cada\n", round(1/cast_sup,1), " km2"),         #Ojo: en fill pongo cast_sup (Castillos/Superficie) pero en el texto informativo de cada provincia
                                                                                      #lo cambio a Superficie/Castillos para que salga un número más fácil de interpretar
                   size = ifelse(NAME_1 == "País Vasco", 2, 3)), 
               color = "black", family = "Franklin Gothic Demi") + 
  scale_size(range = c(2, 3), guide = "none") +
  theme_fivethirtyeight(base_size = 14) +
  scale_x_continuous(breaks = seq(-10, 4, by = 2), labels = rep("", length(seq(-10, 4, by = 2)))) +
  scale_y_continuous(breaks = seq(34, 44, by = 2), labels = rep("", length(seq(34, 44, by = 2)))) +
  theme(text = element_text(family = "Liberation Sans"), 
        legend.position = c(0.07, 0.5),
        legend.background = element_rect(fill = alpha("#F0F0ff", 0.01)),
        legend.direction = "vertical") +
  geom_segment(aes(x = -1, xend = -1, y = 34, yend = 36.5)) +
  geom_segment(aes(x = -1, xend = 5, y = 36.5, yend = 36.5)) +
  annotate("text", x = 3, y = 38, label = "Twitter: @Picanumeros", size = 6, col = "grey50") +
  labs(title = "Número de castillos por kilómetro cuadrado en cada provincia",
       subtitle = "Fuentes: Wikipedia, gadm.org. Número de castillos por provincia obtenido de la publicación de @Heliosmaps\n(https://www.instagram.com/p/CL2EF1PHUqQ/)") +
  ggsave("castillos2.png", dpi = 300, height = 10, width = 10.25)
