library(tidyverse)
library(tm)
library(tidytext)
library(extrafont)
library(ggtext)

#### CÓDIGO UTILIZADO EN PYTHON PARA DESCARGAR LOS TWEETS ####

# import twint
# 
# c = twint.Config()
# c.Search = "wordle '/6'"
# PARA EL WORDLE EN CATALÁN:
# c.Search = "#WordleCAT"
# c.Store_csv = True
# c.Output = "wordle.csv"
# 
# twint.run.Search(c)

#### CÓDIGO DE R A PARTIR DE AQUÍ ####

preprocesamiento <- function(x) removePunctuation(removeNumbers(tolower(x)))
quitaracentos <- function(x){
  x <- str_replace_all(x, "á", "a")
  x <- str_replace_all(x, "é", "e")
  x <- str_replace_all(x, "í", "i")
  x <- str_replace_all(x, "ó", "o")
  x <- str_replace_all(x, "ú", "u")
}

#U+0001F7E9: verde
#U+2B1C: blanco
#U+0001F7E8: amarillo

tablero <- function(x){
  x1 <- str_split(as.character(x), " ")
  x_fin <- 0
  i <- 1
  while(x_fin == 0 & i <= length(x1[[1]])){
    x2 <- stringi::stri_unescape_unicode(str_sub(x1[[1]][i], 1, 2))
    if(is.na(x2) == F){
      if((str_detect(x2, "\\u2b1c") == TRUE) | (str_detect(x2, "\\U0001f7e8") == TRUE) | 
         (str_detect(x2, "\\U0001f7e9") == TRUE)){
        x3 <- stringi::stri_unescape_unicode(x1[[1]][i:(i+5)])
        x3 <- str_replace_all(x3, "\\u2b1c", "blanco ")
        x3 <- str_replace_all(x3, "\\U0001f7e9", "verde ")
        x3 <- str_replace_all(x3, "\\U0001f7e8", "amarillo ")
        x3 <- do.call(rbind.data.frame, str_split(str_trim(x3), " "))
        colnames(x3) <- c(paste0("v",1:ncol(x3)))
        x_fin <- 1
      }
    }
    i <- i+1
  }
  if(x_fin == 0) x3 <- NA
  return(x3)
}

#Hasta las 11:45 del 9 de enero
dat <- rbind(
  read.csv("wordle1.csv", encoding = "UTF-8"),
  read.csv("wordle2.csv", encoding = "UTF-8"),
  read.csv("wordle3.csv", encoding = "UTF-8")
)

dat <- dat[-which(duplicated(dat$id)),]

dat_esp <- dat[which(str_detect(dat$tweet, "ordle \\(ES\\)")),]
dat_eng <- dat[-which(str_detect(dat$tweet, "ordle \\(ES\\)")),]

dat_cat <- read.csv("wordle_cat.csv", encoding = "UTF-8")

dat_cat <- dat_cat[which(str_detect(dat_cat$tweet, "/6")),]



d <- tolower(gsub('\\p{So}|\\p{Cn}','', 
                  as.character(dat_esp$tweet[which(str_detect(dat_esp$tweet, "/6"))]), perl = T))
d <- quitaracentos(d)

loquehay <- sapply(1:length(d), function(i) str_locate_all(d[i], "/6")[[1]][1,])[1,]
caracter_detras_esp <- sapply(1:length(d), 
                              function(i) str_sub(d[i], loquehay[i]-1, loquehay[i]-1))

d <- tolower(gsub('\\p{So}|\\p{Cn}','', 
                  as.character(dat_eng$tweet[which(str_detect(dat_eng$tweet, "/6"))]), perl = T))
d <- quitaracentos(d)

loquehay <- sapply(1:length(d), function(i) str_locate_all(d[i], "/6")[[1]][1,])[1,]
caracter_detras_eng <- sapply(1:length(d), 
                          function(i) str_sub(d[i], loquehay[i]-1, loquehay[i]-1))

d <- tolower(gsub('\\p{So}|\\p{Cn}','', 
                  as.character(dat_cat$tweet[which(str_detect(dat_cat$tweet, "/6"))]), perl = T))
d <- quitaracentos(d)

loquehay <- sapply(1:length(d), function(i) str_locate_all(d[i], "/6")[[1]][1,])[1,]
caracter_detras_cat <- sapply(1:length(d), 
                              function(i) str_sub(d[i], loquehay[i]-1, loquehay[i]-1))

caracter_detras_esp %>% as_tibble() %>% filter(value %in% 1:6) %>%
  group_by(value) %>% summarise(count = n()) %>%
  ggplot(aes(x = value, y = 100*count/sum(count))) + geom_col()

ragg::agg_png("comp_en_es.png", res = 300, width = 1241*3, height = 680*3)
caracter_detras_eng %>% as_tibble() %>%
  mutate(len = dat_eng$language[which(str_detect(dat_eng$tweet, "/6"))]) %>%
  mutate(len = ifelse(len == "en", "en",
                      ifelse(len == "es", "es", "no_en"))) %>%
  filter(value %in% c(1:6, "x")) %>%
  #mutate(value = ifelse(value == "x", 7, value)) %>% mutate(value = as.numeric(as.character(value))) %>%
  group_by(len, value) %>% summarise(count = n()) %>% ungroup() %>%
  group_by(len) %>% mutate(count = 100*count/sum(count)) %>%
  ggplot() + 
  geom_col(data = . %>% filter(len == "en"), mapping = aes(x = value, y = count), alpha = .5) +
  geom_text(data = . %>% filter(len == "en"), 
            mapping = aes(x = value, y = count, label = paste0(round(count, 1), "%")),
            vjust = c(-1.5, rep(-0.5,2), -0.25, rep(-0.5,3)), size = 5.5, family = "Roboto Condensed") + 
  geom_col(data = . %>% filter(len == "es"), mapping = aes(x = value, y = count), alpha = .5, 
           fill = "red", width = 0.6) +
  theme_minimal(base_size = 20) +
  geom_text(data = . %>% filter(len == "es"), 
            mapping = aes(x = value, y = count, label = paste0(round(count, 1), "%")),
            colour = "red", vjust = -0.5, size = 5.5, family = "Roboto Condensed") +
  labs(x = "Turno en el que consiguen terminar el Wordle",
       y = "Porcentaje de personas que terminan\nel Wordle en ese turno",
       title = "Porcentaje de gente que termina el Wordle **en inglés** según turno en el que termina<p>
       y según el idioma del tweet que han publicado (en **gris**, inglés;
       en **<span style='color: red;'>rojo</span>**, español)",
       subtitle = "Tweets recopilados del 31/12/2021 al 9/1/2022 (11:45 hora española peninsular) con la librería 'twint'\nde Python. Sólo tweets que mencionasen 'wordle' y '/6' (n = 382.921)") +
  theme(plot.title = element_markdown(),
        text = element_text(family = "Roboto Condensed"),
        panel.grid = element_line(colour = "grey85"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 24),
        plot.background = element_rect(fill = "#F0F0F0ff", colour = "#F0F0F0ff")) +
  ggplot2::annotate("text", x = 2, y = 30, label = "Media para tweets en inglés\n(codificando X como 7):\n4.21",
                    size = 7, family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 6.5, y = 30, label = "Media para tweets en español\n(codificando X como 7):\n4.64",
           col = "red", size = 7, family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 1.5, y = 13.75, label = "Twitter/Instagram:\n@Picanumeros",
                    size = 5, family = "Roboto Condensed", col = "grey65")
invisible(dev.off())

ragg::agg_png("comp_es_ES.png", res = 300, width = 1241*3, height = 680*3)
caracter_detras_eng %>% as_tibble() %>%
  mutate(len = dat_eng$language[which(str_detect(dat_eng$tweet, "/6"))]) %>%
  mutate(len = ifelse(len == "en", "en",
                      ifelse(len == "es", "es", "no_en"))) %>%
  filter(value %in% c(1:6, "x")) %>%
  #mutate(value = ifelse(value == "x", 7, value)) %>% mutate(value = as.numeric(as.character(value))) %>%
  group_by(len, value) %>% summarise(count = n()) %>% ungroup() %>%
  group_by(len) %>% mutate(count = 100*count/sum(count)) %>%
  ggplot() + 
  geom_col(data = . %>% filter(len == "es"), mapping = aes(x = value, y = count), alpha = .5) +
  geom_text(data = . %>% filter(len == "es"), 
            mapping = aes(x = value, y = count, label = paste0(round(count, 1), "%")),
            vjust = -0.25, size = 6, family = "Roboto Condensed") + 
  geom_col(data = caracter_detras_esp %>% as_tibble() %>% filter(value %in% 1:6) %>%
             group_by(value) %>% summarise(count = n()) %>% mutate(count = 100*count/sum(count)), 
           mapping = aes(x = value, y = count), alpha = .5, 
           fill = "red", width = 0.6) +
  theme_minimal(base_size = 19) +
  geom_text(data = caracter_detras_esp %>% as_tibble() %>% filter(value %in% 1:6) %>%
              group_by(value) %>% summarise(count = n()) %>% mutate(count = 100*count/sum(count)), 
            mapping = aes(x = value, y = count, label = paste0(round(count, 1), "%")),
            colour = "red", vjust = c(rep(-0.5,3), -1, rep(-0.5, 2)), size = 6, family = "Roboto Condensed") +
  labs(x = "Turno en el que consiguen terminar el Wordle",
       y = "Porcentaje de personas que terminan\nel Wordle en ese turno",
       title = "Porcentaje de personas según el idioma del Wordle y según el turno en el que lo<p>termina
       (en **gris**, personas que resuelven el Wordle en inglés y **que tuitean en español**;
       <p>en **<span style='color: red;'>rojo</span>**, personas que resuelven el Wordle en español de wordle.danielfrg.com)",
       subtitle = "Tweets recopilados del 31/12/2021 al 9/1/2022 (11:45 hora española peninsular) con la librería 'twint'<p>de Python. Sólo tweets que mencionasen 'wordle' y '/6' (**n** inglés = 2.410; **<span style='color: red;'>n</span>** español = 6.916)"
       ) +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        text = element_text(family = "Roboto Condensed"),
        panel.grid = element_line(colour = "grey85"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 24),
        plot.background = element_rect(fill = "#F0F0F0ff", colour = "#F0F0F0ff")) +
  ggplot2::annotate("text", x = 1.5, y = 25, label = "Media para tweets\ndel Wordle en inglés\n(codificando X como 7):\n4.64",
                    size = 7, family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 6.5, y = 27, label = "Media para tweets\ndel Wordle en español:\n3.88",
                    col = "red", size = 7, family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 1, y = 13.75, label = "Twitter/Instagram:\n@Picanumeros",
                    size = 5, family = "Roboto Condensed", col = "grey65") +
  scale_y_continuous(limits = c(0, 32))
invisible(dev.off())

ragg::agg_png("comp_eng_ES.png", res = 300, width = 1241*3, height = 680*3)
caracter_detras_eng %>% as_tibble() %>%
  mutate(len = dat_eng$language[which(str_detect(dat_eng$tweet, "/6"))]) %>%
  mutate(len = ifelse(len == "en", "en",
                      ifelse(len == "es", "es", "no_en"))) %>%
  filter(value %in% c(1:6, "x")) %>%
  #mutate(value = ifelse(value == "x", 7, value)) %>% mutate(value = as.numeric(as.character(value))) %>%
  group_by(len, value) %>% summarise(count = n()) %>% ungroup() %>%
  group_by(len) %>% mutate(count = 100*count/sum(count)) %>%
  ggplot() + 
  geom_col(data = . %>% filter(len == "en"), mapping = aes(x = value, y = count), alpha = .5) +
  geom_text(data = . %>% filter(len == "en"), 
            mapping = aes(x = value, y = count, label = paste0(round(count, 1), "%")),
            vjust = -0.25, size = 6, family = "Roboto Condensed") + 
  geom_col(data = caracter_detras_esp %>% as_tibble() %>% filter(value %in% 1:6) %>%
             group_by(value) %>% summarise(count = n()) %>% mutate(count = 100*count/sum(count)), 
           mapping = aes(x = value, y = count), alpha = .5, 
           fill = "red", width = 0.6) +
  theme_minimal(base_size = 19) +
  geom_text(data = caracter_detras_esp %>% as_tibble() %>% filter(value %in% 1:6) %>%
              group_by(value) %>% summarise(count = n()) %>% mutate(count = 100*count/sum(count)), 
            mapping = aes(x = value, y = count, label = paste0(round(count, 1), "%")),
            colour = "red", vjust = c(rep(-0.5,5), -2), size = 6, family = "Roboto Condensed") +
  labs(x = "Turno en el que consiguen terminar el Wordle",
       y = "Porcentaje de personas que terminan\nel Wordle en ese turno",
       title = "Porcentaje de personas según el idioma del Wordle y según el turno en el que lo<p>termina
       (en **gris**, personas que resuelven el Wordle en inglés y **que tuitean en inglés**;
       <p>en **<span style='color: red;'>rojo</span>**, personas que resuelven el Wordle en español de wordle.danielfrg.com)",
       subtitle = "Tweets recopilados del 31/12/2021 al 9/1/2022 (11:45 hora española peninsular) con la librería 'twint'<p>de Python. Sólo tweets que mencionasen 'wordle' y '/6' (**n** inglés = 362.116; **<span style='color: red;'>n</span>** español = 4.975)"
  ) +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        text = element_text(family = "Roboto Condensed"),
        panel.grid = element_line(colour = "grey85"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 24),
        plot.background = element_rect(fill = "#F0F0F0ff", colour = "#F0F0F0ff")) +
  ggplot2::annotate("text", x = 1.5, y = 25, label = "Media para tweets\ndel Wordle en inglés\n(codificando X como 7):\n4.21",
                    size = 7, family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 6.5, y = 27, label = "Media para tweets\ndel Wordle en español:\n3.88",
                    col = "red", size = 7, family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 1, y = 13.75, label = "Twitter/Instagram:\n@Picanumeros",
                    size = 5, family = "Roboto Condensed", col = "grey65") +
  scale_y_continuous(limits = c(0, 35))
invisible(dev.off())

ragg::agg_png("comp_CAT_ES.png", res = 300, width = 1241*3, height = 680*3)
caracter_detras_esp %>% as_tibble() %>% filter(value %in% 1:6) %>%
  group_by(value) %>% summarise(count = n()) %>% mutate(count = 100*count/sum(count)) %>%
  ggplot() + 
  geom_col(mapping = aes(x = value, y = count), alpha = .5, fill = "red") +
  geom_text(mapping = aes(x = value, y = count, label = paste0(round(count, 1), "%")),
            vjust = c(-1, -0.5, -1, -0.5, -0.5, -1.5),  size = 6, family = "Roboto Condensed", col = "red") + 
  geom_col(data = caracter_detras_cat %>% as_tibble() %>% filter(value %in% c(1:6,"x")) %>%
             group_by(value) %>% summarise(count = n()) %>% mutate(count = 100*count/sum(count)), 
           mapping = aes(x = value, y = count), alpha = .5, 
           fill = "blue", width = 0.6) +
  theme_minimal(base_size = 19) +
  geom_text(data = caracter_detras_cat %>% as_tibble() %>% filter(value %in% c(1:6,"x")) %>%
              group_by(value) %>% summarise(count = n()) %>% mutate(count = 100*count/sum(count)), 
            mapping = aes(x = value, y = count, label = paste0(round(count, 1), "%")),
            colour = "blue", vjust = -0.5, size = 6, family = "Roboto Condensed") +
  labs(x = "Turno en el que consiguen terminar el Wordle",
       y = "Porcentaje de personas que terminan\nel Wordle en ese turno",
       title = "Porcentaje de personas según el idioma del Wordle y según el turno en el que lo<p>termina
       (en **<span style='color: red;'>rojo</span>**, personas que resuelven el Wordle en español de wordle.danielfrg.com,
       <p>en **<span style='color: blue;'>azul</span>**, personas que resuelven el Wordle en catalán de gelozp.com/games/wordle/)",
       subtitle = "Tweets recopilados del 31/12/2021 al 9/1/2022 (11:45 hora española peninsular) con la librería 'twint'<p>de Python. Sólo tweets que mencionasen 'wordle' y '/6' (**<span style='color: blue;'>n</span>** catalán = 1.329; **<span style='color: red;'>n</span>** español = 4.975)"
  ) +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        text = element_text(family = "Roboto Condensed"),
        panel.grid = element_line(colour = "grey85"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 24),
        plot.background = element_rect(fill = "#F0F0F0ff", colour = "#F0F0F0ff")) +
  ggplot2::annotate("text", x = 1.5, y = 25, label = "Media para tweets\ndel Wordle en catalán\n(codificando X como 7):\n4.08",
                    col = "blue", size = 7, family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 6.5, y = 27, label = "Media para tweets\ndel Wordle en español:\n3.88",
                    col = "red", size = 7, family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 1, y = 13.75, label = "Twitter/Instagram:\n@Picanumeros",
                    size = 5, family = "Roboto Condensed", col = "grey65") +
  scale_y_continuous(limits = c(0, 35))
invisible(dev.off())

tableros_esp <- lapply(dat_esp$tweet[which(str_detect(dat_esp$tweet, "/6"))], tablero)

aeliminar1 <- which(caracter_detras_esp %in% 1:6 == F)

tableros_esp <- tableros_esp[-aeliminar1]

nuevo_vector1 <- caracter_detras_esp[-aeliminar1]

aeliminar2 <- which(sapply(1:length(tableros_esp), 
                          function(i) length(dim(tableros_esp[[i]]))) == 0)

tableros_esp <- tableros_esp[-aeliminar2]

nuevo_vector2 <- nuevo_vector1[-aeliminar2]

for(i in 1:length(tableros_esp)){
  tableros_esp[[i]] <- tableros_esp[[i]][1:as.numeric(as.character(nuevo_vector2[i])),]
  tableros_esp[[i]][,"id"] <- i
  tableros_esp[[i]][,"fila"] <- 1:nrow(tableros_esp[[i]])
}

dat_tableros_esp <-
  do.call(rbind.data.frame, 
          tableros_esp[-which(sapply(1:length(tableros_esp), function(i) ncol(tableros_esp[[i]])) != 7)])

ragg::agg_png("tablero_ES.png", res = 300, width = 733*3, height = 1000*3)
dat_tableros_esp %>% pivot_longer(-c("id", "fila"), names_to = "columna", values_to = "color") %>%
  group_by(fila, columna) %>% summarise(blanco = sum(color == "blanco", na.rm = T),
                                        verde = sum(color == "verde", na.rm = T),
                                        amarillo = sum(color == "amarillo", na.rm = T)) %>% ungroup() %>%
  pivot_longer(-c("fila", "columna"), names_to = "colores", values_to = "n") %>%
  mutate(colores = factor(colores, levels = c("blanco", "amarillo", "verde"))) %>%
  filter(fila %in% 1:5) %>%
  group_by(fila, columna) %>% mutate(suma_fila = sum(n), n = n/sum(n)) %>% ungroup() %>%
  group_by(fila) %>%
  mutate(fila = paste0("Intento ", fila, " (personas que han llegado = ", suma_fila[1],")"),
         columna = substr(columna, 2,2)) %>%
  #mutate(total = blanco + verde + amarillo) %>%
  ggplot(aes(x = columna, y = n, fill = colores,
             label = round(100*n, 1))) + geom_col() +
  scale_fill_manual(values = c("grey85", "gold", "darkgreen")) +
  facet_wrap(~fila, ncol = 1) +
  geom_text(position = position_stack(vjust = 0.5), family = "Roboto Condensed", size = 5,
            colour = rep(c("black", "grey85", "black"), 25)) +
  theme_minimal(base_size = 18) +
  labs(title = "Porcentaje de personas que obtienen casilla de cada color<p>en cada columna según el intento **en el Wordle en español**",
       x = "Columna", caption = "Twitter/Instagram: @Picanumeros") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown())
invisible(dev.off())

dat_tableros_esp %>% pivot_longer(-c("id", "fila"), names_to = "columna", values_to = "color") %>%
  group_by(id, fila) %>% summarise(num_verdes = sum(color == "verde"),
                                   num_blancos = sum(color == "blanco")) %>% ungroup() %>% group_by(id) %>%
  filter(n() > 1) %>%
  mutate(verdes_ant = c(NA, num_verdes[1:(n()-1)]),
         blancos_ant = c(NA, num_blancos[1:(n()-1)])) %>% ungroup() %>%
  select(num_verdes, verdes_ant) %>% drop_na() %>% group_by(num_verdes, verdes_ant) %>% summarize(n = n()) %>% 
  filter(verdes_ant != 5) %>% group_by(verdes_ant) %>% mutate(n = n/sum(n)) %>%
  ggplot(aes(x = num_verdes, y = verdes_ant, col = n, label = paste0(round(100*n, 1), "%"))) + 
  geom_point(pch = 15, size = 40) +
  geom_text(size = 10, col = "white", family = "Roboto Condensed") + 
  scale_x_continuous(limits = c(-0.5, 5.5), breaks = 0:5) + 
  scale_y_continuous(limits = c(-0.5, 4.5), breaks = 0:4) + 
  theme_minimal(base_size = 18)+
  scale_color_viridis_c(trans = "log1p") + 
  theme(legend.position = "none",
        text = element_text(family = "Roboto Condensed"),
        axis.text = element_text(size = 30),
        panel.grid = element_blank()) +
  labs(x = "Estado siguiente (número de casillas verdes)", 
       caption = "Twitter/Instagram: @Picanumeros",
       y = "Estado actual (número de casillas verdes)",
       title = "Si en un intento consigues Y casillas verdes, ¿cuál es la probabilidad\nde que al intento siguiente consigas X casillas verdes?\nDatos del Wordle en español de wordle.danielfrg.com") +
  ggsave("matriz_verdes.png", dpi = 300, height = 9.3, width = 9.3)

dat_tableros_esp %>% pivot_longer(-c("id", "fila"), names_to = "columna", values_to = "color") %>%
  group_by(id, fila) %>% summarise(num_verdes = sum(color == "verde"),
                                   num_blancos = sum(color == "blanco")) %>% ungroup() %>% group_by(id) %>%
  filter(n() > 1) %>%
  mutate(verdes_ant = c(NA, num_verdes[1:(n()-1)]),
         blancos_ant = c(NA, num_blancos[1:(n()-1)])) %>% ungroup() %>%
  select(num_blancos, blancos_ant) %>% drop_na() %>% group_by(num_blancos, blancos_ant) %>% summarize(n = n()) %>% 
  group_by(blancos_ant) %>% mutate(n = n/sum(n)) %>%
  ggplot(aes(x = 5-num_blancos, y = 5-blancos_ant, col = n, label = paste0(round(100*n, 1), "%"))) + 
  geom_point(pch = 15, size = 40) +
  geom_text(size = 10, col = "white", family = "Roboto Condensed") + 
  scale_x_continuous(limits = c(-0.5, 5.5), breaks = 0:5) + 
  scale_y_continuous(limits = c(-0.5, 5.5), breaks = 0:5) + 
  theme_minimal(base_size = 18)+
  scale_color_viridis_c(trans = "log1p") + 
  theme(legend.position = "none",
        text = element_text(family = "Roboto Condensed"),
        axis.text = element_text(size = 30),
        panel.grid = element_blank()) +
  labs(x = "Estado siguiente (número de casillas no blancas)", 
       caption = "Twitter/Instagram: @Picanumeros",
       y = "Estado actual (número de casillas no blancas)",
       title = "Si en un intento consigues Y casillas no blancas, ¿cuál es la probabilidad\nde que al intento siguiente consigas X casillas no blancas?\nDatos del Wordle en español de wordle.danielfrg.com") +
  ggsave("matriz_noblancas.png", dpi = 300, height = 10.7, width = 9.7)


id_intentos_eng <- data.frame(id = dat_eng$user_id[which(str_detect(dat_eng$tweet, "/6"))], 
                              turno = caracter_detras_eng) %>%
  filter(dat_eng$language[which(str_detect(dat_eng$tweet, "/6"))] == "en") %>%
  filter(turno %in% c(1:6, "x")) %>%
  group_by(id) %>% mutate(intento = 1:n()) %>% ungroup() %>%
  group_by(id) %>% summarise(max = max(intento)) %>% filter(max == 3) %>% pull(id)

data.frame(id = dat_eng$user_id[which(str_detect(dat_eng$tweet, "/6"))], 
           turno = caracter_detras_eng) %>%
  filter(dat_eng$language[which(str_detect(dat_eng$tweet, "/6"))] == "en") %>%
  filter(turno %in% c(1:6, "x")) %>%
  group_by(id) %>% mutate(intento = 1:n()) %>% ungroup() %>% 
  filter(id %in% id_intentos_eng) %>%
  group_by(turno, intento) %>% summarise(n = n()) %>% ungroup() %>%
  group_by(intento) %>% mutate(n = n/sum(n)) %>%
  ggplot(aes(x = intento, y = n, fill = turno, label = paste0(round(100*n,1), "%"))) + 
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(x = "Intento", y = "Porcentaje de personas (%)",
       title = "Porcentaje de personas que consiguen terminar el Wordle en un cierto turno<p>
       según el número del intento (Wordle **en inglés** y **sólo personas que tuitean en inglés** y **que lo han intentado hasta 2 veces**)") +
  theme_minimal(base_size = 18) +
  scale_fill_viridis_d(name = "Turno al que terminan")+
  theme(plot.title = element_markdown(),
        legend.position = "top")

id_intentos_es <- data.frame(id = dat_esp$user_id[which(str_detect(dat_esp$tweet, "/6"))], 
                             turno = caracter_detras_esp) %>% filter(turno %in% 1:6) %>%
  group_by(id) %>% mutate(intento = 1:n()) %>% ungroup() %>%
  group_by(id) %>% summarise(max = max(intento)) %>% filter(max == 2) %>% pull(id)

ragg::agg_png("2intentos_es.png", res = 300, width = 1200*3, height = 1000*3)
data.frame(id = dat_esp$user_id[which(str_detect(dat_esp$tweet, "/6"))], 
           turno = caracter_detras_esp) %>% filter(turno %in% 1:6) %>%
  group_by(id) %>% mutate(intento = 1:n()) %>% ungroup() %>% 
  filter(id %in% id_intentos_es) %>%
  group_by(turno, intento) %>% summarise(n = n()) %>% ungroup() %>%
  group_by(intento) %>% mutate(n = n/sum(n)) %>%
  ggplot(aes(x = intento, y = n*100, fill = turno, label = paste0(round(100*n,1), "%"))) + 
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5), 
            col = rep(c("white", "black"), c(10, 2)),
            size = 8, family = "Roboto Condensed") +
  labs(x = "Intento", y = "Porcentaje de personas (%)",
       title = "Porcentaje de personas que consiguen terminar el Wordle en un<p>cierto turno
       según el número del intento (Wordle **en español** y **sólo**<p>**personas que lo han intentado hasta 2 veces** -n = 1.123-)") +
  theme_minimal(base_size = 24) +
  scale_x_continuous(breaks = 1:2) +
  scale_fill_viridis_d(name = "Turno al que terminan")+
  theme(plot.title = element_markdown(),
        text = element_text(family = "Roboto Condensed"),
        legend.position = "top",
        plot.background = element_rect(fill = "#F0F0F0ff",
                                       colour = "#F0F0F0ff"),
        axis.text.x = element_text(size = 35))
invisible(dev.off())

ragg::agg_png("3intentos_es.png", res = 300, width = 1200*3, height = 1000*3)
id_intentos_es <- data.frame(id = dat_esp$user_id[which(str_detect(dat_esp$tweet, "/6"))], 
                             turno = caracter_detras_esp) %>% filter(turno %in% 1:6) %>%
  group_by(id) %>% mutate(intento = 1:n()) %>% ungroup() %>%
  group_by(id) %>% summarise(max = max(intento)) %>% filter(max == 3) %>% pull(id)

data.frame(id = dat_esp$user_id[which(str_detect(dat_esp$tweet, "/6"))], 
           turno = caracter_detras_esp) %>% filter(turno %in% 1:6) %>%
  group_by(id) %>% mutate(intento = 1:n()) %>% ungroup() %>% 
  filter(id %in% id_intentos_es) %>%
  group_by(turno, intento) %>% summarise(n = n()) %>% ungroup() %>%
  group_by(intento) %>% mutate(n = n/sum(n)) %>%
  ggplot(aes(x = intento, y = n*100, fill = turno, label = paste0(round(100*n,1), "%"))) + 
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5), 
            col = rep(c("white", "black"), c(15, 3)),
            size = 8, family = "Roboto Condensed") +
  labs(x = "Intento", y = "Porcentaje de personas (%)",
       title = "Porcentaje de personas que consiguen terminar el Wordle en un<p>cierto turno
       según el número del intento (Wordle **en español** y **sólo**<p>**personas que lo han intentado hasta 3 veces** -n = 176-)") +
  theme_minimal(base_size = 24) +
  scale_fill_viridis_d(name = "Turno al que terminan")+
  theme(plot.title = element_markdown(),
        text = element_text(family = "Roboto Condensed"),
        legend.position = "top",
        plot.background = element_rect(fill = "#F0F0F0ff",
                                       colour = "#F0F0F0ff"),
        axis.text.x = element_text(size = 35))
invisible(dev.off())

id_intentos_cat <- data.frame(id = dat_cat$user_id[which(str_detect(dat_cat$tweet, "/6"))], 
                              turno = caracter_detras_cat) %>%
  filter(turno %in% c(1:6, "x")) %>%
  group_by(id) %>% mutate(intento = 1:n()) %>% ungroup() %>%
  group_by(id) %>% summarise(max = max(intento)) %>% filter(max == 2) %>% pull(id)

ragg::agg_png("2intentos_cat.png", res = 300, width = 1200*3, height = 1000*3)
data.frame(id = dat_cat$user_id[which(str_detect(dat_cat$tweet, "/6"))], 
           turno = caracter_detras_cat) %>%
  filter(turno %in% c(1:6, "x")) %>%
  group_by(id) %>% mutate(intento = 1:n()) %>% ungroup() %>% 
  filter(id %in% id_intentos_cat) %>%
  group_by(turno, intento) %>% summarise(n = n()) %>% ungroup() %>%
  group_by(intento) %>% mutate(n = n/sum(n)) %>%
  ggplot(aes(x = intento, y = n*100, fill = turno, label = paste0(round(100*n,1), "%"))) + 
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5), 
            #col = rep(c("white", "black"), c(10, 2)),
            size = 8, family = "Roboto Condensed") +
  labs(x = "Intento", y = "Porcentaje de personas (%)",
       title = "Porcentaje de personas que consiguen terminar el Wordle en un<p>cierto turno
       según el número del intento (Wordle **en catalán** y **sólo**<p>**personas que lo han intentado hasta 2 veces** -n = 157-)") +
  theme_minimal(base_size = 24) +
  scale_x_continuous(breaks = 1:2) +
  scale_fill_viridis_d(name = "Turno al que terminan")+
  theme(plot.title = element_markdown(),
        text = element_text(family = "Roboto Condensed"),
        legend.position = "top",
        plot.background = element_rect(fill = "#F0F0F0ff",
                                       colour = "#F0F0F0ff"),
        axis.text.x = element_text(size = 35))
invisible(dev.off())
