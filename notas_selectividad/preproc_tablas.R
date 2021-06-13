library(tidyverse)

# En todos los casos, el preprocesamiento es igual:
#   1)  Se descarga el .csv de la web del SIIU
#   2)  Se eliminan las filas vacías
#   3)  Se cambian los valores de la primera columna indicando los niveles que se representan en cada fila, separados por un _
#   4)  Se cambian los nombres de las columnas indicando los niveles que se representan en cada una de ellas, separados por un _
#   5)  Se pasa la tabla a formato largo con pivot_longer, de forma que pasemos a una tabla con varias columnas indicando los niveles, y una columna indicando el valor
#       para una combinación concreta de niveles.

#### RENDIMIENTO (MATRICULADOS, PRESENTADOS, APROBADOS) POR CCAA ####

# Descripción de los datasets de formato largo que se obtendrán en esta sección:
#   - rendim = PAU Genéricas. Matriculados, presentados y aprobados por tipo de matrícula, tipo de estudio de acceso (solo titulados de bachiller y FP), tipo de convocatoria y naturaleza del centro de impartición 

rendim_ccaa <- read.csv("http://estadisticas.mecd.gob.es/EducaJaxiPx/files/_px/es/csv_sc/Universitaria/PAU/PAU20/l0/PAU0206.px?nocab=1",
                        sep = ";", na.strings = "..", #encoding = "UTF-8",
                        skip = 8, nrows = 323)

rendim_ccaa <- rendim_ccaa[-which(is.na(rendim_ccaa[,2])),]
rendim_ccaa[,1] <- paste0(rep(c("Total",
                                "Andalucía",
                                "Aragón",
                                "Asturias (Principado de)",
                                "Balears (Illes)",
                                "Canarias",
                                "Cantabria",
                                "Castilla - La Mancha",
                                "Castilla y León",
                                "Catalunya",
                                "Comunitat Valenciana",
                                "Estado",
                                "Extremadura",
                                "Galicia",
                                "Madrid (Comunidad de)",
                                "Murcia (Región de)",
                                "Navarra (Comunidad Foral de)",
                                "País Vasco",
                                "Rioja (La)"), each = 12), "_", 
                          rep(paste(rep(c("Total", "GenEsp", "Gen", "Esp"), each = 3), 
                        "_", rep(c("Total", "Bach", "FP"), 4), 
                        sep = ""), 19))
colnames(rendim_ccaa) <- c("X", paste(rep(c("Total", "Ord", "Extra"), each = 90), "_",
                                      rep(rep(c("Total", "Pub", "Pri", "Conc", "NoConsta"), each = 18), 3), "_",
                                      rep(rep(c("Matric", "Present", "Aprob"), each = 6), 15),
                                     "_", rep(2015:2020, 45), 
                                     sep = ""), "X.1")
rendim_ccaa <- rendim_ccaa[,-ncol(rendim_ccaa)]

rendim <- rendim_ccaa %>% pivot_longer(-X, names_to = "dato", values_to = "cifra")
rendim <- data.frame(rendim, do.call(rbind.data.frame, str_split(rendim$X, "_")),
                     do.call(rbind.data.frame, str_split(rendim$dato, "_")))
colnames(rendim) <- c("X", "dato", "cifra", "CCAA", "fase", "procedencia", "convoc", "centro", "metrica", "anio")

#### NOTAS BACHILLERATO ####

# Descripción de los datasets de formato largo que se obtendrán en esta sección:
#   - distr = Distribución de las notas en el estudio de bachiller por tipo de matrícula, tipo de convocatoria y naturaleza del centro de impartición 
#   - nota = Nota media en el estudio de bachiller por tipo de matricula, tipo de convocatoria y naturaleza del centro de impartición 

distr_bach <- read.csv("http://estadisticas.mecd.gob.es/EducaJaxiPx/files/_px/es/csv_sc/Universitaria/PAU/PAU20/l0/PAU0217.px?nocab=1",
                       sep = ";", encoding = "UTF-8", na.strings = "..",
                       skip = 8, nrows = 76)

distr_bach <- distr_bach[-which(is.na(distr_bach[,2])),]
distr_bach[,1] <- paste0(rep(c("Total",
                                "Andalucía",
                                "Aragón",
                                "Asturias (Principado de)",
                                "Balears (Illes)",
                                "Canarias",
                                "Cantabria",
                                "Castilla - La Mancha",
                                "Castilla y León",
                                "Catalunya",
                                "Comunitat Valenciana",
                                "Extremadura",
                                "Galicia",
                                "Madrid (Comunidad de)",
                                "Murcia (Región de)",
                                "Navarra (Comunidad Foral de)",
                                "País Vasco",
                                "Rioja (La)"), each = 3), "_", rep(c("Total", "Ord", "Extraord"), 18), 
                                    sep = "")
colnames(distr_bach) <- c("X", paste(rep(c("Total", "GenEsp", "Esp", "Gen"), each = 150), "_",
                                      rep(rep(c("Total", "Pub", "Pri", "Conc", "NoConsta"), each = 30), 4), "_",
                                      rep(rep(c("De 5 a 6", "De 6 a 7", "De 7 a 8", "De 8 a 9", "De 9 a 10"), each = 6), 20),
                                      "_", rep(2015:2020, 100), 
                                      sep = ""), "X.1")
distr_bach <- distr_bach[,-ncol(distr_bach)]
distr <- distr_bach %>% pivot_longer(-X, names_to = "dato", values_to = "cifra")
distr <- data.frame(distr, do.call(rbind.data.frame, str_split(distr$X, "_")),
                     do.call(rbind.data.frame, str_split(distr$dato, "_")))
colnames(distr) <- c("X", "dato", "cifra", "CCAA", "convoc", "fase", "centro", "nota", "anio")

nota_bach <- read.csv("http://estadisticas.mecd.gob.es/EducaJaxiPx/files/_px/es/csv_sc/Universitaria/PAU/PAU20/l0/PAU0215.px?nocab=1",
                       sep = ";", na.strings = "..",
                       skip = 7, nrows = 95)
nota_bach <- nota_bach[-which(is.na(nota_bach[,2])),]
nota_bach[,1] <- paste0(rep(c("Total",
                               "Andalucía",
                               "Aragón",
                               "Asturias (Principado de)",
                               "Balears (Illes)",
                               "Canarias",
                               "Cantabria",
                               "Castilla - La Mancha",
                               "Castilla y León",
                               "Catalunya",
                               "Comunitat Valenciana",
                               "Extremadura",
                               "Galicia",
                               "Madrid (Comunidad de)",
                               "Murcia (Región de)",
                               "Navarra (Comunidad Foral de)",
                               "País Vasco",
                               "Rioja (La)"), each = 4), "_", rep(c("Total", "GenEsp", "Esp", "Gen"), 18), 
                         sep = "")
colnames(nota_bach) <- c("X", paste(rep(c("Total", "Ord", "Extraord"), each = 30), "_",
                                     rep(rep(c("Total", "Pub", "Pri", "Conc", "NoConsta"), each = 6), 3),
                                     "_", rep(2015:2020, 15), 
                                     sep = ""), "X.1")
nota_bach <- nota_bach[,-ncol(nota_bach)]
nota <- nota_bach %>% pivot_longer(-X, names_to = "dato", values_to = "cifra")
nota <- data.frame(nota, do.call(rbind.data.frame, str_split(nota$X, "_")),
                    do.call(rbind.data.frame, str_split(nota$dato, "_")))
colnames(nota) <- c("X", "dato", "cifra", "CCAA", "fase", "convoc", "centro", "anio")

#### NOTAS FASE GENERAL ####

# Descripción de los datasets de formato largo que se obtendrán en esta sección:
#   - distr2 = Distribución de la nota en la fase general de los titulados de bachiller por tipo de matrícula, tipo de convocatoria y naturaleza del centro de impartición 
#   - nota2 = Nota media de los aptos en la fase general de los titulados de bachiller por tipo de matricula, tipo de convocatoria y naturaleza del centro de impartición 

distr_fase <- read.csv("http://estadisticas.mecd.gob.es/EducaJaxiPx/files/_px/es/csv_sc/Universitaria/PAU/PAU20/l0/PAU0221.px?nocab=1",
                       sep = ";", encoding = "UTF-8", na.strings = "..",
                       skip = 8, nrows = 76)

distr_fase <- distr_fase[-which(is.na(distr_fase[,2])),]
distr_fase[,1] <- paste0(rep(c("Total",
                               "Andalucía",
                               "Aragón",
                               "Asturias (Principado de)",
                               "Balears (Illes)",
                               "Canarias",
                               "Cantabria",
                               "Castilla - La Mancha",
                               "Castilla y León",
                               "Catalunya",
                               "Comunitat Valenciana",
                               "Extremadura",
                               "Galicia",
                               "Madrid (Comunidad de)",
                               "Murcia (Región de)",
                               "Navarra (Comunidad Foral de)",
                               "País Vasco",
                               "Rioja (La)"), each = 3), "_", rep(c("Total", "GenEsp", "Gen"), 18), 
                         sep = "")
colnames(distr_fase) <- c("X", paste(rep(c("Total", "Ord", "Extraord"), each = 180), "_",
                                     rep(rep(c("Total", "Pub", "Pri", "Conc", "NoConsta"), each = 36), 3), "_",
                                     rep(rep(c("De 4 a 5", "De 5 a 6", "De 6 a 7", "De 7 a 8", "De 8 a 9", "De 9 a 10"), each = 6), 15),
                                     "_", rep(2015:2020, 90), 
                                     sep = ""), "X.1")
distr_fase <- distr_fase[,-ncol(distr_fase)]
distr2 <- distr_fase %>% pivot_longer(-X, names_to = "dato", values_to = "cifra")
distr2 <- data.frame(distr2, do.call(rbind.data.frame, str_split(distr2$X, "_")),
                    do.call(rbind.data.frame, str_split(distr2$dato, "_")))
colnames(distr2) <- c("X", "dato", "cifra", "CCAA", "fase", "convoc", "centro", "nota", "anio")

nota_fase <- read.csv("http://estadisticas.mecd.gob.es/EducaJaxiPx/files/_px/es/csv_sc/Universitaria/PAU/PAU20/l0/PAU0219.px?nocab=1",
                      sep = ";", na.strings = "..",
                      skip = 7, nrows = 76)
nota_fase <- nota_fase[-which(is.na(nota_fase[,2])),]
nota_fase[,1] <- paste0(rep(c("Total",
                               "Andalucía",
                               "Aragón",
                               "Asturias (Principado de)",
                               "Balears (Illes)",
                               "Canarias",
                               "Cantabria",
                               "Castilla - La Mancha",
                               "Castilla y León",
                               "Catalunya",
                               "Comunitat Valenciana",
                               "Extremadura",
                               "Galicia",
                               "Madrid (Comunidad de)",
                               "Murcia (Región de)",
                               "Navarra (Comunidad Foral de)",
                               "País Vasco",
                               "Rioja (La)"), each = 3), "_", rep(c("Total", "GenEsp", "Gen"), 18), 
                         sep = "")
colnames(nota_fase) <- c("X", paste(rep(c("Total", "Ord", "Extraord"), each = 30), "_",
                                     rep(rep(c("Total", "Pub", "Pri", "Conc", "NoConsta"), each = 6), 3),
                                     "_", rep(2015:2020, 15), 
                                     sep = ""), "X.1")
nota_fase <- nota_fase[,-ncol(nota_fase)]
nota2 <- nota_fase %>% pivot_longer(-X, names_to = "dato", values_to = "cifra")
nota2 <- data.frame(nota2, do.call(rbind.data.frame, str_split(nota2$X, "_")),
                     do.call(rbind.data.frame, str_split(nota2$dato, "_")))
colnames(nota2) <- c("X", "dato", "cifra", "CCAA", "fase", "convoc", "centro", "anio")

#### NOTAS ACCESO UNI ####

# Descripción de los datasets de formato largo que se obtendrán en esta sección:
#   - distr3 = Distribución de la nota de acceso a grado de los titulados de bachiller por tipo de matricula, tipo de convocatoria y naturaleza del centro de impartición 
#   - nota3 = Nota media de acceso a grado de los titulados de bachiller por tipo de matricula, tipo de convocatoria y naturaleza del centro de impartición  

distr_uni <- read.csv("http://estadisticas.mecd.gob.es/EducaJaxiPx/files/_px/es/csv_sc/Universitaria/PAU/PAU20/l0/PAU0225.px?nocab=1",
                       sep = ";", encoding = "UTF-8", na.strings = "..",
                       skip = 8, nrows = 76)

distr_uni <- distr_uni[-which(is.na(distr_uni[,2])),]
distr_uni[,1] <- paste0(rep(c("Total",
                              "Andalucía",
                              "Aragón",
                              "Asturias (Principado de)",
                              "Balears (Illes)",
                              "Canarias",
                              "Cantabria",
                              "Castilla - La Mancha",
                              "Castilla y León",
                              "Catalunya",
                              "Comunitat Valenciana",
                              "Extremadura",
                              "Galicia",
                              "Madrid (Comunidad de)",
                              "Murcia (Región de)",
                              "Navarra (Comunidad Foral de)",
                              "País Vasco",
                              "Rioja (La)"), each = 3), "_", rep(c("Total", "Ord", "Extraord"), 18), 
                        sep = "")
colnames(distr_uni) <- c("X", paste(rep(c("Total", "GenEsp", "Gen"), each = 150), "_",
                                    rep(rep(c("Total", "Pub", "Pri", "Conc", "NoConsta"), each = 30), 3), "_",
                                    rep(rep(c("De 5 a 6", "De 6 a 7", "De 7 a 8", "De 8 a 9", "De 9 a 10"), each = 6), 15),
                                    "_", rep(2015:2020, 75), 
                                    sep = ""), "X.1")
distr_uni <- distr_uni[,-ncol(distr_uni)]
distr3 <- distr_uni %>% pivot_longer(-X, names_to = "dato", values_to = "cifra")
distr3 <- data.frame(distr3, do.call(rbind.data.frame, str_split(distr3$X, "_")),
                     do.call(rbind.data.frame, str_split(distr3$dato, "_")))
colnames(distr3) <- c("X", "dato", "cifra", "CCAA", "convoc", "fase", "centro", "nota", "anio")


nota_acceso <- read.csv("http://estadisticas.mecd.gob.es/EducaJaxiPx/files/_px/es/csv_sc/Universitaria/PAU/PAU20/l0/PAU0223.px?nocab=1",
                      sep = ";", na.strings = "..",
                      skip = 7, nrows = 76)
nota_acceso <- nota_acceso[-which(is.na(nota_acceso[,2])),]
nota_acceso[,1] <- paste0(rep(c("Total",
                              "Andalucía",
                              "Aragón",
                              "Asturias (Principado de)",
                              "Balears (Illes)",
                              "Canarias",
                              "Cantabria",
                              "Castilla - La Mancha",
                              "Castilla y León",
                              "Catalunya",
                              "Comunitat Valenciana",
                              "Extremadura",
                              "Galicia",
                              "Madrid (Comunidad de)",
                              "Murcia (Región de)",
                              "Navarra (Comunidad Foral de)",
                              "País Vasco",
                              "Rioja (La)"), each = 3), "_", rep(c("Total", "GenEsp", "Gen"), 18), 
                        sep = "")
colnames(nota_acceso) <- c("X", paste(rep(c("Total", "Ord", "Extraord"), each = 30), "_",
                                    rep(rep(c("Total", "Pub", "Pri", "Conc", "NoConsta"), each = 6), 3),
                                    "_", rep(2015:2020, 15), 
                                    sep = ""), "X.1")
nota_acceso <- nota_acceso[,-ncol(nota_acceso)]
nota3 <- nota_acceso %>% pivot_longer(-X, names_to = "dato", values_to = "cifra")
nota3 <- data.frame(nota3, do.call(rbind.data.frame, str_split(nota3$X, "_")),
                    do.call(rbind.data.frame, str_split(nota3$dato, "_")))
colnames(nota3) <- c("X", "dato", "cifra", "CCAA", "fase", "convoc", "centro", "anio")
