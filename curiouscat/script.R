library(rjson)

#Quita Picanumeros y pon tu nombre de usuario entre comillas
usuario <- "Picanumeros" 

#Paso 1: recuperar los datos de la API de CuriousCat (estan todos en json lo cual facilita las cosas)
enlace <- paste0("https://api.curiouscat.me/api/v2/profile?username=", usuario ,"&count=100")

posts_curious <- fromJSON(file = enlace)
n_resp <- posts_curious$userData$answers

lista <- list()
lista[[1]] <- posts_curious
marca_tiempo <- posts_curious$posts[[100]]$timestamp
k <- 2
for(i in k:ceiling(n_resp/100)){
  temp <- fromJSON(file = paste0(enlace, "&max_timestamp=", marca_tiempo))
  lista[[i]] <- temp
  marca_tiempo <- temp$posts[[length(temp$posts)]]$timestamp
  cat("Iteración nº ", i, " concluida. A mimir 10 segundos.\n")
  Sys.sleep(10)
}

#Paso 2: pasar todos esos datos a un conjunto de datos exportable
datos <- list()
for(i in 1:length(lista)){
  id_post <- unlist(lapply(lista[[i]]$posts, function(x) x$id))
  fecha <- unlist(lapply(lista[[i]]$posts, function(x) x$timestamp))
  pregunta <- unlist(lapply(lista[[i]]$posts, function(x) x$comment))
  respuesta <- unlist(lapply(lista[[i]]$posts, function(x) x$reply))
  cuenta_que_la_envia <- unlist(lapply(lista[[i]]$posts, function(x) ifelse(is.null(x$senderData$username), NA, x$senderData$username)))
  likes <- unlist(lapply(lista[[i]]$posts, function(x) x$likes))
  
  temp <- data.frame(id_post, fecha, pregunta, respuesta, cuenta_que_la_envia, likes)
  datos[[i]] <- temp
}
datos <- do.call(rbind.data.frame, datos)
datos <- datos[which(duplicated(datos$id_post)==F),]
datos$fecha <- as.POSIXct(datos$fecha, origin="1970-01-01")

#Paso 3: exportar el conjunto de datos a un CSV
write.csv(datos, "PreguntasCuriousCat.csv", row.names = F)
