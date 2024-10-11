set.seed(1)

# SIMULACIÓN CONSIDERANDO UN ERROR DE MEDIDA DE σ = 1

a <- 10
b <- 8

# Lo vamos a hacer con una Normal estándar, pero serviría cualquier distribución centrada en 0
# (ha de estar centrada en 0 porque el instrumento de medida es insesgado, es decir, tiene error aleatorio pero no sistemático)
# Por ejemplo, se puede probar con una uniforme entre -sqrt(3) y sqrt(3) (que también tiene varianza 1)
# cambiando el rnorm(1000000) por runif(1000000, -sqrt(3), sqrt(3))
# Y si nos ponemos exóticos, con una Poisson con lambda = 1 y restándole una unidad con rpois(1000000, 1) - 1
mediciones_individuales_a <- a + rnorm(1000000)
mediciones_individuales_b <- b + rnorm(1000000)
mean(mediciones_individuales_a) # Promedio de las estimaciones =~ 10
mean(mediciones_individuales_b) # Promedio de las estimaciones =~ 8
sd(mediciones_individuales_a) # Desviación (error) de ~1 unidad. Coincide con σ.
sd(mediciones_individuales_b) # Desviación (error) de ~1 unidad. Coincide con σ.

medicion_T <- a + b + rnorm(1000000)
medicion_D <- a - b + rnorm(1000000)
medicion_conjunta_a <- (medicion_T + medicion_D)/2
medicion_conjunta_b <- (medicion_T - medicion_D)/2
mean(medicion_conjunta_a) # Promedio de las estimaciones =~ 10
mean(medicion_conjunta_b) # Promedio de las estimaciones =~ 8
sd(medicion_conjunta_a) # Desviación (error) de ~0.708 unidades. Coincide aprox. con σ/sqrt(2) = 0.707
sd(medicion_conjunta_b) # Desviación (error) de ~0.708 unidades. Coincide aprox. con σ/sqrt(2) = 0.707
