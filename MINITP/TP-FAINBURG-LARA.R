#Supongamos que un grupo de 44 personas asisten a un evento. ¿Cuál es la probabilidad de que ninguna persona del grupo comparta la misma fecha de cumpleaños? 
#Para resolver esto, considerá lo siguiente:
#entendemos por “cumpleaños” a coincidencia de día y mes,
#asumimos que el año tiene 365 días,
#asumimos que las fechas de cumpleaños de los asistentes son independientes entre sí,
#y consideramos que es igual de probable cumplir cualquiera de los 365 días del año.
n <- 44 #numero de personas en el grupo 
dias <- 365

proba_no_coincidir <- prod((dias - 0:(n-1)) / dias) #proba de que todos cumplan en dias distintos 
proba_coincidir <- 1 - proba_no_coincidir #proba de que coincidan por lo menos dos (por el complemento)

proba_no_coincidir 


#Tres plantas A, B y C producen un chip con las siguientes proporciones: A: 0.50, B: 0.30 y C: 0.20.
#Las tasas de producción defectuosa de cada planta son las siguientes: A: 0.015, B: 0.040 y C: 0.060.
#Se elige un chip al azar y resulta defectuoso. ¿Cuál es la probabilidad de que provenga de la planta C?
A <- 0.50; B <- 0.30; C <- 0.20 #probas de que un chip provenga de cada planta
dA <- 0.015; dB <- 0.040; dC <- 0.060 #defectuosos por planta

D <- A*dA + B*dB + C*dC #proba total de que sea defectuosa
proba_c_dado_D <- C*dC / D #proba de que sea de C dado que es defectuoso (x bayes)

proba_c_dado_D


#Sea B una variable aleatoria con distribución Binomial(n=28,p=0.390)
#y sea k′ el menor número entero tal que P(B≤k′)≥0.95
#buscar cual es ese k
n <- 28 #numero de ensayos
p <- 0.39 #proba de exito de cada ensayo

k <- qbinom(0.95, size = n, prob = p)
k

cat("El valor de k es:", k)

#Sea Z una variable aleatoria con distribución Poisson(λ=134.70)
#¿Cuál es la probabilidad de que Z sea par si se sabe que es menor que 200?

#uso proba condicional: p(z es par dado que z<200) = p(z es par intersección z<200)/p(z<200)
λ <- 134.7

denominador <- ppois(199, λ) #denominador: P(Z<200) ojo q no es menor o igual, solo menor!!

#numerador:suma de probas en los pares<200
pares <- seq(0, 199, by = 2) #hasta 199, y como quiero solo los pares va de a dos!!
numerador <- sum(dpois(pares, λ)) #numerador: P(z es par intersección z<200)

#proba condicional
p <- numerador / denominador
p

#Pablo tiene una caja con R bolitas rojas, V bolitas verdes, A bolitas azules y N bolitas negras. 
#Saca tres bolitas con reposición. Obtiene dos puntos por cada color distinto que obtenga entre las bolitas extraídas.
#Definimos la variable aleatoria X="Cantidad de puntos obtenidos".
#Calcular la función de probabilidad puntual de X en R.
set.seed(251) #ultimos 3 dígitos de mi DNI

#genero cantidades random (R,V,A,N) que van del 1 al 10
R <- sample(1:10, 1)
V <- sample(1:10, 1)
A <- sample(1:10, 1)
N <- sample(1:10, 1)

cat("Valores generados: R =", R, "V =", V, "A =", A, "N =", N, "\n") 

#construyo la urna con los valores generados
bolitas <- c(rep("Roja", R),
             rep("Verde", V),
             rep("Azul", A),
             rep("Negra", N))

#espacio muestral (3 extracciones con reposición)
muestras <- expand.grid(b1 = bolitas, b2 = bolitas, b3 = bolitas)

#función de puntaje
puntaje <- function(extraccion) {
  colores_distintos <- length(unique(extraccion))
  return(2 * colores_distintos)
}

#calculo x para cada extracción
X <- apply(muestras, 1, puntaje)

#probabilidades
tabla <- table(X) / length(X)
px <- data.frame(
  "Valor de X" = names(tabla),
  "Probabilidad" = round(as.numeric(tabla), 4)
)

#tabla
print(px, row.names = FALSE)

#grafico de la función de probabilidad (foto en el archivo pdf)
plot(tabla,
     main = "Px(k)",
     xlab = "cantidad de puntos",
     ylab = "probabilidad",
     col = "blue",
     ylim = c(0,1))



