# Abrimos todos los paquetes que vamos a usar 
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(readr)
##### SETEAMOS LA SEED QUE VAMOS A USAR
set.seed(1)
################################################################################
###EJERCICIO DE ANALISIS DE DATOS
#Cargamos el dataset 
cardata <- read_csv("C:/Users/jrami/Downloads/used_cars.csv")
# Ver el dataset
View(cardata)
#1.Preguntas generales del dataset: 
  #a.¿Cuántos autos se vendieron?
  #b.¿Cuántos de cada marca se vendieron?
  #c.Clasificarlos en gama económica (a los que valen menos de 25000),
  # media (a los que valen más de 25000 y menos de 55000) y de lujo 
  # ( valen más que 55000) y modificar el data set. 
  # Y ver cuántos hay de cada gama.

#a.
#Vemos la cantidad de autos vendidos contando las filas 
num_autos <- nrow(cardata)
cat("Cantidad total de autos:", num_autos, "\n")

#b.
#Aqui vemos cuantos autos vendio cada marca
conteo_marcas <- cardata %>%
  group_by(brand) %>%
  summarise(Cantidad = n()) %>%
  arrange(desc(Cantidad))

print(head(conteo_marcas, 57), n = 57)
#c.
# A partir de este momento separamos la base de datos en tres
# Limpiamos de los datos de millas y precios los simbolos que no son numeros 
# para asi poder trabajar con ellos 
cardata_limp <- cardata %>%
  mutate(
    milage = as.numeric(gsub("[^0-9.]", "", milage)),
    price  = as.numeric(gsub("[^0-9.]", "", price))
  ) %>%
  filter(!is.na(milage), !is.na(price))

View(cardata_limp)
cardata_eco <- cardata_limp %>%
  filter(price <= 25000)

num_autos_eco <- nrow(cardata_eco)
cat("Cantidad total de autos de gama baja:", num_autos_eco, "\n")

cardata_media <- cardata_limp %>%
  filter(price > 25000 & price <= 55000)

num_autos_media <- nrow(cardata_media)
cat("Cantidad total de autos de gama media:", num_autos_media, "\n")

cardata_lujo <- cardata_limp %>%
  filter(price > 55000)
num_autos_lujo <- nrow(cardata_lujo)
cat("Cantidad total de autos de gama alta:", num_autos_lujo, "\n")

#2)  Cambio de características del data set:
#A)Modificar millas a kilómetros y  cambiar de dolares a pesos, 
#cual es el auto que más kilómetros tiene y el que más vale
#B) Agregar las clasificaciones a las columnas



#A) 
cardata_limp <- cardata_limp %>%
mutate(milage = milage * 1.60934)  # Sobrescribe la variable original 
view(cardata_limp)

#B)
cardata_limp <- cardata_limp %>%
  mutate(
    clasificacion = case_when(
      price <= 25000 ~ "Económico",
      price > 25000 & price <= 55000 ~ "Medio", 
      price > 55000 ~ "Lujo"
    )
  )

view(cardata_limp)
#3)  Correlaciones:
  #a)Graficar la distribución de los precios de los autos
  #b)Encontrar la correlación entre  precio y kilómetros realizados
  #c)Encontrar la correlación entre precio y año

#a)Graficamos la distribucion de los precios de los autos

ggplot(cardata_limp, aes(x = price)) +
  geom_histogram(binwidth = 2000, fill = "blue", color = "red", alpha = 0.7) +
  labs(title = "Distribución de precios de autos usados",
       x = "Precio (Pesos)",
       y = "Cantidad de autos") +
  theme_minimal()
#Comentario vemos que el grafico la distribucion esta estirado debido a que existen
#ciertos autos que su precio es relativamente muy alto como por ejemplo el Bugatti

#Districucion
ggplot(cardata_limp, aes(x = price)) +
  geom_histogram(binwidth = 2000, fill = "blue", color = "red", alpha = 0.7) +
  coord_cartesian(xlim = c(0, 200000)) +
  labs(title = "Distribución de precios (hasta 200.000 USD)",
       x = "Precio (USD)",
       y = "Cantidad de autos") +
  theme_minimal()

# Para solucionar esto acotamos las observaciones a aquellas que su precio sea menor
# a un valor arbitrario, en este caso 200000

##Correlacion de los autos sin separarlos por grupos
# B) Correlaci{on precios y kilometros realizados
cor(cardata_limp$milage, cardata_limp$price, use = "complete.obs")
# C) Correlaci{on precios y año de modelo
cor(cardata_limp$model_year, cardata_limp$price, use = "complete.obs")


##################################################################################
#####EJERCICIO DE ANALISIS ECONOMETRICO 

# Primero importamos los datos del data set
gapminder <- read_csv("C:/Users/jrami/Downloads/gapminder.csv")
View(gapminder)

#1. Grafica la evolucion temporal de la variable income per person en Argentina. 
#   Comenta brevemente la tendencia observada.
### Primero filtramos los datos de Argentina para eso usamos el comando filter
argentina <- gapminder %>%
  filter(country == "Argentina")
### Graficamos la evolución temporal del ingreso per cápita
ggplot(argentina, aes(x = year, y = income_per_person)) +
  geom_line(color = "red", linewidth = 1.1) +
  labs(
    title = "Evolución del ingreso por persona en Argentina",
    x = "Año",
    y = "Ingreso por persona (USD)",
    caption = "Fuente: gapminder"
  )
### En la tendencia podemos ver como en general Argentina tiene una tendencia
### positiva, es decir, en general podemos ver que el ingreso por persona en 
### Argentina evoluciona de forma crerciente en el tiempo. Aunque podemos 
### notar grandes bajas tanto en el periodo desde los 80s hasta los 90s
### y en el 2000 por las crisis que ya conocemos 



##Ejercicio 2
#2)Para el entrenamiento, separar los ultimos 10 años de Argentina para
#testear posteriormente. Estima regresiones de income per person sobre
#el tiempo t utilizando tres modelos:
#a) Modelo lineal: income per person = β1t + β0 + ε
#b) Modelo polin´omico de grado 2: income per person = β2t^2 + β1t + β0 + ε
#c) Modelo polin´omico de grado 10: income per person ∼ β0+β1t+β2t^2 + · · · + β10t^10 + ε

## Primero separamos los datos para trainig y para testear
arg_train <- argentina %>% filter(year <= max(year) - 10)
arg_test  <- argentina %>% filter(year >  max(year) - 10)
# Modelo lineal
modelo_lin <- lm(income_per_person ~ year, data = arg_train)

# Modelo cuadrático
modelo_quad <- lm(income_per_person ~ poly(year, 2, raw = TRUE), data = arg_train)

# Modelo polinómico grado 10
modelo_poly10 <- lm(income_per_person ~ poly(year, 10, raw = TRUE), data = arg_train)

# Predicciones sobre el conjunto de entrenamiento

train_pred <- arg_train %>%
  mutate(
    modelo_lin = predict(modelo_lin, newdata = arg_train),
    modelo_quad = predict(modelo_quad, newdata = arg_train),
    modelo_poly10 = predict(modelo_poly10, newdata = arg_train)
  )
ggplot(train_pred, aes(x = year, y = income_per_person)) +
  geom_point(color = "black") +
  geom_line(aes(y = modelo_lin, color = "Lineal"), linewidth = 1) +
  geom_line(aes(y = modelo_quad, color = "Polinomio grado 2"), linewidth = 1) +
  geom_line(aes(y = modelo_poly10, color = "Polinomio grado 10"), linewidth = 1) +
  labs(
    title = "Modelos de ingreso por persona en Argentina (entrenamiento)",
    x = "Año",
    y = "Ingreso por persona"
  ) +
  scale_color_manual(values = c("Lineal" = "purple", "Polinomio grado 2" = "red", "Polinomio grado 10" = "green")) +
  theme_minimal()

#### AGREGADO PREDICTION DE LOS ULTIMOS 10 AÑOS
##Predicciones sobre test
test_pred <- arg_test %>%
  mutate(
    modelo_lin = predict(modelo_lin, newdata = arg_test),
    modelo_quad = predict(modelo_quad, newdata = arg_test),
    modelo_poly10 = predict(modelo_poly10, newdata = arg_test)
  )
## Gráfico de predicciones sobre test
ggplot(test_pred, aes(x = year, y = income_per_person)) +
  geom_point(color = "black") +
  geom_line(aes(y = modelo_lin, color = "Lineal"), linewidth = 1) +
  geom_line(aes(y = modelo_quad, color = "Polinomio grado 2"), linewidth = 1) +
  geom_line(aes(y = modelo_poly10, color = "Polinomio grado 10"), linewidth = 1) +
  labs(
    title = "Predicciones de los modelos sobre el conjunto de test (últimos 10 años)",
    x = "Año",
    y = "Ingreso por persona"
  ) +
  scale_color_manual(values = c("Lineal" = "purple", "Polinomio grado 2" = "red", "Polinomio grado 10" = "green")) +
  theme_minimal()
### Creemos que puede estar overfitteado 


### ¿Que observamos? Podemos observar en el grafico como en las distintas regresiones
### se ve que existe una relacion positiva entre los años y el ingreso por persona,
### tambien podemos observar, como era de esperar, que la regresion del polinomio
### de grado 10 se fittea mucho mas a los datos que las otras.

# P3. Selecciona cuatro paises sudamericanos distintos de Argentina. Con ellos,
#arma:
#a) Una matriz de correlaciones entre los ingresos (income per person)
#de los cinco paises.
#b) Una matriz de correlaciones entre las variaciones porcentuales anuales
#)crecimiento interanual, Y /Y ) de dichos ingresos.

# Elegimos estos paises
paises <- c("Argentina", "Guyana", "Paraguay", "Bolivia", "Uruguay")

gap_sud <- gapminder %>%
  filter(country %in% paises) %>%
  select(country, year, income = income_per_person)

#Primero vamos a pasar los datos a formato ancho: cada país una columna

gap_wide <- gap_sud %>%
  pivot_wider(names_from = country, values_from = income)

head(gap_wide)


#### a) Matriz de correlaciones entre ingresos

cor_ingresos <- gap_wide %>%
  select(-year) %>%
  cor(use = "pairwise.complete.obs")

print("Matriz de correlaciones entre ingresos (niveles):")
print(round(cor_ingresos, 3))

####b) Calcular variaciones porcentuales interanuales (crecimiento Y/Y)

gap_var <- gap_sud %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(growth = (income - lag(income)) / lag(income) * 100) %>%
  ungroup()

# Cambiar las filas por columnas
gap_wide_growth <- gap_var %>%
  select(country, year, growth) %>%
  pivot_wider(names_from = country, values_from = growth)

# Matriz de correlaciones entre variaciones
cor_growth <- gap_wide_growth %>%
  select(-year) %>%
  cor(use = "pairwise.complete.obs") # sirve para que no se descarte el pais completo
# cuando hay datos faltantes

print(round(cor_growth, 3))
#Parte 2
#Inciso 5

# Filtramos un año (por ejemplo 2015 o el último disponible)
datos <- gapminder %>% filter(year == 2010)
# Aseguramos que las variables sean NUMÉRICAS y quitar NA/ceros raros
datos_limpios <- datos %>%
  mutate(
    life_expectancy  = as.numeric(life_expectancy),
    life_expectancy_female = as.numeric(life_expectancy_female)
  ) %>%
  filter(!is.na(life_expectancy),
         !is.na(life_expectancy_female),
         life_expectancy > 0,
         life_expectancy_female > 0)
# Graficoos
ggplot(datos_limpios, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) + 
  #Hacemos esto para emprolijar el gr'afico y que quede a escala
  scale_x_continuous(limits = c(40,90), expand = expansion(mult = 0.02))+
  scale_y_continuous(limits = c(40,90), expand = expansion(mult = 0.02))+
  labs(title = "Esperanza de vida total vs femenina (2010)",
       x = "Esperanza de vida femenina",
       y = "Esperanza de vida total")

# Respuesta teórica: Vemos que hay una correlación positiva entre la esperanza
# de vida total y la esperanza de vida femenina lo cual es algo muy intuitivo


#Inciso 6 



#  Filtrar un año (último disponible)
datos <- gapminder %>%
  filter(year == 2010) %>%
  select(life_expectancy, life_expectancy_female) %>%
  drop_na()

#  Convertir a numérico
datos6 <- datos %>%
  mutate(
    life_expectancy = as.numeric(life_expectancy),
    life_expectancy_female = as.numeric(life_expectancy_female)
  )

#  Estimar modelo
modelo <- lm(life_expectancy ~ life_expectancy_female, data = datos6)

#  Mostrar resultados
summary(modelo)

# Extraemos coeficiente, error estándar y grados de libertad
b1  <- coef(modelo)[2]
se1 <- summary(modelo)$coefficients[2, 2]
gl  <- modelo$df.residual

# Calculamos el estadístico t para H0: beta1 = 1
t_valor  <- (b1 - 1) / se1

# Calculamos el p-valor bilateral
p_valor <- 2 * pt(-abs(t_valor), df = gl)

# Mostramos resultados
cat("β1 estimado:", round(b1, 4), "\n")
cat("Error estándar:", round(se1, 4), "\n")
cat("t =", round(t_valor, 3), "\n")
cat("p-valor =", round(p_valor, 5), "\n")

#Inciso 8

#  Filtrar un solo año (último disponible)
datos8 <- gapminder %>%
  filter(year == max(year)) %>%
  select(life_expectancy, life_expectancy_female, income_per_person) %>%
  drop_na()

#  Convertir a numérico 
datos8 <- datos8 %>%
  mutate(
    life_expectancy = as.numeric(life_expectancy),
    life_expectancy_female = as.numeric(life_expectancy_female),
    income_per_person = as.numeric(income_per_person)
  )

#  Regresión múltiple
modelo8 <- lm(life_expectancy ~ life_expectancy_female + income_per_person,
              data = datos8)

#  Resultados
summary(modelo8)


# Inciso 9

#  Filtramos el último año disponible y seleccionamos variables relevantes
datos9 <- gapminder %>%
  filter(year == max(year)) %>%
  select(life_expectancy, child_mortality, life_expectancy_male, population) %>%
  drop_na()

#  Convertimos a numérico y creamos log(población)
datos9 <- datos9 %>%
  mutate(
    life_expectancy      = as.numeric(life_expectancy),
    child_mortality      = as.numeric(child_mortality),
    life_expectancy_male = as.numeric(life_expectancy_male),
    population           = as.numeric(population),
    log_population       = log(population)
  )
# Log_population tasa de crecimiento de la población

# Estimamos el modelo lineal con tres covariables
modelo9 <- lm(life_expectancy ~ child_mortality + life_expectancy_male + log_population,
              data = datos9)

# Mostramos los resultados
summary(modelo9)


################################################################################
#### EJERCICIO 3 
########### Ejercicio TEG
### Ecribe una función en R llamada resultado ataque
# El atacante puede tirar entre 1 y 3 dados ( depende si la cantidad de fichas
# que tiene el país son 3 o más)
# El defensor puede tirar 1 o 3 dados, pero no más que al atacante

# n_atacante <- con cuantos dados ataca
# N_defensro con cuantos dados se defiende

resultado_ataque <- function(n_atacante,n_defensor) {
  
  #tirada de dados
  
  dados_ataque <- sort(sample(1:6,n_atacante, replace = TRUE),decreasing = TRUE)
  
  dados_defensa <- sort(sample(1:6,n_defensor,replace = TRUE), decreasing = TRUE)
  
  #cantidad de comparaciones posibles
  
  n_comp <- min(n_atacante, n_defensor)
  
  # comparar
  
  perdidas_ataque <- 0
  
  perdidas_defensa <- 0
  
  for (i in 1:n_comp) {
    
    if (dados_ataque[i] > dados_defensa[i]) {
      
      perdidas_defensa <- perdidas_defensa + 1
      
    } else {
      
      perdidas_ataque <- perdidas_ataque + 1
      
    }
  }
  # Recuerdo en que en los empates pierde el atacante.En else están incluídos esos
  # casos
  # devolver vector
  return(c(perdidas_ataque = perdidas_ataque,
           perdidas_defensa = perdidas_defensa))
}

set.seed(1)
resultado_ataque(3,2)

# Ahora vamos a hacer una función que se llame simular_batalla, que reciba como
# argumentos el número de fichas del atacante y del defensor, y que simule una
# batalla completa. El atacante ataca hasta que conquiste al otro, o se quede
# sin fichas

simular_batalla <- function(fichas_atacante, fichas_defensor) {
  
  stopifnot(fichas_atacante >= 1, fichas_defensor >= 0) 
  
  # Bucle de ataques
  
  while (fichas_atacante > 1 && fichas_defensor > 0) {
    
    dados_ataque <- min(3, fichas_atacante - 1)
    
    dados_defensor <- min(3, fichas_defensor, dados_ataque)
    
    res <- resultado_ataque(dados_ataque, dados_defensor)
    
    fichas_atacante <- fichas_atacante  - res["perdidas_ataque"]
    
    fichas_defensor <- fichas_defensor - res["perdidas_defensa"]
  }
  list(
    atacante = fichas_atacante,
    
    defensor = fichas_defensor,
    
    conquistó = (fichas_defensor == 0)
  )
}

set.seed(1)

simular_batalla(3,2)

################# Construcción de probabilidad ataque

probabilidad_ataque <- function(B = 1000, A = 5, D = 5) {
  
  conquistas <- replicate (B , simular_batalla(A, D)$conquistó)
  
  mcon <- mean(conquistas) # Media conquistas
  
  list( estimación = mcon )
}
set.seed(1)
probabilidad_ataque(B=1000)



