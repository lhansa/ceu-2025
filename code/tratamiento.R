# ####################################################
# Tratamiento manual de saltos en series de tiempo
# Leonardo Hansa
# 21 de mayo de 2025
# Sesión CEU
# ####################################################


# Librerías -------------------------------------
# La única librería que uso es la de visualización de 
# datos, ggplot2. Ahora bien, la parte de procesado
# se puede hacer con dplyr de una forma más moderna, 
# o con data.table de una forma más eficiente.
library(ggplot2)

# Parámetros de generación de datos -------------

# Los datos están generados aleatoriamente pero con una 
# forma concreta. Para que puedas replicar el ejercicio
# y que te salgan los mismos resultados, 
# tienes que indicar que la  generación se haga igual. Eso
# lo haces con un parámetro llamado semilla (seed). Lo
# especificas con la función set.seed()
set.seed(31818)

# Generaré datos semanales de un poco más de 2 años
n_dates <- 52 * 2.5
time <- 1:n_dates

# Generación de datos ----------------------------

# El proyecto iba sobre visitas a la web. Genero
# una serie que parte de 100 y crece semanalmente en
# tendencia. Además,
# pongo un poco de ruido para que no sea una línea recta.
visits <- 100 + 0.8 * time + rnorm(n_dates, 0, 5)

# Ahora pongo el escalón. A partir del tercer año, 
# las visitas aumentan un 30% sobre las originales.
visits <- ifelse(time > 52 * 2, visits * 1.3, visits)

# Y creo un data frame con fechas y visitas porque será
# más cómodo para usar luego ggplot2.
df <- data.frame(
  date = 1:n_dates,
  visits
)

# ggplot2 suele partir de un data frame, así que
# es lo primero que pongo
ggplot(df) + 
  # hago un gráfico de series temporales, así que uso líneas con geom_line. 
  # en el eje x están las fechas, en el y las visitas. La línea la pongo
  # de color morado y con un grosor de 1
  geom_line(aes(x = date, y = visits), color = "#800080", linewidth = 1) +
  # especifico los límites del eje vertical, sobre todo para que
  # ponga la base en 0.
  scale_y_continuous(limits = c(0, max(df$visits) * 1.1 )) + 
  # labs() es lo que uso yo para poner títulos y etiquetas, 
  # pero hay más formas
  labs(
    title = "Evolución de visitas semanales durante 2 años y un poco más",
    x = "",
    y = "Visitas",
    caption = "Los datos son inventados"
  )

# Generación de campaña ----------------------------

# Añado una columna nueva al data frame. Será una columna
# con unos y ceros; los unos indican que hubo campaña en 
# en ese momento. Normalmente, este dato indica la intensidad de 
# la campaña, como la inversión, pero en este caso no lo necesito
# por el tipo de ejercicio que hago.
# La campaña será una columna con tantos valores
# como fechas hay. 
campaign <- numeric(n_dates)
# Si la fecha está en el primer quinto del tercer año, 
# entonces pongo 1 (hay campaña); si no, 0.
campaign <- ifelse(time > 52 * 2 & time < 52 * 2.2, 1, 0)

df$campaign <- campaign

# Esta columna la usarás en los ejercicios.

# El gráfico es igual, solo que añado una franja en la zona
# donde hay campaña. Lo hago sin apoyarme en ninguna columna nueva. 
# Pongo manualmente los valores donde aparecerá la franja,
# que será el primer quito del tercer año. La función es annotate() pero
# hay otras formas de hacerlo.
ggplot(df) + 
  geom_line(aes(x = date, y = visits), color = "#800080", linewidth = 1) +
  scale_y_continuous(limits = c(0, max(df$visits) * 1.1 )) + 
  annotate(
    "rect",
    xmin = 52 * 2, xmax = 52 * 2.2, ymin = -Inf, ymax = Inf, 
    fill = "#800080", 
    alpha = 0.2
  ) +
  labs(
    title = "Evolución de visitas semanales durante 2 años y un poco más",
    x = "",
    y = "Visitas",
    caption = "Los datos son inventados"
  )

# Tratamiento de escalón --------------------------------

# El tratamiento del escalón es manual. Lo que hago
# es que, a partir de la subida de datos debida a la nueva medición, 
# aplico un factor de corrección. Hay muchas formas.
# Una rápida es que tomo el último valor antes del cambio
# y el primer valor después del cambio. El ratio de uno entre
# otro me da el factor. 
# CUIDADO. Aquí estoy asumiendo que entre una semana y otra
# el valor real no cambió. Si de verdad hubo una subida o una bajada
# entonces estoy perdiendo esa variación. Esto es solo una aproximación
# para una situación en la que no tenía datos.
correction_factor <- df$visits[52 * 2] / df$visits[52 * 2 + 1]

# Por ejemplo, otra forma de hacerlo sería tomar la media de
# un periodo y la media de otro. Tampoco es 100% fiable porque, como
# la serie tiene una tendencía general, lo que estarías haciendo
# es tomar el valor de la serie a la mitad de cada periodo. Eso no es
# más correcto que tomar la diferencia entre los extremos de los periodos. 
# En cualquier caso, te dejo el código comentado:
# mean_period_1 <- mean(df$visits[(52 * 1.9):(52 * 2)])
# mean_period_2 <- mean(df$visits[(52 * 2 + 1):length(df$visits)])
# correction_factor <- mean_period_1 / mean_period_2

# Ahora creo una nueva columna en el data frame con la serie corregida. 
# La serie es la misma que la original durante los dos primeros
# años, y después es la original multiplicada por el factor de corrección.
df$visits_corrected <- ifelse(
  df$date <= 52 * 2,
  df$visits,
  df$visits * correction_factor 
)

# Visualizo finalmente las dos series a la vez, como dos líneas 
# diferentes, pero con una será rayada para que se vea la diferencia.
ggplot(df) + 
  geom_line(aes(x = date, y = visits), color = "#800080", linewidth = 1, linetype = 2) +
  geom_line(aes(x = date, y = visits_corrected), color = "#800080", linewidth = 1) +
  scale_y_continuous(limits = c(0, max(df$visits) * 1.1 )) + 
  labs(
    title = "Evolución de visitas semanales originales y  corregidas",
    x = "",
    y = "Visitas",
    caption = "Los datos son inventados"
  )


# Ejercicios -------------------------------------

# 1. Rehaz el código anterior con herramientas tidyverse (como dplyr)

# 2. Define el factor de corrección usando los datos de cuándo empieza 
# la campaña. El factor vendrá dado por el valor de la serie de visitas
# en el momento en que empieza la campaña frente al valor justo antes
# de que empiece la campaña.