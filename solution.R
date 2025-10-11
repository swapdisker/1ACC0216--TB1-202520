
library(dplyr)
library(ggplot2) # Nice heatmaps
# Importamos las librerias necesarias

# Leemos el csv
# Reemplazar por su propio path
file_path <- "/home/cisco/Documents/Studies/uni-courses/FDS/positron/tb1-fds/hotel_bookings.csv"
bookings_df <-  read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# Resumen de estadisticas basicas
summary(bookings_df)
head(bookings_df)

# Identificacion de datos faltantes
# Detectamos datos NAs (mas que ello parecen haber datos NULL o indefinidos)
blank_data <- function(x) {
  sum = 0
  for (i in 1:ncol(x)) {
    cat("En la columna", colnames(x[i]), "total de valores en blanco: ", colSums(x[i] == ""), "\n")
  }
}
blank_data(bookings_df)

null_data <- function(x) {
  sum = 0
  for (i in 1:ncol(x)) {
    cat("En la columna", colnames(x[i]), "total de valores en blanco: ", colSums(x[i] == "NULL"), "\n")
  }
}
null_data(bookings_df)


# Tenemos un monton de "NULL"s especificamente en las columnas 
# agent y company. Para que R las reconozca como datos faltantes
# las pasaremos a NAs
bookings_df$agent[bookings_df$agent == "NULL"] <- NA
bookings_df$company[bookings_df$company == "NULL"] <- NA

# Ahora los podemos convertir a un tipo numerico. En nuestro dataset 
# la ausencia de dichos datos implica que el cliente hizo el booking
# directamente sin intermediario (agent) ni agencia (company)
# Los convertiremos a un valor que refleje que estan como Individuos.
bookings_df$agent <- as.numeric(bookings_df$agent)
bookings_df$company <- as.numeric(bookings_df$company)

# Creamos la feature 'has_agent': 1 si no es NA (no hizo el booking personalmente), 0 de otro modo (mediante agencia) 
bookings_df$has_agent <- as.integer(!is.na(bookings_df$agent))

# Creamos 'is_corporate' del mismo modo.
bookings_df$is_corporate <- as.integer(!is.na(bookings_df$company))

# Ahora imputamos el valor
bookings_df$agent[is.na(bookings_df$agent)] <- 0
bookings_df$company[is.na(bookings_df$company)] <- 0

library(DescTools)
library(tidyr)

# Seleccionar columnas continuas relevantes, utilize unicamente estas ya que me parecian las que podian tener picos mas relevantes
cols_outliers <- c("lead_time", "adr", "stays_in_week_nights",
                   "stays_in_weekend_nights", "children", "babies")

# Boxplots antes del tratamiento
bookings_df %>%
  select(all_of(cols_outliers)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Boxplots antes del tratamiento de outliers")

# Aplicar winsorización (5% - 95%), saque esta funcion de google, no entiendo muy bien la winsorizacion
bookings_df_wins <- bookings_df
for (col in cols_outliers) {
  bookings_df_wins[[col]] <- DescTools::Winsorize(bookings_df[[col]], val = quantile(bookings_df[[col]], probs = c(0.05, 0.95), na.rm = TRUE))
}

# Boxplots después del tratamiento
bookings_df_wins %>%
  select(all_of(cols_outliers)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Boxplots después del tratamiento (Winsorización 5%-95%)")

# Comparar resumen estadístico
summary_before <- summary(bookings_df[cols_outliers])
summary_after <- summary(bookings_df_wins[cols_outliers])

cat("\nResumen antes del tratamiento:\n")
print(summary_before)

cat("\nResumen después del tratamiento:\n")
print(summary_after)

# Guardar el nuevo dataset
write.csv(bookings_df_wins, "/home/cisco/Documents/Studies/uni-courses/FDS/positron/tb1-fds/hotel_bookings_winsorized.csv", row.names = FALSE)

cat("\n Dataset con outliers tratados guardado exitosamente como 'hotel_bookings_winsorized.csv'\n")


#======= VISUALIZACION DE LOS DATOS ========

#====== GRAFICO 1 =======
# Crear grafico mostrando la cantidad de reservas por tipo de hotel

ggplot(bookings_df_wins, aes(x = hotel, fill = hotel)) +
  geom_bar() +
  labs(title = "Cantidad de reservas por tipo de hotel",
       x = "Tipo de hotel",
       y = "Número de reservas") +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "orange"))


#======== GRAFICO 2 ========
#QUEREMOS SABER SI LA DEMANDA ESTA AUMENTANDO CON EL TIEMPO

# Agrupar por año y mes de llegada
demanda_mensual <- bookings_df_wins %>%
  group_by(arrival_date_year, arrival_date_month) %>%
  summarise(reservas = n(), .groups = 'drop')

# Se convierte el nombre del mes a numero para ordenarlo luego
demanda_mensual$arrival_date_month <- match(demanda_mensual$arrival_date_month, month.name)

# Lo usaremos para mostrarlo en el eje x cronologicamente
demanda_mensual <- demanda_mensual %>%
  mutate(fecha = as.Date(paste(arrival_date_year, arrival_date_month, "01", sep = "-")))

# Gráfico de línea temporal de la demanda mensual de todos los anios
ggplot(demanda_mensual, aes(x = fecha, y = reservas, color = factor(arrival_date_year))) +
  geom_line(size = 1) +
  geom_smooth(aes(group = 1), method = "loess", color = "#6A0DAD", se = FALSE, size = 1.3) +
  labs(title = "Evolución mensual de la demanda de reservas (2015–2017)",
       x = "Fecha",
       y = "Número de reservas",
       color = "Año") +
  theme_minimal()


#======== GRAFICO 3 ===========
#Grafico para saber las temporadas de reservas

# Agrupar por año y mes
heat_data <- bookings_df_wins %>%
  group_by(arrival_date_year, arrival_date_month) %>%
  summarise(reservas = n(), .groups = 'drop') %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

# Calcular umbrales numéricos
quantiles <- quantile(heat_data$reservas, probs = c(0.33, 0.66))
quantiles

heat_data <- heat_data %>%
  mutate(temporada = case_when(
    reservas <= quantiles[1] ~ "Baja",
    reservas <= quantiles[2] ~ "Media",
    TRUE ~ "Alta"
  ))


ggplot(heat_data, aes(x = arrival_date_month,
                      y = factor(arrival_date_year),
                      fill = temporada)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Baja" = "#AED6F1",
                               "Media" = "#5DADE2",
                               "Alta" = "#154360")) +
  labs(title = "Mapa de calor de temporadas de reservas por mes y año",
       x = "Mes",
       y = "Año",
       fill = "Temporada") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ======== GRAFICO 4 ===========
# Grafico para saber la duracion promedio de estadia por tipo de hotel


# Crear variable de duración total
bookings_df_wins <- bookings_df_wins %>%
  mutate(duracion_total = stays_in_week_nights + stays_in_weekend_nights)

# Crear gráfico boxplot
ggplot(bookings_df_wins, aes(x = hotel, y = duracion_total, fill = hotel)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  labs(title = "Distribución de la duración de las estancias por tipo de hotel",
       x = "Tipo de hotel",
       y = "Duración de la estancia (noches)") +
  theme_minimal()


