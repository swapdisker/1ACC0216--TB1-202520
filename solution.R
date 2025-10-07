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