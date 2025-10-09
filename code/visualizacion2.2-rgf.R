# ====== LIBRERÍAS ======
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(scales)

# ====== CARGA DE DATOS ======
# Usa 'hotel_data' si ya existe; si no, lee el CSV (ajusta el nombre si es distinto)
df <- if (exists("hotel_data")) hotel_data else readr::read_csv("hotel_bookings_winsorized.csv", show_col_types = FALSE)

# ====== LIMPIEZA MÍNIMA (sólo variables necesarias) ======
to_num <- function(x) suppressWarnings(as.numeric(x))

df <- df %>%
  mutate(
    required_car_parking_spaces = as.integer(to_num(required_car_parking_spaces)),
    wants_parking = required_car_parking_spaces > 0,
    
    is_canceled = as.integer(to_num(is_canceled)),           # 0/1
    adr         = to_num(adr),                               # tarifa promedio
    stay_nights = to_num(stays_in_weekend_nights) + to_num(stays_in_week_nights)
  )

# Chequeo rápido (por si hay NAs en las columnas clave)
# (No imputamos: solo ignoramos NAs en los promedios)
# dplyr::glimpse(df)

# ====== 1) CUÁNTO: Solicitud de estacionamiento (global y por hotel) ======
n_total <- nrow(df)
k_park  <- sum(df$wants_parking, na.rm = TRUE)
p_park  <- k_park / n_total
se      <- sqrt(p_park*(1-p_park)/n_total)
ic      <- p_park + c(-1,1)*1.96*se

cat("\n=== GLOBAL: Solicitud de estacionamiento ===\n")
cat(sprintf("Con estacionamiento: %d de %d (%.2f%%) | IC95%%: %.2f%% – %.2f%%\n",
            k_park, n_total, 100*p_park, 100*ic[1], 100*ic[2]))

por_hotel <- df %>%
  group_by(hotel) %>%
  summarise(
    reservas = n(),
    con_parking = sum(wants_parking, na.rm = TRUE),
    prop = con_parking / reservas,
    .groups = "drop"
  ) %>%
  arrange(desc(prop))

cat("\n=== POR HOTEL: Solicitud de estacionamiento (conteo y proporción) ===\n")
print(por_hotel %>% mutate(prop = percent(prop, accuracy = 0.1)))

# Gráfico 1: proporción que solicita parking por hotel
p1 <- ggplot(por_hotel, aes(x = reorder(hotel, prop), y = prop, fill = hotel)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Proporción de reservas que solicitan estacionamiento por hotel",
       x = "Hotel", y = "Reservas con solicitud de estacionamiento (%)", fill = "Hotel") +
  theme_minimal(base_size = 12)
print(p1)

# ====== 2) ¿ES IMPORTANTE? Asociación con resultados (cancelación, ADR, estadía) ======

# 2a) Cancelación (0/1): menor cancelación con parking = señal de valor
cancel_tbl <- df %>%
  group_by(hotel, wants_parking) %>%
  summarise(
    reservas = n(),
    cancel_rate = mean(is_canceled == 1, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== TASA DE CANCELACIÓN por estacionamiento y hotel ===\n")
print(cancel_tbl %>% mutate(cancel_rate = percent(cancel_rate, accuracy = 0.1)))

# Gráfico 2: cancelación por parking (facet por hotel)
p2 <- ggplot(cancel_tbl,
             aes(x = factor(wants_parking, levels=c(FALSE, TRUE), labels=c("Sin parking", "Con parking")),
                 y = cancel_rate, fill = factor(wants_parking))) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Tasa de cancelación según solicitud de estacionamiento",
       x = NULL, y = "Cancelación (%)", fill = "Estacionamiento") +
  facet_wrap(~ hotel, ncol = 1) +
  theme_minimal(base_size = 12)
print(p2)

# 2b) ADR: mayor ADR con parking = clientes potencialmente más valiosos
adr_tbl <- df %>%
  group_by(hotel, wants_parking) %>%
  summarise(
    reservas  = n(),
    adr_mean  = mean(adr, na.rm = TRUE),
    adr_med   = median(adr, na.rm = TRUE),
    .groups   = "drop"
  )

cat("\n=== ADR (tarifa promedio) por estacionamiento y hotel ===\n")
print(adr_tbl %>% mutate(adr_mean = round(adr_mean,2), adr_med = round(adr_med,2)))

# Gráfico 3: ADR por parking (boxplot por hotel)
p3 <- ggplot(df,
             aes(x = factor(wants_parking, levels=c(FALSE, TRUE), labels=c("Sin parking","Con parking")),
                 y = adr, fill = factor(wants_parking))) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.25) +
  labs(title = "ADR por solicitud de estacionamiento",
       x = NULL, y = "ADR") +
  facet_wrap(~ hotel, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 12)
print(p3)

# 2c) Estadía: más noches con parking = captura de viajes más largos
stay_tbl <- df %>%
  group_by(hotel, wants_parking) %>%
  summarise(
    reservas   = n(),
    nights_mean= mean(stay_nights, na.rm = TRUE),
    nights_med = median(stay_nights, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Duración de estadía (noches) por estacionamiento y hotel ===\n")
print(stay_tbl %>% mutate(nights_mean = round(nights_mean,2), nights_med = round(nights_med,2)))

# Gráfico 4: estadía por parking (boxplot por hotel)
p4 <- ggplot(df,
             aes(x = factor(wants_parking, levels=c(FALSE, TRUE), labels=c("Sin parking","Con parking")),
                 y = stay_nights, fill = factor(wants_parking))) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.25) +
  labs(title = "Noches de estadía por solicitud de estacionamiento",
       x = NULL, y = "Noches") +
  facet_wrap(~ hotel, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 12)
print(p4)
