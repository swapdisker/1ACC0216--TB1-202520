library(dplyr); library(readr); library(lubridate); library(ggplot2); library(scales)

df <- if (exists("hotel_data")) hotel_data else readr::read_csv("hotel_bookings_winsorized.csv", show_col_types = FALSE)

# ====== MES DEL EVENTO (cancelación/confirmación) ======
if ("reservation_status_date" %in% names(df)) {
  df$mes_evento <- floor_date(ymd(df$reservation_status_date), "month")
} else {
  df$mes_evento <- floor_date(parse_date_time(
    paste(df$arrival_date_year, df$arrival_date_month, "1"),
    orders = c("Y b d","Y B d")), "month")
}

# ====== VARIABLE DE CANCELACIÓN ======
df$is_canceled <- as.integer(suppressWarnings(as.numeric(df$is_canceled)))

# ====== TABLAS BASE ======
cancel_mensual <- df %>%
  filter(!is.na(mes_evento)) %>%
  group_by(mes_evento) %>%
  summarise(
    reservas    = n(),
    canceladas  = sum(is_canceled == 1, na.rm = TRUE),
    cancel_rate = canceladas / reservas,
    .groups = "drop"
  ) %>%
  arrange(mes_evento)

cancel_mensual_hotel <- df %>%
  filter(!is.na(mes_evento)) %>%
  group_by(hotel, mes_evento) %>%
  summarise(
    reservas    = n(),
    canceladas  = sum(is_canceled == 1, na.rm = TRUE),
    cancel_rate = canceladas / reservas,
    .groups = "drop"
  ) %>%
  arrange(hotel, mes_evento)

# ====== TABLAS ======
cat("\n=== CANCELACIÓN MENSUAL (GLOBAL) — TABLA COMPLETA ===\n")
cancel_mensual_fmt <- cancel_mensual %>%
  mutate(mes = format(mes_evento, "%Y-%m"),
         cancel_rate = percent(cancel_rate, accuracy = 0.1)) %>%
  select(mes, reservas, canceladas, cancel_rate)
print(cancel_mensual_fmt, n = Inf, width = Inf)

cat("\n=== CANCELACIÓN MENSUAL POR HOTEL — TABLA COMPLETA ===\n")
cancel_mensual_hotel_fmt <- cancel_mensual_hotel %>%
  mutate(mes = format(mes_evento, "%Y-%m"),
         cancel_rate = percent(cancel_rate, accuracy = 0.1)) %>%
  select(hotel, mes, reservas, canceladas, cancel_rate)
print(cancel_mensual_hotel_fmt, n = Inf, width = Inf)

# ====== GRÁFICOS ======
p_cancel_global <- ggplot(cancel_mensual, aes(x = mes_evento, y = cancel_rate)) +
  geom_line() + geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Tasa de cancelación mensual (global)",
       x = "Mes", y = "Cancelaciones / Reservas (%)") +
  theme_minimal(base_size = 12)
print(p_cancel_global)

p_cancel_hotel <- ggplot(cancel_mensual_hotel, aes(x = mes_evento, y = cancel_rate, color = hotel, group = hotel)) +
  geom_line() + geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Tasa de cancelación mensual por hotel",
       x = "Mes", y = "Cancelaciones / Reservas (%)", color = "Hotel") +
  facet_wrap(~ hotel, ncol = 1, scales = "free_x") +
  theme_minimal(base_size = 12)
print(p_cancel_hotel)
