library(readr); library(dplyr); library(lubridate); library(ggplot2); library(scales)
default_path <- "hotel_bookings_winsorized.csv"

if (exists("hotel_data")) {
  df <- hotel_data
} else {
  df <- readr::read_csv(default_path, show_col_types = FALSE)
}

# ================== LIMPIEZA CLAVE ==================
to_num <- function(x) suppressWarnings(as.numeric(x))
df <- df %>%
  mutate(
    children = to_num(children),
    babies   = to_num(babies),
    children = if_else(is.na(children), 0, children),
    babies   = if_else(is.na(babies), 0, babies),
    has_kids = (children > 0 | babies > 0)
  )

# ================== 1) GLOBAL ==================
total_reservas <- nrow(df)
con_kids       <- sum(df$has_kids)
prop_global    <- con_kids / total_reservas
se             <- sqrt(prop_global * (1 - prop_global) / total_reservas)
ic             <- prop_global + c(-1, 1) * 1.96 * se

cat("\n=== GLOBAL ===\n")
cat(sprintf("Con niños/bebés: %d de %d (%.2f%%)\n", con_kids, total_reservas, 100*prop_global))
cat(sprintf("IC95%%: %.2f%% – %.2f%%\n", 100*ic[1], 100*ic[2]))

# ================== 2) POR HOTEL ==================
por_hotel_tbl <- df %>%
  group_by(hotel) %>%
  summarise(
    reservas = n(),
    con_kids = sum(has_kids),
    prop     = con_kids / reservas,
    .groups = "drop"
  ) %>%
  arrange(desc(prop))

cat("\n=== POR HOTEL (conteo y proporción) ===\n")
print(por_hotel_tbl %>% mutate(prop = percent(prop, accuracy = 0.1)))

# Gráfico barra por hotel (proporción)
p_hotel <- ggplot(por_hotel_tbl, aes(x = reorder(hotel, prop), y = prop, fill = hotel)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Proporción de reservas con niños y/o bebés por hotel",
       x = "Hotel", y = "Proporción de reservas con niños/bebés", fill = "Hotel") +
  theme_minimal(base_size = 12)
print(p_hotel)

# ================== 3) POR MES & HOTEL ==================
# Parse del mes a fecha (día 1 de cada mes)
df <- df %>%
  mutate(
    mes_fecha = parse_date_time(
      paste(arrival_date_year, arrival_date_month, "1"),
      orders = c("Y b d", "Y B d")  # b = mes abreviado (Jul), B = mes completo (July)
    ),
    mes = floor_date(mes_fecha, "month")
  )

por_mes_hotel_tbl <- df %>%
  filter(!is.na(mes)) %>%
  group_by(hotel, mes) %>%
  summarise(
    reservas = n(),
    con_kids = sum(has_kids),
    prop     = con_kids / reservas,
    .groups  = "drop"
  ) %>%
  arrange(hotel, mes)

# --- IMPRESIÓN COMPLETA ---
cat("\n=== POR MES & HOTEL (tabla completa) ===\n")
# Formato: mes YYYY-MM y prop en %
tab_impresa <- por_mes_hotel_tbl %>%
  mutate(
    mes  = format(mes, "%Y-%m"),
    prop = percent(prop, accuracy = 0.1)
  ) %>%
  arrange(hotel, mes) %>%
  as.data.frame()

# Imprime todas las filas para ambos hoteles
print(tab_impresa, row.names = FALSE)

# Gráfico línea por mes, facet por hotel
p_mes_hotel <- ggplot(por_mes_hotel_tbl, aes(x = mes, y = prop, color = hotel, group = hotel)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Proporción mensual de reservas con niños/bebés por hotel",
       x = "Mes", y = "Proporción de reservas con niños/bebés", color = "Hotel") +
  facet_wrap(~ hotel, ncol = 1, scales = "free_x") +
  theme_minimal(base_size = 12)
print(p_mes_hotel)
