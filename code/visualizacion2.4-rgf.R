library(dplyr); library(readr); library(ggplot2); library(scales)

df <- if (exists("hotel_data")) hotel_data else readr::read_csv("hotel_bookings_winsorized.csv", show_col_types = FALSE)

# Conteo y % por segmento
seg_tbl <- df %>%
  mutate(market_segment = ifelse(is.na(market_segment) | market_segment=="", "Desconocido", market_segment)) %>%
  count(market_segment, name = "reservas") %>%
  arrange(desc(reservas)) %>%
  mutate(porc = reservas / sum(reservas))

cat("\n=== Reservas por segmento de mercado ===\n")
print(seg_tbl %>% mutate(porc = percent(porc, accuracy = 0.1)))

# Top segmentos por reservas
p_seg <- ggplot(seg_tbl, aes(x = reorder(market_segment, reservas), y = reservas)) +
  geom_col() +
  coord_flip() +
  labs(title = "Reservas por segmento de mercado",
       x = "Segmento", y = "Reservas") +
  theme_minimal(base_size = 12)
print(p_seg)
