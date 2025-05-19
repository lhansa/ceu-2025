#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: libs
#| include: false

library(ggplot2)
library(ggdag)

# Create ggplot theme based on minimal with no gridlines. 
# Title is size 8 and bold, and caption is size 6.
theme_slides <- function() {
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # plot.title = element_text(face = "bold"),
      # plot.caption = element_text(size = 10),
      # plot.subtitle = element_text(size = 10),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      # axis.text = element_text(size = 8),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )
}
theme_set(theme_slides())
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: compute-sales
set.seed(31818)
n_dates <- 52 * 3
time <- 1:n_dates

stores  <- 50 + 0.8 * time

# create sales per store decreasing smoothly overtime
sales_per_store <- 200 - 0.1 * (time ** 1.2)

total_sales <- stores * sales_per_store

df1 <- data.frame(
  date = 1:n_dates,
  sales_per_store, 
  total_sales
)

# df1 |> 
#   tidyr::pivot_longer(-date) |> 
#   ggplot() + 
#   geom_line(aes(x = date, y = value)) +
#   facet_wrap(~ name, scales = "free_y", ncol = 2)

#
#
#
#| label: plot-total-sales
ggplot(df1) + 
  # geom_line(aes(x = date, y = sales_per_store), color = "#800080", linewidth = 1) +
  geom_line(aes(x = date, y = total_sales), color = "#800080", linewidth = 1) +
  scale_y_continuous(limits = c(0, max(df1$total_sales) * 1.1 )) + 
  labs(
    title = "Evolución de ventas semanales durante 3 años",
    x = "",
    y = "Ventas (unidades ficticias)",
    caption = "Los datos son inventados"
  ) 

#
#
#
#
#
#
#
#| label: plot-sales-per-store
ggplot(df1) + 
  geom_line(aes(x = date, y = sales_per_store), color = "#800080", linewidth = 1) +
  # geom_line(aes(x = date, y = total_sales), color = "#800080", linewidth = 1) +
  scale_y_continuous(limits = c(0, max(df1$sales_per_store) * 1.1 )) + 
  labs(
    title = "Evolución de ventas semanales por tienda",
    x = "",
    y = "Ventas (unidades ficticias)",
    caption = "Los datos son inventados"
  ) 
#
#
#
#
#
#
#
#
#
#
#
#
#| label: data-visits

set.seed(31818)
n_dates <- 52 * 2.5
time <- 1:n_dates

visits <- 100 + 0.8 * time + rnorm(n_dates, 0, 5)
visits <- ifelse(time > 52 * 2, visits * 1.3, visits)

df <- data.frame(
  date = 1:n_dates,
  visits
)

ggplot(df) + 
  geom_line(aes(x = date, y = visits), color = "#800080", linewidth = 1) +
  scale_y_continuous(limits = c(0, max(df$visits) * 1.1 )) + 
  labs(
    title = "Evolución de visitas semanales durante 2 años y un poco más",
    x = "",
    y = "Visitas",
    caption = "Los datos son inventados"
  )

#
#
#
#
#
#| label: data-visits-ads

campaign <- numeric(n_dates)
campaign <- ifelse(time > 52 * 2 & time < 52 * 2.2, 1, 0)

df$campaign <- campaign

ggplot(df) + 
  # add rectangle where campaign is 1
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
#
#
#
#
#
#| label: modelo1

# model with brms
fit <- lm(visits ~ date + campaign, data = df)
broom::tidy(summary(fit))

# fit <- brms::brm(visits ~ campaign, data = df)
# # extract posterior samples for campaign
# posterior <- brms::posterior_samples(fit)
# head(posterior)

# ggplot(posterior) + 
#   geom_histogram(aes(x = b_campaign), fill = "#800080")


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: data-visits-ads-corrected

mean_period_1 <- mean(df$visits[(52 * 1.9):(52 * 2)])
mean_period_2 <- mean(df$visits[(52 * 2 + 1):length(df$visits)])

correction_factor <- df$visits[52 * 2] / df$visits[52 * 2 + 1]

df$visits_corrected <- ifelse(
  df$date <= 52 * 2,
  df$visits,
  df$visits * correction_factor 
)

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

#
#
#
#
#| label: consecuencias-modelo
fit <- lm(visits_corrected ~ date + campaign, data = df)
broom::tidy(summary(fit))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: ggdag1
library(ggdag)

tidy_ggdag <- dagify(
  Madrid ~ ~Guadalajara, 
  Guadalajara ~ ~Calatayud,
  Calatayud ~ ~Zaragoza,
  Zaragoza ~ ~Lerida, 
  Lerida ~ ~Tarragona, 
  Tarragona ~ ~Prat, 
  Prat ~ ~Barcelona, 
  exposure = "Madrid", 
  outcome = "Barcelona"
) %>%
  tidy_dagitty()

ggdag(tidy_ggdag) +
  theme_dag()
#
#
#
#
#
#| label: ggdag2
tidy_ggdag <- dagify(
  Madrid ~ ~Guadalajara + Calatayud + Zaragoza + Lerida + Tarragona + Prat + Barcelona,
  Guadalajara ~ ~Calatayud + Zaragoza + Lerida + Tarragona + Prat + Barcelona,
  Calatayud ~ ~Zaragoza + Lerida + Tarragona + Prat + Barcelona, 
  Zaragoza ~ ~Lerida + Tarragona + Prat + Barcelona,
  Lerida ~ ~Tarragona + Prat + Barcelona,
  Tarragona ~ ~Prat + Barcelona,
  Prat ~ ~Barcelona,
  exposure = "Madrid", 
  outcome = "Barcelona"
) %>%
  tidy_dagitty()

ggdag(tidy_ggdag) +
  theme_dag()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: ggdag3
tidy_ggdag <- dagify(
  Madrid ~ ~Guadalajara + Calatayud + Zaragoza + Lerida + Tarragona + Prat + Barcelona,
  Barcelona ~ ~Guadalajara + Calatayud + Zaragoza + Lerida + Tarragona + Prat,
  exposure = "Madrid", 
  outcome = "Barcelona"
) %>%
  tidy_dagitty()

ggdag(tidy_ggdag) +
  theme_dag()
#
#
#
#
#| label: train-sales

df_train_sales <- readr::read_csv("data/ventas.csv", show_col_types = FALSE)

df_train_sales$fecha <- as.Date(df_train_sales$fecha, format = "%d/%m/%Y")

ggplot(df_train_sales) + 
  geom_line(aes(x = fecha, y = ventas), color = "#800080", linewidth = 1) +
  # show thousands on y axis
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "")) +
  labs(
    title = "Evolución de ventas semanales",
    x = "",
    y = "Ventas (miles de €)",
    caption = "Los datos son reales pero anónimos"
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# simulate asymetric data centered around 100
set.seed(31818)
n_clientes <- 500
avg_inc <- 50
year_diffs <- rnorm(n_clientes, avg_inc, 30)

df_customers <- data.frame(
  id = 1:n_clientes,
  year_diffs
)

ggplot(df_customers) + 
  geom_col(aes(x = reorder(id, year_diffs), y = year_diffs), fill = "#800080") + 
  labs(
    x = "", y = "Diferencia anual (unid. monetarias)", 
    title = "Variación de gasto anual por cliente",
    caption = "Los datos son inventados"
  ) + 
  coord_flip() + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
