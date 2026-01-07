install.packages(c("rnaturalearth", "rnaturalearthdata", "sf"))

library(tidyverse)
library(readr)
library(ggplot2)
library(ggthemes)
library(gt)
library(gtExtras)
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

ruta_education <- "./datos/data_education.csv"

data_education <- rio::import(ruta_education)

str(data_education)


head(data_education)
colnames(data_education)


ruta_gdp <- "./datos/data_gdp.csv"

data_gdp <- rio::import(ruta_gdp)

str(data_gdp)



head(data_gdp)
colnames(data_gdp)



#ARREGLAR:
data_education <- janitor::clean_names(data_education)

education_clean <- data_education |> 
  select(iso3c = ref_area, anio = time_period, tasa_educacion = obs_value) |> 
  mutate(
    anio = as.numeric(anio), 
    tasa_educacion = as.numeric(tasa_educacion), 
    pais = countrycode(iso3c, origin = "iso3c", destination = "un.name.es")) |> 
  filter(!is.na(tasa_educacion), !is.na(pais)) |> 
  arrange(pais, anio)
  

# HACER GRÁFICO POR AÑO - PAIS

paises_interes <- c("España", "Francia", "Alemania", "Italia")

education_clean |> 
  filter(pais %in% paises_interes) |> 
  ggplot(aes(x = anio, y = tasa_educacion, color = pais)) +
  geom_line() + 
  geom_point(size = 2) +       
  theme_minimal() +            
  labs(
    title = "Evolución de la formación superior en Europa",
    subtitle = "Porcentaje de población adulta (25-64) con estudios terciarios",
    y = "Porcentaje (%)",
    x = "Año",
    color = "País"
  ) +
  theme(legend.position = "bottom")


#TABLAS:
#Primero calculo la evolución:
ranking_evolucion <- education_clean |>
  group_by(pais) |>
  summarise(
    anio_inicio = min(anio),
    anio_fin = max(anio),
    valor_inicial = first(tasa_educacion, order_by = anio),
    valor_final = last(tasa_educacion, order_by = anio)
  ) |>
  mutate(
    crecimiento_total = valor_final - valor_inicial,
    periodo = paste0(anio_inicio, "-", anio_fin)
  ) |>
  ungroup()

ranking_evolucion <- ranking_evolucion |> filter(!is.na(crecimiento_total))  

#Luego hago la tabla:

# TABLA 1: TOP 3 MAYOR CRECIMIENTO
tabla_top <- ranking_evolucion |>
  arrange(desc(crecimiento_total)) |> 
  head(3) |>                         
  select(pais, periodo, valor_inicial, valor_final, crecimiento_total) |>
  gt() |>
  tab_header(
    title = md("**Líderes en la Revolución Educativa**"),
    subtitle = "Países con mayor crecimiento en población con educación superior"
  ) |>
  fmt_number(
    columns = c(valor_inicial, valor_final, crecimiento_total),
    decimals = 1
  ) |>
  cols_label(
    pais = "País",
    periodo = "Periodo Analizado",
    valor_inicial = "% Inicial",
    valor_final = "% Actual (Total)",
    crecimiento_total = "Crecimiento (+)"
  ) |>
  data_color(
    columns = crecimiento_total,
    method = "numeric",
    palette = c("lightgreen", "forestgreen")
  )

print(tabla_top)

#Segunda tabla para el menor creciiento:

tabla_bottom <- ranking_evolucion |>
  arrange(crecimiento_total) |> #
  head(3) |>                    
  select(pais, periodo, valor_inicial, valor_final, crecimiento_total) |>
  gt() |>
  tab_header(
    title = md("**Evolución Educativa Estancada**"),
    subtitle = "Países con el menor crecimiento en el periodo"
  ) |>
  fmt_number(
    columns = c(valor_inicial, valor_final, crecimiento_total),
    decimals = 1
  ) |>
  cols_label(
    pais = "País",
    periodo = "Periodo Analizado",
    valor_inicial = "% Inicial",
    valor_final = "% Actual (Total)",
    crecimiento_total = "Crecimiento"
  ) |>
  data_color(
    columns = crecimiento_total,
    method = "numeric",
    palette = c("orange", "red"),
    reverse = TRUE )


print(tabla_bottom)

gtsave(tabla_top, "tabla_top_educacion.png")

