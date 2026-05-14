############################################################
# PARTE MAVI - TABLAS Y GRÁFICOS PARA EL REPORTE QUARTO
############################################################

library(tidyverse)
library(gt)
library(scales)

############################################################
# 1. TABLAS PRESENTABLES
############################################################

# Tabla 1. Casos por grupo etario
tabla_edad <- casos_por_edad |>
  rename(
    `Clasificación` = CLASIFICACION_MANUAL,
    `Grupo etario` = EDAD_UC_IRAG,
    `Casos` = cantidad,
    `%` = porcentaje
  )

# Tabla 2. Casos por sexo
tabla_sexo <- casos_por_sexo |>
  rename(
    `Clasificación` = CLASIFICACION_MANUAL,
    `Sexo` = SEXO,
    `Casos` = cantidad,
    `%` = porcentaje
  )

# Tabla 3. Positividad viral por semana
tabla_positividad_viral <- positividad_viral_por_semana |>
  rename(
    `Año` = ANIO_FECHA_INTER,
    `Semana epidemiológica` = SEPI_FECHA_INTER,
    `Total de casos` = total_casos_en_se,
    `VSR positivos` = positivos_vsr,
    `Influenza positivos` = positivos_influenza,
    `SARS-CoV-2 positivos` = positivos_covid,
    `% VSR` = porcentaje_vsr,
    `% Influenza` = porcentaje_influenza,
    `% SARS-CoV-2` = porcentaje_covid
  )

# Tabla 4. Comorbilidades
tabla_comorbilidades <- frecuencia_comorbilidades |>
  rename(
    `Comorbilidad` = comorbilidad,
    `Casos` = cantidad,
    `%` = porcentaje
  )

# Tabla 5. Soporte respiratorio
tabla_soporte <- distribucion_soporte |>
  rename(
    `Soporte respiratorio` = soporte,
    `Casos` = cantidad,
    `%` = porcentaje
  )

# Tabla 6. Mortalidad
tabla_mortalidad <- resumen_fallecidos |>
  rename(
    `Total de pacientes` = total_pacientes,
    `Fallecidos` = fallecidos,
    `% fallecidos` = porcentaje_fallecidos
  )

############################################################
# 2. GRÁFICOS
############################################################

# Gráfico 1. Distribución por grupo etario
grafico_edad <- casos_por_edad |>
  ggplot(aes(x = EDAD_UC_IRAG, y = cantidad, fill = CLASIFICACION_MANUAL)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Distribución de casos por grupo etario",
    x = "Grupo etario",
    y = "Número de casos",
    fill = "Clasificación"
  ) +
  theme_minimal()

# Gráfico 2. Distribución por sexo
grafico_sexo <- casos_por_sexo |>
  ggplot(aes(x = SEXO, y = cantidad, fill = CLASIFICACION_MANUAL)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribución de casos por sexo",
    x = "Sexo",
    y = "Número de casos",
    fill = "Clasificación"
  ) +
  theme_minimal()

# Gráfico 3. Curva epidémica IRAG / IRAG extendida
grafico_curva_epidemica <- casos_irag_irage_por_semana |>
  mutate(se_anio = paste0("SE ", semana, "-", anio)) |>
  ggplot(aes(x = reorder(se_anio, semana), y = total_casos, color = evento, group = evento)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(
    title = "Casos de IRAG e IRAG extendida por semana epidemiológica",
    x = "Semana epidemiológica",
    y = "Número de casos",
    color = "Evento"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))

# Gráfico 4. Positividad viral semanal
grafico_positividad_viral <- positividad_viral_por_semana |>
  pivot_longer(
    cols = c(porcentaje_vsr, porcentaje_influenza, porcentaje_covid),
    names_to = "virus",
    values_to = "porcentaje"
  ) |>
  mutate(
    virus = recode(
      virus,
      porcentaje_vsr = "VSR",
      porcentaje_influenza = "Influenza",
      porcentaje_covid = "SARS-CoV-2"
    ),
    se_anio = paste0("SE ", SEPI_FECHA_INTER, "-", ANIO_FECHA_INTER)
  ) |>
  ggplot(aes(x = reorder(se_anio, SEPI_FECHA_INTER), y = porcentaje, color = virus, group = virus)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Porcentaje de positividad viral por semana epidemiológica",
    x = "Semana epidemiológica",
    y = "Positividad (%)",
    color = "Virus"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))

# Gráfico 5. Comorbilidades
grafico_comorbilidades <- frecuencia_comorbilidades |>
  ggplot(aes(x = reorder(comorbilidad, cantidad), y = cantidad)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Comorbilidades registradas en pacientes con IRAG/IRAG extendida",
    x = "Comorbilidad",
    y = "Número de casos"
  ) +
  theme_minimal()

# Gráfico 6. Soporte respiratorio
grafico_soporte <- distribucion_soporte |>
  ggplot(aes(x = soporte, y = cantidad)) +
  geom_col() +
  labs(
    title = "Distribución del soporte respiratorio",
    x = "Tipo de soporte",
    y = "Número de casos"
  ) +
  theme_minimal()
