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
# Crear etiquetas de semanas
etiquetas_x <- casos_irag_irage_por_semana |>
  arrange(anio, semana) |>
  distinct(anio, semana) |>
  mutate(se_anio = paste0("SE ", semana, "-", anio))

# Gráfico curva epidémica
etiquetas_x <- casos_irag_irage_por_semana |>
  arrange(anio, semana) |>
  distinct(anio, semana) |>
  mutate(se_anio = paste0("SE ", semana, "-", anio))

grafico_curva_epidemica <- casos_irag_irage_por_semana |>
  arrange(anio, semana) |>
  mutate(
    semana_orden = dense_rank(paste(anio, semana)),
    evento = recode(
      evento,
      "Casos de IRAG entre los internados" = "IRAG",
      "Casos de IRAG extendida entre los internados" = "IRAG extendida"
    )
  ) |>
  ggplot(aes(x = semana_orden,
             y = total_casos,
             color = evento,
             group = evento)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = seq(1, nrow(etiquetas_x), by = 8),
    labels = etiquetas_x$se_anio[seq(1, nrow(etiquetas_x), by = 8)]
  ) +
  labs(
    title = "Casos de IRAG e IRAG extendida por semana epidemiológica",
    x = "Semana epidemiológica",
    y = "Número de casos",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45,
                               hjust = 1,
                               size = 8)
  )

# Gráfico 4. Positividad viral semanal
# Etiquetas para eje X
etiquetas_pos <- positividad_viral_por_semana |>
  arrange(ANIO_FECHA_INTER, SEPI_FECHA_INTER) |>
  distinct(ANIO_FECHA_INTER, SEPI_FECHA_INTER) |>
  mutate(
    semana_orden = row_number(),
    se_anio = paste0("SE ", SEPI_FECHA_INTER, "-", ANIO_FECHA_INTER)
  )

# Gráfico 4. Positividad viral semanal
grafico_positividad_viral <- positividad_viral_por_semana |>
  arrange(ANIO_FECHA_INTER, SEPI_FECHA_INTER) |>
  mutate(
    semana_orden = dense_rank(paste(ANIO_FECHA_INTER, SEPI_FECHA_INTER))
  ) |>
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
    )
  ) |>
  ggplot(aes(
    x = semana_orden,
    y = porcentaje,
    color = virus,
    group = virus
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = etiquetas_pos$semana_orden[seq(1, nrow(etiquetas_pos), by = 8)],
    labels = etiquetas_pos$se_anio[seq(1, nrow(etiquetas_pos), by = 8)]
  ) +
  labs(
    title = "Porcentaje de positividad viral por semana epidemiológica",
    x = "Semana epidemiológica",
    y = "Positividad (%)",
    color = "Virus"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

# Gráfico 5. Comorbilidades
grafico_comorbilidades <- frecuencia_comorbilidades |>
  mutate(
    comorbilidad = recode(
      comorbilidad,
      "PREMATURIDAD" = "Prematuridad",
      "ASMA" = "Asma",
      "CARDIOPATIA_CONGENITA" = "Cardiopatía congénita",
      "ENF_NEUROLOGICA_CRONICA" = "Enfermedad neurológica crónica",
      "OTRAS_COMORBILIDADES" = "Otras comorbilidades",
      "S_DOWN" = "Síndrome de Down",
      "INMUNOCOMPROMETIDO_OTRAS_CAUSAS" = "Inmunocompromiso",
      "DESNUTRICION" = "Desnutrición",
      "DBP" = "DBP",
      "VIH" = "VIH",
      "SIN_COMORBILIDADES" = "Sin comorbilidades",
      "OBESIDAD" = "Obesidad"
    )
  ) |>
  ggplot(aes(
    x = reorder(comorbilidad, cantidad),
    y = cantidad
  )) +
  geom_col(fill = "#4E79A7") +
  geom_text(
    aes(label = cantidad),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Comorbilidades registradas",
    x = NULL,
    y = "Número de casos"
  ) +
  expand_limits(y = max(frecuencia_comorbilidades$cantidad) * 1.1) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = -2
    ),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11)
  )

# Gráfico 6. Soporte respiratorio
grafico_soporte <- distribucion_soporte |>
  mutate(
    porcentaje = round(porcentaje, 1),
    etiqueta = paste0(
      porcentaje, "%\n(",
      cantidad, " casos)"
    )
  ) |>
  ggplot(aes(
    x = "",
    y = cantidad,
    fill = soporte
  )) +
  geom_col(
    width = 1,
    color = "white",
    linewidth = 1
  ) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5),
    size = 3,
    fontface = "bold",
    color = "white"
  ) +
  labs(
    title = "Distribución del soporte respiratorio",
    subtitle = "Pacientes hospitalizados con IRAG/IRAG extendida",
    fill = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Bajo flujo" = "#0072B2",
      "Alto flujo" = "#009E73",
      "Ventilación mecánica" = "#7B3294",
      "Sin soporte registrado" = "#F0C808"
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12,
      hjust = 1
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 1
    ),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_blank()
  )
