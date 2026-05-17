############################################################
# PARTE MAVI — TABLAS Y GRÁFICOS PARA EL REPORTE QUARTO
# UC-IRAG · Hospital General de Niños "Dr. Ricardo Gutiérrez"
# Autor: Mavi
# Descripción: Este script genera todas las visualizaciones
#   (tablas con {gt} y gráficos con {ggplot2}) requeridas
#   por el reporte automatizado reporte_ucirag.qmd.
#   Depende del objeto `base`, los indicadores calculados
#   y los subconjuntos definidos en script_FINAL.R.
############################################################

# ── Paquetes ────────────────────────────────────────────
library(tidyverse)  # manipulación de datos + ggplot2
library(gt)         # tablas presentables
library(scales)     # formateo de ejes (percent, comma)
library(patchwork)  # combinar múltiples gráficos si se necesita

# ── Paleta institucional ─────────────────────────────────
# Se define una paleta única para todo el reporte,
# garantizando coherencia visual entre tablas y gráficos.
paleta_clasificacion <- c(
  "IRAG"           = "#1B6CA8",   # azul institucional
  "IRAG extendida" = "#E07B39"    # naranja complementario
)

paleta_virus <- c(
  "VSR"       = "#2E86AB",   # azul-teal
  "Influenza" = "#E84855",   # rojo
  "SARS-CoV-2"= "#6A4C93"    # violeta
)

paleta_soporte <- c(
  "Sin soporte"          = "#ADB5BD",
  "Bajo flujo"           = "#74C0FC",
  "Alto flujo"           = "#1971C2",
  "Ventilación mecánica" = "#862E9C"
)

# ── Tema base para todos los gráficos ───────────────────
# Centralizar el tema evita repetir código en cada gráfico
# y facilita cambios globales de estilo.
tema_irag <- theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle     = element_text(size = 11, color = "grey40", hjust = 0),
    plot.caption      = element_text(size = 8, color = "grey60"),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 9),
    axis.title        = element_text(size = 10),
    panel.grid.minor  = element_blank(),
    strip.text        = element_text(face = "bold")
  )


############################################################
# 1. TABLAS PRESENTABLES CON {gt}
############################################################

# ─────────────────────────────────────────────────────────
# Tabla 1. Casos por grupo etario (IRAG e IRAGe)
# Fuente: indicador 5 (casos_por_edad)
# Muestra la distribución etaria para cada clasificación.
# ─────────────────────────────────────────────────────────
tabla_edad <- casos_por_edad |>
  mutate(
    CLASIFICACION_MANUAL = recode(
      CLASIFICACION_MANUAL,
      "Infección respiratoria aguda grave (IRAG)" = "IRAG",
      "IRAG extendida"                             = "IRAG extendida"
    )
  ) |>
  rename(
    `Clasificación`  = CLASIFICACION_MANUAL,
    `Grupo etario`   = EDAD_UC_IRAG,
    `Casos (n)`      = cantidad,
    `%`              = porcentaje
  ) |>
  gt(groupname_col = "Clasificación") |>
  tab_header(
    title    = "Distribución de casos por grupo etario",
    subtitle = "IRAG e IRAG extendida · UC-IRAG HGNRG"
  ) |>
  fmt_number(columns = `Casos (n)`, decimals = 0) |>
  fmt_number(columns = `%`, decimals = 1) |>
  cols_align(align = "center", columns = c(`Casos (n)`, `%`)) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")

# ─────────────────────────────────────────────────────────
# Tabla 2. Casos por sexo
# Fuente: indicador 6 (casos_por_sexo)
# ─────────────────────────────────────────────────────────
tabla_sexo <- casos_por_sexo |>
  mutate(
    CLASIFICACION_MANUAL = recode(
      CLASIFICACION_MANUAL,
      "Infección respiratoria aguda grave (IRAG)" = "IRAG",
      "IRAG extendida"                             = "IRAG extendida"
    )
  ) |>
  rename(
    `Clasificación` = CLASIFICACION_MANUAL,
    `Sexo`          = SEXO,
    `Casos (n)`     = cantidad,
    `%`             = porcentaje
  ) |>
  gt(groupname_col = "Clasificación") |>
  tab_header(
    title    = "Distribución de casos por sexo",
    subtitle = "IRAG e IRAG extendida · UC-IRAG HGNRG"
  ) |>
  fmt_number(columns = `Casos (n)`, decimals = 0) |>
  cols_align(align = "center", columns = c(`Casos (n)`, `%`)) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")

# ─────────────────────────────────────────────────────────
# Tabla 3. Resumen de positividad viral por semana
# Fuente: indicador 7 (positividad_viral_por_semana)
# Se muestra resumida para no saturar el reporte.
# ─────────────────────────────────────────────────────────
tabla_positividad_viral <- positividad_viral_por_semana |>
  rename(
    `Año`                  = ANIO_FECHA_INTER,
    `SE`                   = SEPI_FECHA_INTER,
    `Total casos`          = total_casos_en_se,
    `VSR+`                 = positivos_vsr,
    `Influenza+`           = positivos_influenza,
    `SARS-CoV-2+`          = positivos_covid,
    `% VSR`                = porcentaje_vsr,
    `% Influenza`          = porcentaje_influenza,
    `% SARS-CoV-2`         = porcentaje_covid
  ) |>
  gt() |>
  tab_header(
    title    = "Positividad viral por semana epidemiológica",
    subtitle = "Porcentajes calculados sobre el total de casos en cada SE"
  ) |>
  fmt_number(columns = c(`Total casos`, `VSR+`, `Influenza+`, `SARS-CoV-2+`),
             decimals = 0) |>
  fmt_number(columns = c(`% VSR`, `% Influenza`, `% SARS-CoV-2`),
             decimals = 1) |>
  tab_spanner(label = "Positivos (n)", columns = c(`VSR+`, `Influenza+`, `SARS-CoV-2+`)) |>
  tab_spanner(label = "Positividad (%)", columns = c(`% VSR`, `% Influenza`, `% SARS-CoV-2`)) |>
  cols_align(align = "center", columns = -c(`Año`)) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")

# ─────────────────────────────────────────────────────────
# Tabla 4. Frecuencia de comorbilidades
# Fuente: indicador 9 (frecuencia_comorbilidades)
# Sólo pacientes con al menos una comorbilidad registrada.
# ─────────────────────────────────────────────────────────
tabla_comorbilidades <- frecuencia_comorbilidades |>
  mutate(
    comorbilidad = recode(
      comorbilidad,
      "PREMATURIDAD"                   = "Prematuridad",
      "ASMA"                           = "Asma",
      "CARDIOPATIA_CONGENITA"          = "Cardiopatía congénita",
      "ENF_NEUROLOGICA_CRONICA"        = "Enf. neurológica crónica",
      "OTRAS_COMORBILIDADES"           = "Otras comorbilidades",
      "S_DOWN"                         = "Síndrome de Down",
      "INMUNOCOMPROMETIDO_OTRAS_CAUSAS"= "Inmunocompromiso",
      "DESNUTRICION"                   = "Desnutrición",
      "DBP"                            = "Displasia broncopulmonar",
      "VIH"                            = "VIH",
      "SIN_COMORBILIDADES"             = "Sin comorbilidades",
      "OBESIDAD"                       = "Obesidad",
      "DIABETES"                       = "Diabetes"
    )
  ) |>
  rename(
    `Comorbilidad`  = comorbilidad,
    `Casos (n)`     = cantidad,
    `%`             = porcentaje
  ) |>
  gt() |>
  tab_header(
    title    = "Comorbilidades registradas",
    subtitle = "Pacientes con al menos una comorbilidad · IRAG e IRAGe"
  ) |>
  fmt_number(columns = `Casos (n)`, decimals = 0) |>
  cols_align(align = "center", columns = c(`Casos (n)`, `%`)) |>
  data_color(
    columns  = `%`,
    palette  = "Blues",
    domain   = c(0, 100)
  ) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")

# ─────────────────────────────────────────────────────────
# Tabla 5. Distribución del soporte respiratorio
# Fuente: indicador 20 (distribucion_soporte)
# ─────────────────────────────────────────────────────────
tabla_soporte <- distribucion_soporte |>
  rename(
    `Soporte respiratorio` = soporte,
    `Casos (n)`            = cantidad,
    `%`                    = porcentaje
  ) |>
  gt() |>
  tab_header(
    title    = "Distribución del soporte respiratorio",
    subtitle = "Pacientes hospitalizados con IRAG/IRAG extendida"
  ) |>
  fmt_number(columns = `Casos (n)`, decimals = 0) |>
  cols_align(align = "center", columns = c(`Casos (n)`, `%`)) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")

# ─────────────────────────────────────────────────────────
# Tabla 6. Resumen de fallecidos
# Fuente: indicadores 14, 15, 16
# ─────────────────────────────────────────────────────────
tabla_mortalidad <- bind_rows(
  resumen_fallecidos |>
    mutate(indicador = "Total fallecidos / Total pacientes") |>
    select(indicador,
           n = fallecidos,
           total = total_pacientes,
           porcentaje = porcentaje_fallecidos),
  fallecidos_con_comorbilidad |>
    mutate(indicador = "Fallecidos con comorbilidad") |>
    select(indicador,
           n = fallecidos_con_comor,
           total = total_fallecidos,
           porcentaje = porcentaje_con_comor)
) |>
  rename(
    `Indicador`   = indicador,
    `n`           = n,
    `Total`       = total,
    `%`           = porcentaje
  ) |>
  gt() |>
  tab_header(
    title    = "Resumen de mortalidad",
    subtitle = "Pacientes IRAG e IRAGe · UC-IRAG HGNRG"
  ) |>
  cols_align(align = "center", columns = -`Indicador`) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")

# ─────────────────────────────────────────────────────────
# Tabla 7 (NUEVA). Indicadores de hospitalización, UCI y
#   letalidad (resumen global de los indicadores 2, 3, 4)
# Fuente: proporcion_hospitalizaciones, proporcion_uci,
#         tasa_letalidad (base agrupada)
# ─────────────────────────────────────────────────────────
# Se construye una tabla resumen que combina los tres
# indicadores calculados desde la base agrupada.
tabla_indicadores_globales <- tibble(
  `Indicador` = c(
    "Proporción de hospitalizaciones IRAG/IRAGe (%)",
    "Proporción IRAG/IRAGe en UCI (%)",
    "Tasa de letalidad IRAG/IRAGe (%)"
  ),
  `Mediana (%)` = c(
    median(proporcion_hospitalizaciones$porcentaje_hospitalizaciones, na.rm = TRUE),
    median(proporcion_uci$porcentaje_irag_en_uci, na.rm = TRUE),
    median(tasa_letalidad$tasa_letalidad_pct, na.rm = TRUE)
  ),
  `Mínimo (%)` = c(
    min(proporcion_hospitalizaciones$porcentaje_hospitalizaciones, na.rm = TRUE),
    min(proporcion_uci$porcentaje_irag_en_uci, na.rm = TRUE),
    min(tasa_letalidad$tasa_letalidad_pct, na.rm = TRUE)
  ),
  `Máximo (%)` = c(
    max(proporcion_hospitalizaciones$porcentaje_hospitalizaciones, na.rm = TRUE),
    max(proporcion_uci$porcentaje_irag_en_uci, na.rm = TRUE),
    max(tasa_letalidad$tasa_letalidad_pct, na.rm = TRUE)
  )
) |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) |>
  gt() |>
  tab_header(
    title    = "Indicadores de hospitalización, UCI y letalidad",
    subtitle = "Resumen del período SE 20/2024 – SE 5/2026"
  ) |>
  cols_align(align = "center", columns = -`Indicador`) |>
  tab_source_note("Fuente: Carga agrupada UC-IRAG HGNRG")

# ─────────────────────────────────────────────────────────
# Tabla 8 (NUEVA). Vacunación
# Fuente: indicadores 10, 11
# Resume las coberturas de vacunación antigripal y VSR
# para los distintos grupos (≥6 m, <6 m, materna).
# ─────────────────────────────────────────────────────────
tabla_vacunacion <- tibble(
  `Grupo`      = c(
    "Antigripal (≥ 6 meses)",
    "Antigripal materna (< 6 meses)",
    "VSR materna (< 6 meses)"
  ),
  `Con vacunación (n)` = c(
    vacunacion_gripe_mayores$vacunados,
    vacunacion_gripe_materna$madres_vacunadas,
    vacunacion_vsr_materna$madre_vacunada_vsr
  ),
  `Con dato (n)` = c(
    vacunacion_gripe_mayores$con_dato_vacunacion,
    vacunacion_gripe_materna$con_dato_vac_materna,
    vacunacion_vsr_materna$con_dato_vsr_materna
  ),
  `Cobertura (%)` = c(
    vacunacion_gripe_mayores$porcentaje_vacunados,
    vacunacion_gripe_materna$porcentaje_vac_materna,
    vacunacion_vsr_materna$porcentaje_vacunadas
  )
) |>
  gt() |>
  tab_header(
    title    = "Cobertura de vacunación e inmunización",
    subtitle = "Pacientes IRAG/IRAGe · UC-IRAG HGNRG"
  ) |>
  fmt_number(columns = c(`Con vacunación (n)`, `Con dato (n)`), decimals = 0) |>
  cols_align(align = "center", columns = -`Grupo`) |>
  data_color(
    columns = `Cobertura (%)`,
    palette = c("#fee8c8", "#e34a33"),
    domain  = c(0, 100)
  ) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")

# ─────────────────────────────────────────────────────────
# Tabla 9 (NUEVA). Completitud de variables clave
# Fuente: objeto `base`
# Evalúa la proporción de faltantes (NA + código 9) en las
# variables más relevantes para el reporte.
# ─────────────────────────────────────────────────────────
vars_calidad <- c(
  "SEXO", "EDAD_UC_IRAG", "SEPI_FECHA_INTER",
  "CUIDADO_INTENSIVO", "FALLECIDO",
  "VSR_FINAL", "INFLUENZA_FINAL", "COVID_19_FINAL",
  "PRESENCIA_COMORBILIDADES",
  "VAC_ANTIGRIPAL", "VAC_ANTIGRIPAL_MATERNA", "VAC_VSR",
  "OSELTAMIVIR", "OXIGENOTERAPIA_BAJO_FLUJO",
  "OXIGENOTERAPIA_ALTO_FLUJO", "VM"
)

tabla_completitud <- map_dfr(vars_calidad, function(v) {
  if (!v %in% names(base)) return(NULL)
  total   <- nrow(base)
  n_na    <- sum(is.na(base[[v]]))
  # código 9 como sin dato (aplica a columnas numéricas/character)
  n_cod9  <- sum(base[[v]] == "9", na.rm = TRUE)
  faltante <- n_na + n_cod9
  tibble(
    Variable           = v,
    `Total (n)`        = total,
    `Faltantes (n)`    = faltante,
    `Completitud (%)`  = round((1 - faltante / total) * 100, 1)
  )
}) |>
  arrange(`Completitud (%)`) |>
  gt() |>
  tab_header(
    title    = "Completitud de variables clave",
    subtitle = "NA + código 9 considerados como sin dato"
  ) |>
  fmt_number(columns = c(`Total (n)`, `Faltantes (n)`), decimals = 0) |>
  cols_align(align = "center", columns = -Variable) |>
  data_color(
    columns = `Completitud (%)`,
    palette = c("#d73027", "#fee090", "#4dac26"),
    domain  = c(0, 100)
  ) |>
  tab_source_note("Fuente: SNVS 2.0 · base depurada")


############################################################
# 2. GRÁFICOS
############################################################

# ─────────────────────────────────────────────────────────
# Auxiliar: etiqueta combinada SE-año para eje X temporal.
# Se reutiliza en todos los gráficos de series temporales.
# ─────────────────────────────────────────────────────────
crear_orden_semanas <- function(df, anio_col, semana_col) {
  # Devuelve el df con una columna `semana_orden` (entero
  # correlativo) y `se_label` (texto "SE XX-AAAA").
  df |>
    arrange({{ anio_col }}, {{ semana_col }}) |>
    mutate(
      semana_orden = dense_rank(paste({{ anio_col }}, {{ semana_col }})),
      se_label     = paste0("SE ", {{ semana_col }}, "-", {{ anio_col }})
    )
}

# Helper para breaks del eje X: cada N semanas muestra etiqueta.
breaks_eje_x <- function(df, cada_n = 8) {
  filas_unicas <- df |>
    distinct(semana_orden, se_label) |>
    arrange(semana_orden)
  idx <- seq(1, nrow(filas_unicas), by = cada_n)
  list(
    breaks = filas_unicas$semana_orden[idx],
    labels = filas_unicas$se_label[idx]
  )
}

# ─────────────────────────────────────────────────────────
# Gráfico 1. Distribución por grupo etario (mejorado)
# Fuente: indicador 5 (casos_por_edad)
# Se invierte el eje para legibilidad, se agrega % como
# texto dentro de las barras, y se usan colores de paleta.
# ─────────────────────────────────────────────────────────
grafico_edad <- casos_por_edad |>
  mutate(
    clasificacion = recode(
      CLASIFICACION_MANUAL,
      "Infección respiratoria aguda grave (IRAG)" = "IRAG",
      "IRAG extendida"                             = "IRAG extendida"
    ),
    etiqueta = paste0(porcentaje, "%")
  ) |>
  ggplot(aes(
    x    = EDAD_UC_IRAG,
    y    = cantidad,
    fill = clasificacion
  )) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(
    aes(label = etiqueta),
    position = position_dodge(width = 0.7),
    hjust    = -0.1,
    size     = 3
  ) +
  coord_flip() +
  scale_fill_manual(values = paleta_clasificacion) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Distribución de casos por grupo etario",
    subtitle = "IRAG e IRAG extendida · SE 20/2024 – SE 5/2026",
    x        = NULL,
    y        = "Número de casos",
    caption  = "Fuente: SNVS 2.0"
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ─────────────────────────────────────────────────────────
# Gráfico 2. Distribución por sexo (mejorado)
# Fuente: indicador 6 (casos_por_sexo)
# Se agrega porcentaje como etiqueta y se usa la paleta.
# ─────────────────────────────────────────────────────────
grafico_sexo <- casos_por_sexo |>
  mutate(
    clasificacion = recode(
      CLASIFICACION_MANUAL,
      "Infección respiratoria aguda grave (IRAG)" = "IRAG",
      "IRAG extendida"                             = "IRAG extendida"
    ),
    etiqueta = paste0(porcentaje, "%")
  ) |>
  ggplot(aes(
    x    = SEXO,
    y    = cantidad,
    fill = clasificacion
  )) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(
    aes(label = etiqueta),
    position = position_dodge(width = 0.6),
    vjust    = -0.4,
    size     = 3.5
  ) +
  scale_fill_manual(values = paleta_clasificacion) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Distribución de casos por sexo",
    subtitle = "IRAG e IRAG extendida · SE 20/2024 – SE 5/2026",
    x        = NULL,
    y        = "Número de casos",
    caption  = "Fuente: SNVS 2.0"
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ─────────────────────────────────────────────────────────
# Gráfico 3. Curva epidémica IRAG e IRAG extendida (mejorado)
# Fuente: indicador 1 (casos_irag_irage_por_semana)
# El eje X usa semana_orden (numérico) para evitar saltos;
# se etiquetan cada 8 semanas para no saturar.
# ─────────────────────────────────────────────────────────
datos_curva <- casos_irag_irage_por_semana |>
  mutate(
    evento = recode(
      evento,
      "Casos de IRAG entre los internados"          = "IRAG",
      "Casos de IRAG extendida entre los internados" = "IRAG extendida"
    )
  ) |>
  crear_orden_semanas(anio, semana)

eje_curva <- breaks_eje_x(datos_curva, cada_n = 8)

grafico_curva_epidemica <- datos_curva |>
  ggplot(aes(
    x      = semana_orden,
    y      = total_casos,
    color  = evento,
    group  = evento
  )) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = paleta_clasificacion) +
  scale_x_continuous(
    breaks = eje_curva$breaks,
    labels = eje_curva$labels
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Curva epidémica — Casos de IRAG e IRAG extendida",
    subtitle = "Por semana epidemiológica de internación",
    x        = "Semana epidemiológica",
    y        = "Número de casos",
    caption  = "Fuente: Carga agrupada UC-IRAG HGNRG"
  ) +
  tema_irag

# ─────────────────────────────────────────────────────────
# Gráfico 4. Positividad viral semanal (mejorado)
# Fuente: indicador 7 (positividad_viral_por_semana)
# Usa pivot_longer en formato tidy + paleta de virus.
# Se agregan bandas sombreadas por año para orientación.
# ─────────────────────────────────────────────────────────
datos_positividad <- positividad_viral_por_semana |>
  crear_orden_semanas(ANIO_FECHA_INTER, SEPI_FECHA_INTER) |>
  pivot_longer(
    cols      = c(porcentaje_vsr, porcentaje_influenza, porcentaje_covid),
    names_to  = "virus",
    values_to = "porcentaje"
  ) |>
  mutate(
    virus = recode(
      virus,
      porcentaje_vsr       = "VSR",
      porcentaje_influenza = "Influenza",
      porcentaje_covid     = "SARS-CoV-2"
    )
  )

eje_positividad <- breaks_eje_x(datos_positividad, cada_n = 8)

grafico_positividad_viral <- datos_positividad |>
  ggplot(aes(
    x      = semana_orden,
    y      = porcentaje,
    color  = virus,
    group  = virus
  )) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = paleta_virus) +
  scale_x_continuous(
    breaks = eje_positividad$breaks,
    labels = eje_positividad$labels
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Positividad viral por semana epidemiológica",
    subtitle = "VSR, Influenza y SARS-CoV-2 · pacientes IRAG/IRAGe",
    x        = "Semana epidemiológica",
    y        = "Positividad (%)",
    caption  = "Fuente: SNVS 2.0"
  ) +
  tema_irag

# ─────────────────────────────────────────────────────────
# Gráfico 5. Comorbilidades (mejorado)
# Fuente: indicador 9 (frecuencia_comorbilidades)
# Se ordenan de mayor a menor, etiquetas más limpias y
# se usa un degradado de color proporcional a la frecuencia.
# ─────────────────────────────────────────────────────────
grafico_comorbilidades <- frecuencia_comorbilidades |>
  mutate(
    comorbilidad = recode(
      comorbilidad,
      "PREMATURIDAD"                    = "Prematuridad",
      "ASMA"                            = "Asma",
      "CARDIOPATIA_CONGENITA"           = "Cardiopatía congénita",
      "ENF_NEUROLOGICA_CRONICA"         = "Enf. neurológica crónica",
      "OTRAS_COMORBILIDADES"            = "Otras",
      "S_DOWN"                          = "Síndrome de Down",
      "INMUNOCOMPROMETIDO_OTRAS_CAUSAS" = "Inmunocompromiso",
      "DESNUTRICION"                    = "Desnutrición",
      "DBP"                             = "Displasia broncopulmonar",
      "VIH"                             = "VIH",
      "SIN_COMORBILIDADES"              = "Sin comorbilidades",
      "OBESIDAD"                        = "Obesidad",
      "DIABETES"                        = "Diabetes"
    ),
    comorbilidad = reorder(comorbilidad, cantidad)
  ) |>
  ggplot(aes(
    x    = comorbilidad,
    y    = cantidad,
    fill = cantidad
  )) +
  geom_col() +
  geom_text(
    aes(label = paste0(cantidad, " (", porcentaje, "%)")),
    hjust = -0.1,
    size  = 3.2
  ) +
  coord_flip() +
  scale_fill_gradient(low = "#BDD7EE", high = "#1B6CA8", guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Frecuencia de comorbilidades registradas",
    subtitle = "Entre pacientes IRAG/IRAGe con al menos una comorbilidad",
    x        = NULL,
    y        = "Número de casos",
    caption  = "Fuente: SNVS 2.0"
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ─────────────────────────────────────────────────────────
# Gráfico 6. Soporte respiratorio — gráfico de barras apiladas
# Fuente: indicador 20 (distribucion_soporte)
# ─────────────────────────────────────────────────────────
grafico_soporte <- distribucion_soporte |>
  mutate(
    soporte  = factor(soporte,
                      levels = c("Sin soporte", "Bajo flujo",
                                 "Alto flujo", "Ventilación mecánica")),
    etiqueta = paste0(porcentaje, "%\n(n=", cantidad, ")")
  ) |>
  ggplot(aes(
    x    = "",
    y    = cantidad,
    fill = soporte
  )) +
  geom_col(width = 0.5, color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = etiqueta),
    position  = position_stack(vjust = 0.5),
    size      = 3.2,
    color     = "white",
    fontface  = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = paleta_soporte) +
  labs(
    title    = "Distribución del soporte respiratorio",
    subtitle = "Pacientes hospitalizados con IRAG/IRAG extendida",
    x        = NULL,
    y        = "Número de casos",
    fill     = NULL,
    caption  = "Fuente: SNVS 2.0"
  ) +
  tema_irag +
  guides(fill = guide_legend(nrow = 2))

# ─────────────────────────────────────────────────────────
# Gráfico 7. Proporción semanal de hospitalizaciones
# Fuente: indicador 2 (proporcion_hospitalizaciones)
# Muestra qué % de los internados por todas las causas
# corresponde a IRAG/IRAGe, semana a semana.
# ─────────────────────────────────────────────────────────
datos_hosp <- proporcion_hospitalizaciones |>
  crear_orden_semanas(anio, semana)

eje_hosp <- breaks_eje_x(datos_hosp, cada_n = 8)

grafico_hospitalizaciones <- datos_hosp |>
  ggplot(aes(x = semana_orden, y = porcentaje_hospitalizaciones)) +
  geom_col(fill = "#1B6CA8", alpha = 0.85) +
  geom_smooth(
    method  = "loess",
    se      = FALSE,
    color   = "#E07B39",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = eje_hosp$breaks,
    labels = eje_hosp$labels
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title    = "Proporción de hospitalizaciones IRAG/IRAGe",
    subtitle = "Sobre el total de internados por todas las causas",
    x        = "Semana epidemiológica",
    y        = "Porcentaje (%)",
    caption  = "Fuente: Carga agrupada UC-IRAG HGNRG · línea naranja = tendencia (loess)"
  ) +
  tema_irag

# ─────────────────────────────────────────────────────────
# Gráfico 8. Proporción de IRAG/IRAGe en UCI
# Fuente: indicador 3 (proporcion_uci)
# Muestra qué % de los ingresos a UCI correspondió a IRAG/IRAGe.
# ─────────────────────────────────────────────────────────
datos_uci <- proporcion_uci |>
  crear_orden_semanas(anio, semana)

eje_uci <- breaks_eje_x(datos_uci, cada_n = 8)

grafico_uci <- datos_uci |>
  ggplot(aes(x = semana_orden, y = porcentaje_irag_en_uci)) +
  geom_col(fill = "#862E9C", alpha = 0.8) +
  geom_smooth(
    method    = "loess",
    se        = FALSE,
    color     = "#E07B39",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = eje_uci$breaks,
    labels = eje_uci$labels
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title    = "Proporción de IRAG/IRAGe entre ingresos a UCI",
    subtitle = "Sobre el total de ingresos a UCI por todas las causas",
    x        = "Semana epidemiológica",
    y        = "Porcentaje (%)",
    caption  = "Fuente: Carga agrupada UC-IRAG HGNRG · línea naranja = tendencia (loess)"
  ) +
  tema_irag

# ─────────────────────────────────────────────────────────
# Gráfico 9. Tasa de letalidad semanal
# Fuente: indicador 4 (tasa_letalidad)
# Muestra la evolución semanal de la tasa de letalidad.
# Se excluyen semanas con 0 internados para evitar extremos.
# ─────────────────────────────────────────────────────────
datos_letalidad <- tasa_letalidad |>
  filter(!is.na(tasa_letalidad_pct)) |>
  crear_orden_semanas(anio, semana)

eje_letalidad <- breaks_eje_x(datos_letalidad, cada_n = 8)

grafico_letalidad <- datos_letalidad |>
  ggplot(aes(x = semana_orden, y = tasa_letalidad_pct)) +
  geom_col(fill = "#C0392B", alpha = 0.7) +
  scale_x_continuous(
    breaks = eje_letalidad$breaks,
    labels = eje_letalidad$labels
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title    = "Tasa de letalidad — IRAG/IRAG extendida",
    subtitle = "Defunciones / internados × 100, por semana epidemiológica",
    x        = "Semana epidemiológica",
    y        = "Tasa de letalidad (%)",
    caption  = "Fuente: Carga agrupada UC-IRAG HGNRG"
  ) +
  tema_irag

# ─────────────────────────────────────────────────────────
# Gráfico 10. Distribución viral por grupo etario
# Fuente: objeto `base` (base nominal depurada)
# Compara la positividad de VSR, Influenza y SARS-CoV-2
# según grupo etario — útil para identificar grupos vulnerables.
# ─────────────────────────────────────────────────────────
distribucion_viral_edad <- base |>
  filter(!is.na(EDAD_UC_IRAG)) |>
  mutate(
    positivo_vsr       = !is.na(VSR_FINAL) &
      !as.character(VSR_FINAL) %in% c("Negativo", "Sin resultado"),
    positivo_influenza = !is.na(INFLUENZA_FINAL) &
      !as.character(INFLUENZA_FINAL) %in% c("Negativo", "Sin resultado"),
    positivo_covid     = as.character(COVID_19_FINAL) == "Positivo"
  ) |>
  group_by(EDAD_UC_IRAG) |>
  summarise(
    total              = n(),
    VSR                = sum(positivo_vsr,       na.rm = TRUE),
    Influenza          = sum(positivo_influenza,  na.rm = TRUE),
    `SARS-CoV-2`       = sum(positivo_covid,      na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols      = c(VSR, Influenza, `SARS-CoV-2`),
    names_to  = "virus",
    values_to = "n_positivos"
  ) |>
  mutate(porcentaje = round(n_positivos / total * 100, 1))

grafico_viral_edad <- distribucion_viral_edad |>
  ggplot(aes(
    x    = EDAD_UC_IRAG,
    y    = porcentaje,
    fill = virus
  )) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = paleta_virus) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  coord_flip() +
  labs(
    title    = "Positividad viral por grupo etario",
    subtitle = "% de casos positivos sobre total en cada grupo · IRAG/IRAGe",
    x        = NULL,
    y        = "Positividad (%)",
    fill     = NULL,
    caption  = "Fuente: SNVS 2.0"
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ─────────────────────────────────────────────────────────
# Gráfico 11. Vacunación — comparación de coberturas
# Fuente: indicadores 10, 11
# Muestra en una sola vista las tres coberturas.
# ─────────────────────────────────────────────────────────
datos_vacunacion_graf <- tibble(
  grupo    = c(
    "Antigripal (≥ 6 m)",
    "Antigripal materna (< 6 m)",
    "VSR materna (< 6 m)"
  ),
  cobertura = c(
    vacunacion_gripe_mayores$porcentaje_vacunados,
    vacunacion_gripe_materna$porcentaje_vac_materna,
    vacunacion_vsr_materna$porcentaje_vacunadas
  )
)

grafico_vacunacion <- datos_vacunacion_graf |>
  ggplot(aes(x = reorder(grupo, cobertura), y = cobertura, fill = grupo)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(cobertura, "%")),
    hjust  = -0.2,
    size   = 4,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("#1B6CA8", "#2E86AB", "#48CAE4")) +
  scale_y_continuous(
    limits = c(0, 110),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Coberturas de vacunación e inmunización",
    subtitle = "Pacientes IRAG/IRAGe según grupo etario",
    x        = NULL,
    y        = "Cobertura (%)",
    caption  = "Fuente: SNVS 2.0 · entre quienes tienen dato registrado"
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ─────────────────────────────────────────────────────────
# Gráfico 12. Completitud de variables clave
# Fuente: objeto `base`
# Gráfico de barras horizontales mostrando el % de completitud
# de cada variable para facilitar la evaluación de calidad.
# ─────────────────────────────────────────────────────────
datos_completitud <- map_dfr(vars_calidad, function(v) {
  if (!v %in% names(base)) return(NULL)
  total   <- nrow(base)
  n_na    <- sum(is.na(base[[v]]))
  n_cod9  <- sum(base[[v]] == "9", na.rm = TRUE)
  faltante <- n_na + n_cod9
  tibble(
    variable         = v,
    completitud_pct  = round((1 - faltante / total) * 100, 1)
  )
}) |>
  mutate(
    color_barra = case_when(
      completitud_pct >= 90 ~ "Alta (≥90%)",
      completitud_pct >= 70 ~ "Media (70–89%)",
      TRUE                   ~ "Baja (<70%)"
    ),
    color_barra = factor(
      color_barra,
      levels = c("Alta (≥90%)", "Media (70–89%)", "Baja (<70%)")
    )
  )

grafico_completitud <- datos_completitud |>
  ggplot(aes(
    x    = reorder(variable, completitud_pct),
    y    = completitud_pct,
    fill = color_barra
  )) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(completitud_pct, "%")),
    hjust = -0.1,
    size  = 3.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Alta (≥90%)"    = "#4dac26",
      "Media (70–89%)" = "#f4a261",
      "Baja (<70%)"    = "#d62728"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Completitud de variables clave",
    subtitle = "Porcentaje de registros con dato válido (excluye NA y código 9)",
    x        = NULL,
    y        = "Completitud (%)",
    fill     = NULL,
    caption  = "Fuente: SNVS 2.0 · base depurada"
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


############################################################
# 3. IMPRESIÓN DE OBJETOS
#    Para usar en el .qmd: basta llamar el nombre del objeto
#    dentro de un chunk de R. Ejemplos:
#
#    ```{r}
#    tabla_edad
#    ```
#    ```{r}
#    grafico_curva_epidemica
#    ```
############################################################

# Lista de todos los objetos disponibles para el reporte:
cat("
══════════════════════════════════════════════════
  OBJETOS DE VISUALIZACIÓN DISPONIBLES
══════════════════════════════════════════════════

TABLAS ({gt}):
  tabla_edad                  → Casos por grupo etario
  tabla_sexo                  → Casos por sexo
  tabla_positividad_viral     → Positividad viral semanal
  tabla_comorbilidades        → Frecuencia de comorbilidades
  tabla_soporte               → Soporte respiratorio
  tabla_mortalidad            → Resumen de fallecidos
  tabla_indicadores_globales  → Hospit. / UCI / Letalidad [NUEVA]
  tabla_vacunacion            → Coberturas de vacunación [NUEVA]
  tabla_completitud           → Completitud de variables [NUEVA]

GRÁFICOS ({ggplot2}):
  grafico_edad                → Distribución por grupo etario
  grafico_sexo                → Distribución por sexo
  grafico_curva_epidemica     → Curva epidémica IRAG/IRAGe
  grafico_positividad_viral   → Positividad viral semanal
  grafico_comorbilidades      → Frecuencia de comorbilidades
  grafico_soporte             → Soporte respiratorio (barras)
  grafico_hospitalizaciones   → % Hospit. IRAG/IRAGe [NUEVA]
  grafico_uci                 → % IRAG/IRAGe en UCI [NUEVA]
  grafico_letalidad           → Tasa de letalidad [NUEVA]
  grafico_viral_edad          → Positividad viral × edad [NUEVA]
  grafico_vacunacion          → Coberturas vacunación [NUEVA]
  grafico_completitud         → Completitud variables [NUEVA]

══════════════════════════════════════════════════
")
