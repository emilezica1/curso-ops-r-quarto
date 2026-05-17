############################################################
# BLOQUE ADICIONAL — INFLUENZA POR SUBTIPO Y OSELTAMIVIR
# UC-IRAG · Hospital General de Niños "Dr. Ricardo Gutiérrez"
# Autor: Mavi
# Descripción: Visualizaciones de circulación de Influenza
#   según subtipo/linaje y registro de uso de oseltamivir,
#   incluyendo el análisis por resultado virológico y grupo
#   etario.
#   Depende del objeto `base` y `pacientes_irag_o_irage`
#   definidos en script_FINAL.R, y del tema_irag definido
#   en mavi_visualizaciones_FINAL.R.
#
# LIMITACIÓN DE DATO — OSELTAMIVIR:
#   La variable sólo codifica presencia confirmada (1).
#   El NA no distingue "no se administró" de "no se registró".
#   Por eso todos los análisis de oseltamivir usan la
#   categoría "Con registro de administración" vs
#   "Sin registro", y nunca "No recibió".
############################################################

############################################################
# A. DATOS INTERMEDIOS
############################################################

# ─────────────────────────────────────────────────────────
# Paleta de colores para subtipos de Influenza.
# Azules para Tipo A (de oscuro a claro según subtipo),
# verdes para Tipo B.
# ─────────────────────────────────────────────────────────
paleta_influenza <- c(
  "Influenza A H1N1"             = "#1565C0",
  "Influenza A H3N2"             = "#42A5F5",
  "Influenza A sin subtipificar" = "#90CAF9",
  "Influenza B Victoria"         = "#2E7D32",
  "Influenza B sin linaje"       = "#A5D6A7"
)

# ─────────────────────────────────────────────────────────
# Subconjunto: sólo casos con Influenza positiva.
# Se excluyen "Negativo" y "Sin resultado" para los
# análisis de distribución y evolución por subtipo.
# Los niveles de INFLUENZA_FINAL (factor creado en
# script_FINAL.R sección 7.5) son:
#   "Influenza A H1N1", "Influenza A H3N2",
#   "Influenza A sin subtipificar",
#   "Influenza B Victoria", "Influenza B sin linaje",
#   "Negativo", "Sin resultado"
# ─────────────────────────────────────────────────────────
casos_influenza_positivos <- pacientes_irag_o_irage |>
  filter(
    !is.na(INFLUENZA_FINAL),
    !as.character(INFLUENZA_FINAL) %in% c("Negativo", "Sin resultado")
  )

# ─────────────────────────────────────────────────────────
# Base para análisis de oseltamivir.
# Se construyen dos variables categóricas sobre el total
# de pacientes IRAG/IRAGe:
#
#   oseltamivir_cat:
#     "Con registro de administración" → OSELTAMIVIR == "1"
#     "Sin registro"                   → OSELTAMIVIR == NA
#     (NA no distingue no-administrado de dato faltante)
#
#   resultado_influenza_cat:
#     "Influenza positiva"   → cualquier subtipo confirmado
#     "Influenza negativa"   → resultado Negativo
#     "Sin resultado/dato"   → Sin resultado o NA
# ─────────────────────────────────────────────────────────
base_oseltamivir <- pacientes_irag_o_irage |>
  mutate(
    oseltamivir_cat = case_when(
      OSELTAMIVIR == "1" ~ "Con registro de administración",
      is.na(OSELTAMIVIR) ~ "Sin registro"
    ),
    oseltamivir_cat = factor(
      oseltamivir_cat,
      levels = c("Con registro de administración", "Sin registro")
    ),
    resultado_influenza_cat = case_when(
      !is.na(INFLUENZA_FINAL) &
        !as.character(INFLUENZA_FINAL) %in%
        c("Negativo", "Sin resultado")            ~ "Influenza positiva",
      as.character(INFLUENZA_FINAL) == "Negativo" ~ "Influenza negativa",
      TRUE                                        ~ "Sin resultado/dato"
    ),
    resultado_influenza_cat = factor(
      resultado_influenza_cat,
      levels = c("Influenza positiva", "Influenza negativa", "Sin resultado/dato")
    )
  )

# ─────────────────────────────────────────────────────────
# Cruce: resultado virológico × registro de oseltamivir.
# Se incluyen TODOS los registros (con y sin dato de
# administración) para mostrar la distribución completa
# dentro de cada grupo virológico.
# ─────────────────────────────────────────────────────────
cruce_oseltamivir_influenza <- base_oseltamivir |>
  count(resultado_influenza_cat, oseltamivir_cat, name = "n") |>
  group_by(resultado_influenza_cat) |>
  mutate(
    total_grupo = sum(n),
    porcentaje  = round(n / total_grupo * 100, 1)
  ) |>
  ungroup()

# ─────────────────────────────────────────────────────────
# Registro de oseltamivir por grupo etario.
# % calculado sobre el total de cada grupo etario
# (con y sin registro).
# ─────────────────────────────────────────────────────────
oseltamivir_por_edad <- base_oseltamivir |>
  filter(!is.na(EDAD_UC_IRAG)) |>
  count(EDAD_UC_IRAG, oseltamivir_cat, name = "n") |>
  group_by(EDAD_UC_IRAG) |>
  mutate(
    total_grupo = sum(n),
    porcentaje  = round(n / total_grupo * 100, 1)
  ) |>
  ungroup()

# ─────────────────────────────────────────────────────────
# Conteos semanales de Influenza positiva por subtipo.
# Base para la curva de evolución temporal.
# ─────────────────────────────────────────────────────────
influenza_por_semana_subtipo <- pacientes_irag_o_irage |>
  filter(
    !is.na(SEPI_FECHA_INTER),
    !is.na(INFLUENZA_FINAL),
    !as.character(INFLUENZA_FINAL) %in% c("Negativo", "Sin resultado")
  ) |>
  count(ANIO_FECHA_INTER, SEPI_FECHA_INTER, INFLUENZA_FINAL, name = "n") |>
  arrange(ANIO_FECHA_INTER, SEPI_FECHA_INTER) |>
  mutate(
    semana_orden = dense_rank(paste(ANIO_FECHA_INTER, SEPI_FECHA_INTER)),
    se_label     = paste0("SE ", SEPI_FECHA_INTER, "-", ANIO_FECHA_INTER)
  )

# ─────────────────────────────────────────────────────────
# Resumen de distribución por subtipo (para tabla y gráfico
# de barras).
# ─────────────────────────────────────────────────────────
resumen_subtipos_influenza <- casos_influenza_positivos |>
  count(INFLUENZA_FINAL, name = "n") |>
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    tipo       = if_else(
      str_starts(as.character(INFLUENZA_FINAL), "Influenza A"),
      "Tipo A", "Tipo B"
    )
  ) |>
  arrange(tipo, desc(n))


############################################################
# B. TABLAS ({gt})
############################################################

# ─────────────────────────────────────────────────────────
# Tabla A. Distribución de Influenza por subtipo/linaje.
# n y % calculados sobre el total de casos Influenza+.
# Agrupados por Tipo A / Tipo B con fila de total global.
# ─────────────────────────────────────────────────────────
tabla_subtipos_influenza <- resumen_subtipos_influenza |>
  rename(
    `Subtipo / linaje` = INFLUENZA_FINAL,
    `Tipo`             = tipo,
    `Casos (n)`        = n,
    `%`                = porcentaje
  ) |>
  gt(groupname_col = "Tipo") |>
  tab_header(
    title    = "Distribución de casos de Influenza por subtipo y linaje",
    subtitle = "Pacientes IRAG/IRAGe con resultado positivo a Influenza"
  ) |>
  fmt_number(columns = `Casos (n)`, decimals = 0) |>
  cols_align(align = "center", columns = c(`Casos (n)`, `%`)) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  grand_summary_rows(
    columns = `Casos (n)`,
    fns     = list(Total = ~ sum(.)),
    fmt     = ~ fmt_number(., decimals = 0)
  ) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")

# ─────────────────────────────────────────────────────────
# Tabla B. Registro de oseltamivir según resultado de
# Influenza.
# Muestra n y % de "Con registro" y "Sin registro" dentro
# de cada grupo virológico.
# La nota al pie aclara la limitación del dato.
# ─────────────────────────────────────────────────────────
tabla_oseltamivir_influenza <- cruce_oseltamivir_influenza |>
  select(resultado_influenza_cat, oseltamivir_cat, n, porcentaje) |>
  rename(
    `Resultado Influenza` = resultado_influenza_cat,
    `Oseltamivir`         = oseltamivir_cat,
    `Casos (n)`           = n,
    `%`                   = porcentaje
  ) |>
  gt(groupname_col = "Resultado Influenza") |>
  tab_header(
    title    = "Registro de oseltamivir según resultado de Influenza",
    subtitle = "Pacientes IRAG/IRAGe · UC-IRAG HGNRG"
  ) |>
  fmt_number(columns = `Casos (n)`, decimals = 0) |>
  cols_align(align = "center", columns = c(`Casos (n)`, `%`)) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  tab_footnote(
    footnote  = paste0(
      "'Sin registro' incluye tanto pacientes que no recibieron el antiviral ",
      "como registros sin dato cargado. La variable OSELTAMIVIR sólo codifica ",
      "presencia confirmada (1); la ausencia no está diferenciada del dato faltante."
    ),
    locations = cells_column_labels(columns = `Oseltamivir`)
  ) |>
  tab_source_note("Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026")


############################################################
# C. GRÁFICOS ({ggplot2})
############################################################

# ─────────────────────────────────────────────────────────
# Gráfico A. Distribución de Influenza por subtipo/linaje.
# Barras horizontales ordenadas de mayor a menor frecuencia.
# El color distingue Tipo A (azules) de Tipo B (verdes).
# La leyenda se omite porque el color está en el eje Y.
# ─────────────────────────────────────────────────────────
grafico_subtipos_influenza <- resumen_subtipos_influenza |>
  mutate(
    INFLUENZA_FINAL = fct_reorder(as.character(INFLUENZA_FINAL), n),
    etiqueta        = paste0(n, " (", porcentaje, "%)")
  ) |>
  ggplot(aes(
    x    = INFLUENZA_FINAL,
    y    = n,
    fill = as.character(INFLUENZA_FINAL)
  )) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.1,
    size  = 3.5
  ) +
  coord_flip() +
  scale_fill_manual(values = paleta_influenza) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Distribución de Influenza por subtipo y linaje",
    subtitle = "Casos positivos a Influenza · IRAG/IRAGe",
    x        = NULL,
    y        = "Número de casos",
    caption  = "Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026"
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ─────────────────────────────────────────────────────────
# Gráfico B. Evolución temporal de Influenza por subtipo.
# Curvas semanales para cada subtipo/linaje detectado.
# Útil para visualizar cocirculación y reemplazo entre
# subtipos a lo largo de las temporadas.
# ─────────────────────────────────────────────────────────
eje_influenza <- influenza_por_semana_subtipo |>
  distinct(semana_orden, se_label) |>
  arrange(semana_orden)

idx_inf <- seq(1, nrow(eje_influenza), by = 8)

grafico_influenza_temporal <- influenza_por_semana_subtipo |>
  ggplot(aes(
    x     = semana_orden,
    y     = n,
    color = as.character(INFLUENZA_FINAL),
    group = as.character(INFLUENZA_FINAL)
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = paleta_influenza, name = NULL) +
  scale_x_continuous(
    breaks = eje_influenza$semana_orden[idx_inf],
    labels = eje_influenza$se_label[idx_inf]
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Evolución temporal de Influenza por subtipo y linaje",
    subtitle = "Casos positivos por semana epidemiológica · IRAG/IRAGe",
    x        = "Semana epidemiológica",
    y        = "Número de casos",
    caption  = "Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026"
  ) +
  tema_irag +
  guides(color = guide_legend(nrow = 2))

# ─────────────────────────────────────────────────────────
# Gráfico C. Registro de oseltamivir según resultado de
# Influenza — barras agrupadas (dodge).
# Se usa posición "dodge" en lugar de barras apiladas al
# 100% porque "Sin registro" es ambiguo: apilar daría una
# falsa imagen de complementariedad.
# Las etiquetas muestran n y % para máxima transparencia.
# ─────────────────────────────────────────────────────────
grafico_oseltamivir_influenza <- cruce_oseltamivir_influenza |>
  ggplot(aes(
    x    = resultado_influenza_cat,
    y    = n,
    fill = oseltamivir_cat
  )) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(
    aes(label = paste0(porcentaje, "%\n(n=", n, ")")),
    position = position_dodge(width = 0.6),
    vjust    = -0.3,
    size     = 3.2
  ) +
  scale_fill_manual(
    values = c(
      "Con registro de administración" = "#1565C0",
      "Sin registro"                   = "#B0BEC5"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Registro de oseltamivir según resultado de Influenza",
    subtitle = paste0(
      "Pacientes IRAG/IRAGe · 'Sin registro' no distingue ",
      "no-administración de dato faltante"
    ),
    x        = NULL,
    y        = "Número de casos",
    fill     = NULL,
    caption  = "Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026"
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ─────────────────────────────────────────────────────────
# Gráfico D. Registro de oseltamivir por grupo etario.
# Muestra el % con registro de administración en cada
# grupo etario sobre el total del grupo (con y sin registro).
# Útil para detectar patrones de prescripción por edad.
# ─────────────────────────────────────────────────────────
grafico_oseltamivir_edad <- oseltamivir_por_edad |>
  filter(oseltamivir_cat == "Con registro de administración") |>
  ggplot(aes(
    x = EDAD_UC_IRAG,
    y = porcentaje
  )) +
  geom_col(fill = "#1565C0", width = 0.65) +
  geom_text(
    aes(label = paste0(porcentaje, "%\n(n=", n, ")")),
    hjust = -0.1,
    size  = 3.2
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 110),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Registro de oseltamivir por grupo etario",
    subtitle = "% con registro de administración sobre total del grupo · IRAG/IRAGe",
    x        = NULL,
    y        = "% con registro de oseltamivir",
    caption  = paste0(
      "Fuente: SNVS 2.0 · SE 20/2024 – SE 5/2026\n",
      "'Sin registro' incluye no-administración y dato no cargado"
    )
  ) +
  tema_irag +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


############################################################
# D. RESUMEN EN CONSOLA
############################################################

cat("
══════════════════════════════════════════════════
  INFLUENZA + OSELTAMIVIR — OBJETOS DISPONIBLES
══════════════════════════════════════════════════

DATOS INTERMEDIOS:
  casos_influenza_positivos     → subconjunto Influenza+
  resumen_subtipos_influenza    → n y % por subtipo/linaje
  influenza_por_semana_subtipo  → conteos semanales por subtipo
  base_oseltamivir              → base con oseltamivir_cat y
                                   resultado_influenza_cat
  cruce_oseltamivir_influenza   → cruce virológico × oseltamivir
  oseltamivir_por_edad          → registro antiviral × edad

TABLAS ({gt}):
  tabla_subtipos_influenza      → Distribución por subtipo/linaje
  tabla_oseltamivir_influenza   → Oseltamivir × resultado Influenza

GRÁFICOS ({ggplot2}):
  grafico_subtipos_influenza    → Barras: distribución por subtipo
  grafico_influenza_temporal    → Curvas temporales por subtipo
  grafico_oseltamivir_influenza → Barras agrupadas: registro × resultado
  grafico_oseltamivir_edad      → % registro oseltamivir por edad

══════════════════════════════════════════════════
  LIMITACION: OSELTAMIVIR solo codifica presencia (1).
  'Sin registro' != 'No recibio'. Interpretar con cautela.
══════════════════════════════════════════════════
")
