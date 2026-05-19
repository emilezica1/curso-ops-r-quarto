################ PARTE EMI - INDICADORES ################

#Arranco acá usando la base que dejó Mica (objeto "base")
#y la base agrupada UC_IRAG_Carga_Agrupada_CABA_Gutierrez para los indicadores 1 a 4
#Los subconjuntos los armo desde "base" respetando las variables que ya creó Mica

##### PREPARACIÓN BASE AGRUPADA

#La base agrupada viene en formato ancho (una columna por rango etario)
#La paso a formato largo para poder agrupar y sumar por edad sin problemas
#Las primeras 7 columnas son fijas, a partir de la 8 vienen los rangos etarios
rangos_etarios <- c(
  "0 a 2 m", "3 a 5 m", "6 a 11 m", "12 a 23 m",
  "2 a 4 años", "5 a 9 años", "10 a 14 años", "15 a 19 años",
  "20 a 24 años", "25 a 29 años", "30 a 34 años", "35 a 39 años",
  "40 a 44 años", "45 a 49 años", "50 a 54 años", "55 a 59 años",
  "60 a 64 años", "65 a 69 años", "70 a 74 años", ">= a 75 años",
  "Sin especificar"
)

agrupada_raw <- UC_IRAG_Carga_Agrupada_CABA_Gutierrez

nombres_columnas_fijas <- c(
  "jurisdiccion", "anio", "semana", "fecha_registro", "usuario", "hospital", "evento"
)

cantidad_rangos <- ncol(agrupada_raw) - 7
names(agrupada_raw) <- c(nombres_columnas_fijas, rangos_etarios[seq_len(cantidad_rangos)])

#pivot_longer: cada fila pasa a ser hospital + semana + evento + grupo_edad + casos
#Los NA los trato como 0 porque celda vacía = no hubo casos ese grupo esa semana
agrupada <- agrupada_raw |>
  pivot_longer(
    cols      = all_of(rangos_etarios[seq_len(cantidad_rangos)]),
    names_to  = "grupo_edad",
    values_to = "casos"
  ) |>
  mutate(
    casos  = replace_na(as.numeric(casos), 0),
    anio   = as.integer(anio),
    semana = as.integer(semana)
  )

#Guardo las etiquetas exactas de los eventos para no tener errores tipográficos en los filtros
EVENTO_IRAG             <- "Casos de IRAG entre los internados"
EVENTO_IRAGE            <- "Casos de IRAG extendida entre los internados"
EVENTO_TODAS_LAS_CAUSAS <- "Pacientes internados por todas las causas"
EVENTO_UCI_TOTAL        <- "Pacientes ingresados a UCI"
EVENTO_UCI_IRAG         <- "Casos de IRAG entre los ingresados a UCI"
#IRAGe en UCI aparece con dos variantes según cómo se cargó
EVENTO_UCI_IRAGE        <- c(
  "Casos de IRAG extendida entre los ingresados a UCI",
  "Casos de IRAG EXTENDIDA entre los ingresados a UCI"
)
EVENTO_DEFUNCIONES_IRAG  <- "Defunciones por IRAG"
EVENTO_DEFUNCIONES_IRAGE <- "Defunciones por IRAG extendida"

##### SUBCONJUNTOS DESDE BASE NOMINAL

#Armo los subconjuntos que reusan varios indicadores
#Uso "base" que es el objeto que dejó Mica con todas las variables ya preparadas
pacientes_irag_o_irage <- base |>
  filter(CLASIFICACION_MANUAL %in% c(
    "Infección respiratoria aguda grave (IRAG)",
    "IRAG extendida"
  ))

pacientes_irage_solo <- base |>
  filter(CLASIFICACION_MANUAL == "IRAG extendida")

pacientes_fallecidos <- pacientes_irag_o_irage |>
  filter(FALLECIDO == "SI")

#Menores de 6 meses usando los niveles que definió Mica en EDAD_UC_IRAG
pacientes_menor_6m <- pacientes_irag_o_irage |>
  filter(EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses"))

pacientes_mayor_6m <- pacientes_irag_o_irage |>
  filter(!EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses"))


##### INDICADORES DESDE BASE AGRUPADA

#Indicador 1: Casos de IRAG e IRAGe por semana epidemiológica
#Sumo todos los rangos etarios para obtener el total semanal
#Lo muestro separado por IRAG e IRAGe para ver la evolución de cada uno
casos_irag_irage_por_semana <- agrupada |>
  filter(evento %in% c(EVENTO_IRAG, EVENTO_IRAGE)) |>
  group_by(anio, semana, evento) |>
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
  arrange(anio, semana)

cat("=== INDICADOR 1: Casos de IRAG e IRAGe por SE ===\n")
print(casos_irag_irage_por_semana)

#Indicador 2: Proporción de hospitalizaciones por IRAG/IRAGe
#Fórmula: (casos IRAG + IRAGe internados / total internados todas las causas) * 100
#Uno numerador y denominador con left_join usando año y semana como llave
numerador_hospitalizaciones <- agrupada |>
  filter(evento %in% c(EVENTO_IRAG, EVENTO_IRAGE)) |>
  group_by(anio, semana) |>
  summarise(casos_irag_irage = sum(casos, na.rm = TRUE), .groups = "drop")

denominador_hospitalizaciones <- agrupada |>
  filter(evento == EVENTO_TODAS_LAS_CAUSAS) |>
  group_by(anio, semana) |>
  summarise(total_internados = sum(casos, na.rm = TRUE), .groups = "drop")

proporcion_hospitalizaciones <- left_join(
  numerador_hospitalizaciones, denominador_hospitalizaciones, by = c("anio", "semana")
) |>
  mutate(
    #if_else para evitar dividir por cero en semanas sin internados
    porcentaje_hospitalizaciones = if_else(
      total_internados > 0,
      round(casos_irag_irage / total_internados * 100, 1),
      NA_real_
    )
  ) |>
  arrange(anio, semana)

cat("\n=== INDICADOR 2: % Hospitalizaciones por IRAG/IRAGe ===\n")
print(proporcion_hospitalizaciones)

#Indicador 3: Proporción de IRAG/IRAGe con ingreso a UCI
#Fórmula: (IRAG/IRAGe en UCI / total IRAG/IRAGe internados) * 100
#La base es específica de UC-IRAG así que el denominador correcto es el total
#de casos IRAG/IRAGe internados, no el total de ingresos UCI del hospital
numerador_uci <- agrupada |>
  filter(evento %in% c(EVENTO_UCI_IRAG, EVENTO_UCI_IRAGE)) |>
  group_by(anio, semana) |>
  summarise(irag_irage_en_uci = sum(casos, na.rm = TRUE), .groups = "drop")

denominador_uci <- agrupada |>
  filter(evento %in% c(EVENTO_IRAG, EVENTO_IRAGE)) |>
  group_by(anio, semana) |>
  summarise(total_internados_irag = sum(casos, na.rm = TRUE), .groups = "drop")

proporcion_uci <- left_join(
  numerador_uci, denominador_uci, by = c("anio", "semana")
) |>
  mutate(
    porcentaje_irag_en_uci = if_else(
      total_internados_irag > 0,
      round(irag_irage_en_uci / total_internados_irag * 100, 1),
      NA_real_
    )
  ) |>
  arrange(anio, semana)

cat("\n=== INDICADOR 3: % IRAG/IRAGe con ingreso a UCI ===\n")
print(proporcion_uci)

#Indicador 4: Tasa de Letalidad por IRAG/IRAGe
#Fórmula: (fallecidos IRAG/IRAGe / internados IRAG/IRAGe) * 100
numerador_letalidad <- agrupada |>
  filter(evento %in% c(EVENTO_DEFUNCIONES_IRAG, EVENTO_DEFUNCIONES_IRAGE)) |>
  group_by(anio, semana) |>
  summarise(fallecidos_irag_irage = sum(casos, na.rm = TRUE), .groups = "drop")

denominador_letalidad <- agrupada |>
  filter(evento %in% c(EVENTO_IRAG, EVENTO_IRAGE)) |>
  group_by(anio, semana) |>
  summarise(internados_irag_irage = sum(casos, na.rm = TRUE), .groups = "drop")

tasa_letalidad <- left_join(
  numerador_letalidad, denominador_letalidad, by = c("anio", "semana")
) |>
  mutate(
    tasa_letalidad_pct = if_else(
      internados_irag_irage > 0,
      round(fallecidos_irag_irage / internados_irag_irage * 100, 1),
      NA_real_
    )
  ) |>
  arrange(anio, semana)

cat("\n=== INDICADOR 4: Tasa de Letalidad IRAG/IRAGe ===\n")
print(tasa_letalidad)


##### INDICADORES DESDE BASE NOMINAL

#Indicador 5: Casos por grupo etario
#Distribución de IRAG/IRAGe por EDAD_UC_IRAG (ya es factor ordenado, lo hizo Mica)
#Lo hago separado por clasificación para ver si difiere la distribución etaria
casos_por_edad <- pacientes_irag_o_irage |>
  count(CLASIFICACION_MANUAL, EDAD_UC_IRAG, name = "cantidad") |>
  group_by(CLASIFICACION_MANUAL) |>
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1)) |>
  ungroup() |>
  arrange(CLASIFICACION_MANUAL, EDAD_UC_IRAG)

cat("\n=== INDICADOR 5: Casos por grupo etario ===\n")
print(casos_por_edad)

#Indicador 6: Casos por sexo
#SEXO ya viene como factor (lo hizo Mica en 7.2)
#Excluyo los NA porque son registros sin dato de sexo
casos_por_sexo <- pacientes_irag_o_irage |>
  filter(!is.na(SEXO)) |>
  count(CLASIFICACION_MANUAL, SEXO, name = "cantidad") |>
  group_by(CLASIFICACION_MANUAL) |>
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1)) |>
  ungroup()

cat("\n=== INDICADOR 6: Casos por sexo ===\n")
print(casos_por_sexo)

#Indicador 7: % Positividad viral por SE
#VSR_FINAL, INFLUENZA_FINAL y COVID_19_FINAL ya son factor (los hizo Mica en 7.5)
#Positivo = cualquier nivel que no sea "Negativo" ni "Sin resultado"
#Uso SEPI_FECHA_INTER y ANIO_FECHA_INTER porque es la SE de referencia que definió Mica
positividad_viral_por_semana <- pacientes_irag_o_irage |>
  filter(!is.na(SEPI_FECHA_INTER)) |>
  group_by(ANIO_FECHA_INTER, SEPI_FECHA_INTER) |>
  summarise(
    total_casos_en_se   = n(),
    positivos_vsr       = sum(!is.na(VSR_FINAL) &
                                !as.character(VSR_FINAL) %in% c("Negativo", "Sin resultado"),
                              na.rm = TRUE),
    positivos_influenza = sum(!is.na(INFLUENZA_FINAL) &
                                !as.character(INFLUENZA_FINAL) %in% c("Negativo", "Sin resultado"),
                              na.rm = TRUE),
    positivos_covid     = sum(as.character(COVID_19_FINAL) == "Positivo", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    porcentaje_vsr       = round(positivos_vsr       / total_casos_en_se * 100, 1),
    porcentaje_influenza = round(positivos_influenza / total_casos_en_se * 100, 1),
    porcentaje_covid     = round(positivos_covid     / total_casos_en_se * 100, 1)
  ) |>
  arrange(ANIO_FECHA_INTER, SEPI_FECHA_INTER)

cat("\n=== INDICADOR 7: % Positividad viral por SE ===\n")
print(positividad_viral_por_semana)

#Indicador 8: Comorbilidad en pacientes IRAG/IRAGe
#PRESENCIA_COMORBILIDADES ya es factor "Con comorbilidad"/"Sin comorbilidad" (Mica, 7.6)
#Fórmula: (Con comorbilidad / total IRAG/IRAGe) * 100
resumen_comorbilidad <- pacientes_irag_o_irage |>
  summarise(
    total_pacientes      = n(),
    con_comorbilidad     = sum(PRESENCIA_COMORBILIDADES == "Con comorbilidad", na.rm = TRUE),
    porcentaje_con_comor = round(con_comorbilidad / total_pacientes * 100, 1)
  )

cat("\n=== INDICADOR 8: Comorbilidad en pacientes IRAG/IRAGe ===\n")
print(resumen_comorbilidad)

#Indicador 9: Tipo de comorbilidades
#Las columnas de comorbilidad las convirtió Mica a character en 7.6
#con na_if para los códigos 9, entonces "1" = presente, "2" = ausente, NA = sin dato
pacientes_con_comorbilidad <- pacientes_irag_o_irage |>
  filter(PRESENCIA_COMORBILIDADES == "Con comorbilidad")
total_con_comorbilidad <- nrow(pacientes_con_comorbilidad)

frecuencia_comorbilidades <- map_dfr(vars_como, function(v) {
  if (!v %in% names(pacientes_con_comorbilidad)) return(NULL)
  tibble(
    comorbilidad = v,
    cantidad     = sum(pacientes_con_comorbilidad[[v]] == "1", na.rm = TRUE),
    porcentaje   = round(cantidad / total_con_comorbilidad * 100, 1)
  )
}) |> filter(cantidad > 0) |> arrange(desc(cantidad))

cat("\n=== INDICADOR 9: Tipo de comorbilidades ===\n")
cat(paste0("Base: ", total_con_comorbilidad, " pacientes con al menos una comorbilidad\n"))
print(frecuencia_comorbilidades)

#Indicador 10: Vacunación VSR materna (< 6 meses)
#VAC_VSR viene como character (lo convirtió Mica en 7.8)
#Fórmula: (madre vacunada con dato / total < 6m con dato) * 100
#Con dato = tiene info (no es NA ni "SIN DATO")
vacunacion_vsr_materna <- pacientes_menor_6m |>
  summarise(
    total_menores_6m     = n(),
    con_dato_vsr_materna = sum(!is.na(VAC_VSR) & VAC_VSR != "SIN DATO", na.rm = TRUE),
    madre_vacunada_vsr   = sum(!is.na(VAC_VSR) &
                                 !VAC_VSR %in% c("SIN DATO", "MADRE NO VACUNADA"),
                               na.rm = TRUE),
    porcentaje_vacunadas = round(madre_vacunada_vsr / con_dato_vsr_materna * 100, 1)
  )

cat("\n=== INDICADOR 10: Vacunación VSR materna (casos < 6 meses) ===\n")
print(vacunacion_vsr_materna)

#Indicador 11: Vacunación antigripal
#VAC_ANTIGRIPAL y VAC_ANTIGRIPAL_MATERNA ya son factor (Mica, 7.8)
#>= 6m: (CONSTATADA o REFERIDA / total con dato) * 100
#< 6m: (CONSTATADA o REFERIDA materna / total madres con dato) * 100
vacunacion_gripe_mayores <- pacientes_mayor_6m |>
  summarise(
    total_pacientes      = n(),
    con_dato_vacunacion  = sum(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA", "NO VACUNADO"),
                               na.rm = TRUE),
    vacunados            = sum(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA"), na.rm = TRUE),
    porcentaje_vacunados = round(vacunados / con_dato_vacunacion * 100, 1)
  )

vacunacion_gripe_materna <- pacientes_menor_6m |>
  summarise(
    total_menores_6m       = n(),
    con_dato_vac_materna   = sum(VAC_ANTIGRIPAL_MATERNA %in%
                                   c("CONSTATADA", "REFERIDA", "MADRE NO VACUNADA"),
                                 na.rm = TRUE),
    madres_vacunadas       = sum(VAC_ANTIGRIPAL_MATERNA %in% c("CONSTATADA", "REFERIDA"),
                                 na.rm = TRUE),
    porcentaje_vac_materna = round(madres_vacunadas / con_dato_vac_materna * 100, 1)
  )

cat("\n=== INDICADOR 11: Vacunación antigripal (>= 6 meses) ===\n")
print(vacunacion_gripe_mayores)
cat("\n=== INDICADOR 11b: Vacunación antigripal materna (< 6 meses) ===\n")
print(vacunacion_gripe_materna)

#Indicador 12: Palivizumab / Nirsevimab (< 6 meses)
#PALIVIZUMAB ya es character con NA donde había 9 (Mica, 7.8)
#Fórmula: (con PALIVIZUMAB == "1" / total < 6m) * 100
profilaxis_vsr_menores <- pacientes_menor_6m |>
  summarise(
    total_menores_6m  = n(),
    con_profilaxis    = sum(PALIVIZUMAB == "1", na.rm = TRUE),
    porcentaje_profil = round(con_profilaxis / total_menores_6m * 100, 1)
  )

cat("\n=== INDICADOR 12: Palivizumab/Nirsevimab (< 6 meses) ===\n")
print(profilaxis_vsr_menores)

#Indicador 13: Ingreso a UCI
#CUIDADO_INTENSIVO ya es factor SI/NO (Mica, 7.4)
#Fórmula: (CUIDADO_INTENSIVO == SI / total IRAG/IRAGe) * 100
ingreso_uci_resumen <- pacientes_irag_o_irage |>
  summarise(
    total_pacientes   = n(),
    ingresaron_uci    = sum(CUIDADO_INTENSIVO == "SI", na.rm = TRUE),
    porcentaje_en_uci = round(ingresaron_uci / total_pacientes * 100, 1)
  )

cat("\n=== INDICADOR 13: Ingreso a UCI ===\n")
print(ingreso_uci_resumen)

#Indicador 14: Fallecidos
#FALLECIDO ya es factor SI/NO (Mica, 7.4)
#Fórmula: (FALLECIDO == SI / total IRAG/IRAGe) * 100
resumen_fallecidos <- pacientes_irag_o_irage |>
  summarise(
    total_pacientes       = n(),
    fallecidos            = sum(FALLECIDO == "SI", na.rm = TRUE),
    porcentaje_fallecidos = round(fallecidos / total_pacientes * 100, 1)
  )

cat("\n=== INDICADOR 14: Fallecidos ===\n")
print(resumen_fallecidos)

#Indicador 15: Fallecidos con comorbilidades
#Fórmula: (fallecidos con "Con comorbilidad" / total fallecidos) * 100
fallecidos_con_comorbilidad <- pacientes_fallecidos |>
  summarise(
    total_fallecidos     = n(),
    fallecidos_con_comor = sum(PRESENCIA_COMORBILIDADES == "Con comorbilidad", na.rm = TRUE),
    porcentaje_con_comor = round(fallecidos_con_comor / total_fallecidos * 100, 1)
  )

cat("\n=== INDICADOR 15: Fallecidos con comorbilidades ===\n")
print(fallecidos_con_comorbilidad)

#Indicador 16: Distribución por grupo etario de fallecidos
#EDAD_UC_IRAG ya es factor ordenado (Mica, 7.2)
#Cuántos fallecidos hay en cada grupo y qué % del total representan
distribucion_edad_fallecidos <- pacientes_fallecidos |>
  count(EDAD_UC_IRAG, name = "cantidad") |>
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1)) |>
  arrange(desc(cantidad))

cat("\n=== INDICADOR 16: Grupo etario de fallecidos ===\n")
print(distribucion_edad_fallecidos)

#Indicador 17: Consistencia de clasificación IRAGe
#CONSISTENCIA_IRAG_EXT ya es factor (Mica, 7.4)
#Fórmula: (BIEN CLASIFICADO / total IRAGe) * 100
consistencia_irage <- pacientes_irage_solo |>
  summarise(
    total_irage          = n(),
    bien_clasificados    = sum(CONSISTENCIA_IRAG_EXT == "BIEN CLASIFICADO", na.rm = TRUE),
    porcentaje_correctos = round(bien_clasificados / total_irage * 100, 1)
  )

cat("\n=== INDICADOR 17: Consistencia clasificación IRAGe ===\n")
print(consistencia_irage)

#Indicador 18: Tratamiento (Oseltamivir y O2 bajo flujo)
#OSELTAMIVIR y OXIGENOTERAPIA_BAJO_FLUJO ya son character con NA en lugar de 9 (Mica, 7.7)
#Fórmula: (con el tratamiento / total IRAG/IRAGe) * 100 para cada uno
resumen_tratamiento <- pacientes_irag_o_irage |>
  summarise(
    total_pacientes          = n(),
    con_oseltamivir          = sum(OSELTAMIVIR == "1", na.rm = TRUE),
    con_o2_bajo_flujo        = sum(OXIGENOTERAPIA_BAJO_FLUJO == "1", na.rm = TRUE),
    porcentaje_oseltamivir   = round(con_oseltamivir   / total_pacientes * 100, 1),
    porcentaje_o2_bajo_flujo = round(con_o2_bajo_flujo / total_pacientes * 100, 1)
  )

cat("\n=== INDICADOR 18: Tratamiento (Oseltamivir / O2 bajo flujo) ===\n")
print(resumen_tratamiento)

#Indicador 19: Caso grave
#caso_grave ya es factor SI/NO que armó Mica en 7.9
#Fórmula: (caso_grave == SI / total IRAG/IRAGe) * 100
resumen_casos_graves <- pacientes_irag_o_irage |>
  summarise(
    total_pacientes   = n(),
    casos_graves      = sum(caso_grave == "SI", na.rm = TRUE),
    porcentaje_graves = round(casos_graves / total_pacientes * 100, 1)
  )

cat("\n=== INDICADOR 19: Caso grave ===\n")
print(resumen_casos_graves)

#Indicador 20: Soporte respiratorio
#soporte ya es factor ordenado que armó Mica en 7.9
#Muestro tanto el % total con soporte como la distribución por tipo
resumen_soporte_respiratorio <- pacientes_irag_o_irage |>
  summarise(
    total_pacientes        = n(),
    con_soporte            = sum(soporte %in% c("Bajo flujo", "Alto flujo",
                                                "Ventilación mecánica"), na.rm = TRUE),
    porcentaje_con_soporte = round(con_soporte / total_pacientes * 100, 1)
  )

distribucion_soporte <- pacientes_irag_o_irage |>
  filter(!is.na(soporte)) |>
  count(soporte, name = "cantidad") |>
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1))

cat("\n=== INDICADOR 20: Soporte respiratorio ===\n")
print(resumen_soporte_respiratorio)
cat("\nDistribución por tipo de soporte:\n")
print(distribucion_soporte)
