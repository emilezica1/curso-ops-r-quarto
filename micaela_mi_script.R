#####  variables - MICA
# Punto 5 y 7: prepara la unidad de análisis y crea/ordena las variables necesarias

##### PUNTO 5 - UNIDAD DE ANÁLISIS
# Unidad individual: cada fila es un caso nominal notificado (UC_IRAG_EST10194)
# con sus determinaciones de virus respiratorios (VSR_FINAL, INFLUENZA_FINAL, COVID_19_FINAL)
# Unidad temporal: SE según fecha de internación (SEPI_FECHA_INTER + ANIO_FECHA_INTER)

##### 7.1 INCLUSIÓN/EXCLUSIÓN Y DEDUPLICACIÓN

base <- UC_IRAG_EST10194 |>
  filter(
    !is.na(FECHA_INTERNACION),
    !is.na(EVOLUCION),
    !is.na(VSR_FINAL) | !is.na(INFLUENZA_FINAL) | !is.na(COVID_19_FINAL),
    CLASIFICACION_MANUAL != "Casos invalidados por epidemiología"
  )

base <- base |>
  group_by(ID, IDEVENTOCASO) |>
  arrange(rowSums(is.na(across(everything())))) |>
  slice(1) |>
  ungroup()

cat("Registros tras depuración:", nrow(base), "\n")

##### 7.2 VARIABLES SOCIODEMOGRÁFICAS

niveles_edad <- c("0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses",
                  "12 a 23 Meses", "02 a 04 Años", "05 a 09 Años",
                  "10 a 14 Años", "15 a 19 Años")

base <- base |>
  mutate(
    SEXO         = factor(SEXO),
    EDAD_UC_IRAG = factor(EDAD_UC_IRAG, levels = niveles_edad, ordered = TRUE)
  )

print(table(base$EDAD_UC_IRAG, useNA = "ifany"))
print(table(base$SEXO,         useNA = "ifany"))

##### 7.3 VARIABLES TEMPORALES

base <- base |>
  mutate(
    SEPI_FECHA_INTER    = as.integer(SEPI_FECHA_INTER),
    ANIO_FECHA_INTER    = as.integer(ANIO_FECHA_INTER),
    se_anio             = paste0("SE ", SEPI_FECHA_INTER, "-", ANIO_FECHA_INTER),
    FECHA_INTERNACION   = as.Date(FECHA_INTERNACION),
    FECHA_FALLECIMIENTO = as.Date(FECHA_FALLECIMIENTO)
  )

print(range(base$se_anio, na.rm = TRUE))

##### 7.4 VARIABLES CLÍNICAS

base <- base |>
  mutate(
    CONSISTENCIA_IRAG_EXT = factor(CONSISTENCIA_IRAG_EXT,
                                   levels = c("BIEN CLASIFICADO",
                                              "NO CORRESPONDE POR IRAG",
                                              "NO CORRESPONDE POR CLINICA")),
    CUIDADO_INTENSIVO = factor(CUIDADO_INTENSIVO, levels = c("SI", "NO")),
    FALLECIDO         = factor(FALLECIDO,         levels = c("SI", "NO")),
    CURADO            = factor(CURADO,            levels = c("SI", "NO"))
  )

print(table(base$CUIDADO_INTENSIVO,    useNA = "ifany"))
print(table(base$FALLECIDO,            useNA = "ifany"))
print(table(base$CURADO,               useNA = "ifany"))
print(table(base$CONSISTENCIA_IRAG_EXT, useNA = "ifany"))

##### 7.5 VARIABLES DE LABORATORIO

base <- base |>
  mutate(
    VSR_FINAL = factor(VSR_FINAL,
                       levels = c("VSR A", "VSR B", "VSR", "Negativo", "Sin resultado")),
    INFLUENZA_FINAL = factor(INFLUENZA_FINAL,
                             levels = c("Influenza A H1N1", "Influenza A H3N2",
                                        "Influenza A sin subtipificar",
                                        "Influenza B Victoria", "Influenza B sin linaje",
                                        "Negativo", "Sin resultado")),
    COVID_19_FINAL = factor(COVID_19_FINAL)
  )

print(table(base$VSR_FINAL,       useNA = "ifany"))
print(table(base$INFLUENZA_FINAL, useNA = "ifany"))
print(table(base$COVID_19_FINAL,  useNA = "ifany"))

##### 7.6 COMORBILIDADES

vars_como <- c("ASMA", "DIABETES", "OBESIDAD", "VIH",
               "CARDIOPATIA_CONGENITA", "DBP", "ENF_NEUROLOGICA_CRONICA",
               "DESNUTRICION", "PREMATURIDAD", "S_DOWN",
               "INMUNOCOMPROMETIDO_OTRAS_CAUSAS",
               "OTRAS_COMORBILIDADES", "SIN_COMORBILIDADES")

base <- base |>
  mutate(
    PRESENCIA_COMORBILIDADES = case_when(
      PRESENCIA_COMORBILIDADES == 1 ~ "Con comorbilidad",
      PRESENCIA_COMORBILIDADES == 2 ~ "Sin comorbilidad",
      TRUE ~ NA_character_
    ),
    PRESENCIA_COMORBILIDADES = factor(PRESENCIA_COMORBILIDADES,
                                      levels = c("Con comorbilidad", "Sin comorbilidad")),
    across(all_of(vars_como), ~ na_if(as.character(.x), "9"))
  )

print(table(base$PRESENCIA_COMORBILIDADES, useNA = "ifany"))

##### 7.7 TRATAMIENTO Y SOPORTE

vars_trat <- c("OSELTAMIVIR", "OXIGENOTERAPIA_BAJO_FLUJO",
               "OXIGENOTERAPIA_ALTO_FLUJO", "VM")

base <- base |>
  mutate(across(all_of(vars_trat), ~ na_if(as.character(.x), "9")))

##### 7.8 INMUNIZACIÓN

base <- base |>
  mutate(
    VAC_ANTIGRIPAL = factor(VAC_ANTIGRIPAL,
                            levels = c("CONSTATADA", "REFERIDA", "NO VACUNADO", "SIN DATO")),
    VAC_ANTIGRIPAL_MATERNA = factor(VAC_ANTIGRIPAL_MATERNA,
                                    levels = c("CONSTATADA", "REFERIDA",
                                               "MADRE NO VACUNADA", "SIN DATO")),
    VAC_VSR     = as.character(VAC_VSR),
    PALIVIZUMAB = na_if(as.character(PALIVIZUMAB), "9")
  )

print(table(base$VAC_ANTIGRIPAL,        useNA = "ifany"))
print(table(base$VAC_ANTIGRIPAL_MATERNA, useNA = "ifany"))

##### 7.9 VARIABLES DERIVADAS

# SE-AÑO: ya construida en 7.3 como se_anio
# CASO_GRAVE: SI si tiene UCI, VM u OAF
# SOPORTE: categoría ordenada de soporte respiratorio

base <- base |>
  mutate(
    caso_grave = case_when(
      CUIDADO_INTENSIVO == "SI" | VM == "SI" | OXIGENOTERAPIA_ALTO_FLUJO == "SI" ~ "SI",
      !is.na(CUIDADO_INTENSIVO) | !is.na(VM) | !is.na(OXIGENOTERAPIA_ALTO_FLUJO) ~ "NO",
      TRUE ~ NA_character_
    ),
    caso_grave = factor(caso_grave, levels = c("SI", "NO")),
    
    soporte = case_when(
      VM == "SI"                        ~ "Ventilación mecánica",
      OXIGENOTERAPIA_ALTO_FLUJO == "SI" ~ "Alto flujo",
      OXIGENOTERAPIA_BAJO_FLUJO == "SI" ~ "Bajo flujo",
      !is.na(OXIGENOTERAPIA_BAJO_FLUJO) |
        !is.na(OXIGENOTERAPIA_ALTO_FLUJO) |
        !is.na(VM)                      ~ "Sin soporte",
      TRUE ~ NA_character_
    ),
    soporte = factor(soporte,
                     levels = c("Sin soporte", "Bajo flujo",
                                "Alto flujo", "Ventilación mecánica"),
                     ordered = TRUE)
  )

print(table(base$caso_grave, useNA = "ifany"))
print(table(base$soporte,    useNA = "ifany"))

##### 7.10 FALTANTES

analizar_faltantes(base, "base")

##### 7.11 VERIFICACIÓN FINAL

cat("Filas:", nrow(base), "| Columnas:", ncol(base), "\n")