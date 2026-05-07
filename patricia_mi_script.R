# Inclusión/Exclusión
aplicar_criterios <- function(df){
  df %>%
    filter(!is.na(FECHA_INTERNACION),
           !is.na(CONDICION_EGRESO),
           !is.na(MUESTRA_LABORATORIO),
           CLASIFICACION_MANUAL != "Casos invalidados por epidemiología")
}

# Duplicados por ID + IDEVENTOCASO
depurar_duplicados <- function(df){
  df %>%
    group_by(ID, IDEVENTOCASO) %>%
    mutate(n_na = rowSums(is.na(.))) %>%
    arrange(n_na) %>%
    slice(1) %>%
    ungroup() %>%
    select(-n_na)
}

# Análisis de faltantes (NA vs código 9)
analizar_faltantes <- function(df, nombre_base){
  cat("\n--- Análisis de faltantes en", nombre_base, "---\n")
  resultados <- data.frame(variable=character(), prop_total_faltante=numeric())
  for(v in colnames(df)){
    total <- nrow(df)
    n_na <- sum(is.na(df[[v]]))
    n_cod9 <- sum(df[[v]] == 9, na.rm = TRUE)
    prop_total <- round(100 * (n_na + n_cod9) / total, 1)
    resultados <- rbind(resultados, data.frame(variable=v, prop_total_faltante=prop_total))
  }
  print(resultados)
  criticas <- resultados %>% filter(prop_total_faltante > 30)
  if(nrow(criticas) > 0){
    cat("\nVariables con más del 30% de faltantes:\n")
    print(criticas)
  }
}

