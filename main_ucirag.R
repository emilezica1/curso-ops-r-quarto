
# MAIN UC-IRAG
# Hospital General de Niños "Dr. Ricardo Gutiérrez"
# Equipo Buenos Aires - CABA
#
# Objetivo:
# Ejecutar en orden los scripts del análisis UC-IRAG y dejar
# disponibles las bases, indicadores, tablas y gráficos usados
# por el reporte Quarto.

# 1. CONFIGURACIÓN


# TRUE = oculta cat(), print() y mensajes internos de los scripts.
EJECUCION_SILENCIOSA <- FALSE

# TRUE = intenta renderizar el Quarto al final.
RENDERIZAR_QUARTO <- TRUE

ARCHIVO_QUARTO <- "reporte_ucirag.qmd"


# 2. PAQUETES


paquetes_necesarios <- c(
  "tidyverse",
  "readxl",
  "lubridate",
  "writexl",
  "googledrive",
  "gt",
  "scales",
  "patchwork",
  "plotly"
)

cargar_paquete <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      paste0(
        "Falta instalar el paquete '", pkg, "'.\n",
        "Instalalo con: install.packages('", pkg, "')"
      ),
      call. = FALSE
    )
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

invisible(lapply(paquetes_necesarios, cargar_paquete))

# 3. FUNCIONES AUXILIARES DEL MAIN


buscar_script <- function(nombre_base) {
  candidatos <- c(
    nombre_base,
    sub("\\.R$", "(3).R", nombre_base),
    sub("\\.R$", "(2).R", nombre_base),
    sub("\\.R$", "(1).R", nombre_base)
  )
  
  candidatos <- unique(candidatos)
  encontrado <- candidatos[file.exists(candidatos)]
  
  if (length(encontrado) == 0) {
    stop(
      paste0(
        "No encontré el script requerido: ", nombre_base, "\n",
        "Busqué estas variantes: ", paste(candidatos, collapse = ", "), "\n",
        "Revisá que el main esté en la misma carpeta que los scripts."
      ),
      call. = FALSE
    )
  }
  
  encontrado[1]
}

source_controlado <- function(nombre_base) {
  archivo <- buscar_script(nombre_base)
  message("Ejecutando: ", archivo)
  
  if (EJECUCION_SILENCIOSA) {
    suppressWarnings(
      suppressMessages(
        invisible(
          capture.output(
            source(archivo, local = .GlobalEnv)
          )
        )
      )
    )
  } else {
    source(archivo, local = .GlobalEnv)
  }
  
  invisible(TRUE)
}

verificar_objetos <- function(objetos, etapa) {
  faltan <- objetos[!vapply(objetos, exists, logical(1), envir = .GlobalEnv)]
  
  if (length(faltan) > 0) {
    stop(
      paste0(
        "Después de la etapa '", etapa, "' faltan estos objetos: ",
        paste(faltan, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  message("OK: ", etapa)
  invisible(TRUE)
}


# 4. EJECUCIÓN EN ORDEN


# 4.1 Carga y filtrado de bases
source_controlado("Ana_mi_script.R")

verificar_objetos(
  c(
    "UC_IRAG_EST10194",
    "UC_IRAG_Carga_Agrupada_CABA_Gutierrez"
  ),
  "carga de bases"
)

# 4.2 Funciones auxiliares
source_controlado("patricia_mi_script.R")

verificar_objetos(
  c(
    "aplicar_criterios",
    "depurar_duplicados",
    "analizar_faltantes"
  ),
  "funciones auxiliares"
)

# 4.3 Preparación de base nominal
source_controlado("micaela_mi_script.R")

verificar_objetos(
  c(
    "base",
    "vars_como"
  ),
  "preparación de base nominal"
)

# 4.4 Indicadores epidemiológicos
source_controlado("Emiliano_mi_script.R")

verificar_objetos(
  c(
    "pacientes_irag_o_irage",
    "casos_irag_irage_por_semana",
    "proporcion_hospitalizaciones",
    "proporcion_uci",
    "tasa_letalidad",
    "casos_por_edad",
    "casos_por_sexo",
    "positividad_viral_por_semana",
    "frecuencia_comorbilidades"
  ),
  "indicadores epidemiológicos"
)

# 4.5 Tablas y gráficos principales
source_controlado("mavi_mi_script.R")

verificar_objetos(
  c(
    "tabla_edad",
    "tabla_sexo",
    "tabla_indicadores_globales",
    "tabla_completitud",
    "grafico_edad",
    "grafico_sexo",
    "grafico_curva_epidemica",
    "grafico_positividad_viral"
  ),
  "tablas y gráficos principales"
)

# 4.6 Bloque adicional influenza/oseltamivir
source_controlado("mavi_bloque_adicional_influenza.R")

verificar_objetos(
  c(
    "tabla_subtipos_influenza",
    "tabla_oseltamivir_influenza",
    "grafico_subtipos_influenza",
    "grafico_influenza_temporal"
  ),
  "bloque adicional influenza/oseltamivir"
)


# 5. CONTROL DEL INDICADOR DE UCI


if (exists("proporcion_uci", envir = .GlobalEnv)) {
  
  uci_mayor_100 <- proporcion_uci |>
    dplyr::filter(
      !is.na(porcentaje_irag_en_uci),
      porcentaje_irag_en_uci > 100
    )
  
  if (nrow(uci_mayor_100) > 0) {
    warning(
      paste0(
        "Se detectaron ", nrow(uci_mayor_100),
        " semanas con porcentaje_irag_en_uci > 100%. ",
        "Se exporta el detalle a 'revision_indicador_uci_mayor_100.csv'."
      ),
      call. = FALSE
    )
    
    readr::write_csv(
      uci_mayor_100,
      "revision_indicador_uci_mayor_100.csv"
    )
    
  } else {
    message("OK indicador UCI: no se detectaron porcentajes mayores a 100%.")
  }
}

# 6. RENDER OPCIONAL DEL QUARTO

if (RENDERIZAR_QUARTO) {
  
  if (!file.exists(ARCHIVO_QUARTO)) {
    
    warning(
      paste0(
        "No encontré el archivo Quarto: ", ARCHIVO_QUARTO,
        ". Se omite el render."
      ),
      call. = FALSE
    )
    
  } else if (!requireNamespace("quarto", quietly = TRUE)) {
    
    warning(
      paste0(
        "El paquete 'quarto' no está instalado. ",
        "Se omite el render. Si querés renderizar desde R, instalalo con: ",
        "install.packages('quarto')"
      ),
      call. = FALSE
    )
    
  } else {
    
    message("Renderizando Quarto: ", ARCHIVO_QUARTO)
    quarto::quarto_render(ARCHIVO_QUARTO)
  }
}


# 7. RESUMEN FINAL

message("")
message("══════════════════════════════════════════════════")
message("EJECUCIÓN FINALIZADA")
message("══════════════════════════════════════════════════")
message("Objetos principales disponibles:")
message("- base")
message("- pacientes_irag_o_irage")
message("- proporcion_hospitalizaciones")
message("- proporcion_uci")
message("- tasa_letalidad")
message("- tabla_indicadores_globales")
message("- tabla_edad / tabla_sexo / tabla_completitud")
message("- grafico_curva_epidemica / grafico_positividad_viral")
message("- tablas y gráficos de influenza/oseltamivir")
message("══════════════════════════════════════════════════")
message("")