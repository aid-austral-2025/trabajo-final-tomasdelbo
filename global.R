##### global.R ####
#Carga de paquetes
suppressPackageStartupMessages({
  if (!require(tidyverse)) install.packages("tidyverse", repos = "https://cloud.r-project.org"); library(tidyverse)
  if (!require(shinyjs)) install.packages("shinyjs", repos = "https://cloud.r-project.org"); library(shinyjs)
  if (!require(shinydashboard)) install.packages("shinydashboard", repos = "https://cloud.r-project.org"); library(shinydashboard)
  if (!require(shinyWidgets)) install.packages("shinyWidgets", repos = "https://cloud.r-project.org"); library(shinyWidgets)
  if (!require(DT)) install.packages("DT", repos = "https://cloud.r-project.org"); library(DT)
  if (!require(plotly)) install.packages("plotly", repos = "https://cloud.r-project.org"); library(plotly)
})


#Definicion de zona horaria y evitar notacion cientifica
options(scipen = 999)
Sys.setenv(TZ = "America/Argentina/Buenos_Aires")

#Definicion de la direccion para guardar datos generados
DATA_DIR  <- "data"
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)


# Rutas de archivos
SM_XLS_DIR <- "datos_crudos/SM-DT-SMF - Solicitudes_Modificación/RS-ING-02-PLANILLA REGISTRO SM-DT-SMF 01-06-16.xlsx"               # Carpeta con .xlsx (SM)
NP_XLS_PATH <- "datos_crudos/PROYECTOS I+D A REPORTAR/RS-ING-XX PLANILLA REGISTRO PROYECTOS I+D 10-04-17.xlsx"                # Archivo o carpeta (NP)
PE_XLS_FILE <- "datos_crudos/RS-ING-09 PLANILLA REGISTRO P.E.10-03-16.xlsx"  # Archivo (PE)


# --- Paths CSV para las tablas a utilizar
f_personal   <- file.path(DATA_DIR, "personal.csv")
f_sectores   <- file.path(DATA_DIR, "sectores.csv")
f_tipos      <- file.path(DATA_DIR, "tipos_tarea.csv")
f_proyectos  <- file.path(DATA_DIR, "proyectos.csv")
f_servicios  <- file.path(DATA_DIR, "servicios_pe.csv")
f_sm         <- file.path(DATA_DIR, "sm.csv")
f_rutinas    <- file.path(DATA_DIR, "rutinas.csv")
f_imput      <- file.path(DATA_DIR, "imputaciones.csv")


#Helpers
#Funcion para lectura segura de csv, si el archivo no existe
safe_read <- function(path){
  if (!file.exists(path)) return(tibble())
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

#funcion para sobreescribir CSV
save_overwrite <- function(df, path){ readr::write_csv(df, path) }

# Append con normalización para el guardado de imputaciones
save_append <- function(df_new, path){
  req_cols <- c(
    "imput_id","fecha","persona_id","tipo_id",
    "proyecto_id","pe_id","sm_id","rutina_id",
    "sector_id","horas","observ","anio","mes"
  )
  
  #Funcion de normalizacion
  norm <- function(x){
    x %>% mutate(
      imput_id    = suppressWarnings(as.integer(imput_id)),
      fecha       = as.Date(fecha),
      persona_id  = suppressWarnings(as.integer(persona_id)),
      tipo_id     = suppressWarnings(as.integer(tipo_id)),
      proyecto_id = as.character(proyecto_id),
      pe_id       = as.character(pe_id),
      sm_id       = as.character(sm_id),
      rutina_id   = suppressWarnings(as.integer(rutina_id)),
      sector_id   = suppressWarnings(as.integer(sector_id)),
      horas       = suppressWarnings(as.numeric(horas)),
      observ      = as.character(observ),
      anio        = suppressWarnings(as.integer(anio)),
      mes         = suppressWarnings(as.integer(mes))
    )
  }
  #Si al nuevo dataframe le faltan columnas las completo con NA y normalizo
  miss_new <- setdiff(req_cols, names(df_new))
  if (length(miss_new)) df_new[miss_new] <- NA
  df_new <- norm(df_new)
  
  #Si el archivo existe los combino. Si no existe lo creo y guardo los datos
  if (file.exists(path)) {
    old <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    miss_old <- setdiff(req_cols, names(old))
    if (length(miss_old)) old[miss_old] <- NA
    old <- norm(old)
    df_new <- df_new[, req_cols]
    old    <- old[,    req_cols]
    all <- dplyr::bind_rows(old, df_new)
    readr::write_csv(all, path)
  } else {
    df_new <- df_new[, req_cols]
    readr::write_csv(df_new, path)
  }
  invisible(TRUE)
}


#Funcion para buscar columnas
df_if_ncol <- function(df, i){ if (ncol(df) >= i) df[[i]] else NA }

#Carga de excel de SM
#Abre el Excel y obtiene todas sus hojas
#Tomo las columnas de numero de SM y fecha de cierre
#Filtro las que tienen numero de SM descartando las vacias
#Limpio los datos dejando los numeros validos que no tienen fecha de cierre asignada
#Genero una tabla con los datos

load_sm_from_excel <- function(path = SM_XLS_DIR) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(tibble())
  sheets <- tryCatch(suppressMessages(readxl::excel_sheets(path)), error = function(e) character())
  if (length(sheets) == 0) return(tibble())
  acc <- list()
  for (sh in sheets) {
    df <- tryCatch(suppressMessages(readxl::read_excel(path, sheet = sh, skip = 6, col_names = FALSE)), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) next
    colB <- df_if_ncol(df, 2)
    colI <- df_if_ncol(df, 9)
    tmp <- tibble(num = as.character(colB), c9 = as.character(colI)) %>%
      mutate(num = str_trim(num)) %>%
      filter(!is.na(num), num != "", is.na(c9) | c9 == "") %>%
      mutate(num = str_replace_all(num, "[^0-9]", "")) %>%
      filter(num != "")
    if (nrow(tmp) > 0) acc[[length(acc) + 1]] <- tmp
  }
  if (length(acc) == 0) return(tibble())
  bind_rows(acc) %>%
    distinct(num) %>%
    arrange(suppressWarnings(as.numeric(num))) %>%
    mutate(sm_id = row_number(), sm_desc = paste0("SM-", num)) %>%
    select(sm_id, sm_desc)
}

# Carga de excel de proyectos
#Leo el archivo desde la fila 6 para evitar titulos
#Saco las columnas de numero del proyecto, nombre y fecha de homologacion
#Lipio los datos
#Filtro por los que no tienen fecha de homologacion
#junto el numero de proyecto con el nombre
#Asigno id y lo marco como activo
#Devuelvo la tabla
load_np_from_excel <- function(path = NP_XLS_PATH) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(tibble())
  
  df <- tryCatch(
    suppressMessages(readxl::read_excel(path, sheet = 1, skip = 5, col_names = FALSE)),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(tibble())

  colA <- df_if_ncol(df, 1)  # Código / Nombre corto
  colB <- df_if_ncol(df, 2)  # Descripción
  colL <- df_if_ncol(df, 12) # Fecha de cierre
  
  tibble(A = as.character(colA), B = as.character(colB), L = as.character(colL)) %>%
    mutate(A = str_trim(A), B = str_trim(B)) %>%
    filter(!is.na(A), A != "", is.na(L) | L == "") %>%  # Solo los NP sin fecha de cierre
    mutate(proyecto = if_else(!is.na(B) & B != "", paste0(A, "  -  ", B), A)) %>%
    distinct(proyecto, .keep_all = TRUE) %>%
    mutate(
      proyecto_id = row_number(),
      activo = TRUE
    ) %>%
    select(proyecto_id, proyecto, activo)
}


#Leo la hoja 4 del archivo excel de pedidos especiales desde la fila 7 para evitar titulos
#Extraigo las columnas 1, 3 y 5 que tienen los datos necesarios
#Limpieza
#Filtro los que tienen la columna 5 vacia o NA
#Tomo las que no tienen la columna 1 vacia
#Genero el texto de la columna pe_desc <- col(1) - col(3)
#Elimino duplicados

load_pe_from_excel <- function(file = PE_XLS_FILE) {
  # Verificar que el archivo exista
  if (is.null(file) || is.na(file) || !file.exists(file)) return(tibble())
  
  # Leer la hoja (usa sheet = 4 y skip = 6, como tu caso)
  df <- tryCatch(
    suppressMessages(readxl::read_excel(file, sheet = 4, skip = 6, col_names = FALSE, col_types = "text")),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(tibble())
  
  # Asegurar columnas: B (2), D (4), F (6)
  colB <- if (ncol(df) >= 1) df[[1]] else rep(NA_character_, nrow(df))
  colD <- if (ncol(df) >= 3) df[[3]] else rep(NA_character_, nrow(df))
  colF <- if (ncol(df) >= 5) df[[5]] else rep(NA_character_, nrow(df))
  
  # Limpieza de texto
  clean_text <- function(x) {
    x <- as.character(x)
    x <- stringr::str_replace_all(x, "[\\u00A0\\t\\r\\n]", " ")
    x <- stringr::str_replace_all(x, "[[:space:]]+", " ")
    stringr::str_trim(x)
  }
  
  colB <- clean_text(colB)
  colD <- clean_text(colD)
  colF <- clean_text(colF)
  
  # Crear tabla y filtrar: mantener filas donde F está vacía
  out <- tibble(B = colB, D = colD, F = colF) %>%
    dplyr::filter(is.na(F) | F == "") %>%      # solo filas donde F está vacía
    dplyr::filter(!is.na(B), B != "") %>%      # y donde B no esté vacía
    dplyr::mutate(
      pe_desc = ifelse(!is.na(D) & D != "", paste0(B, " - ", D), B)
    ) %>%
    dplyr::distinct(pe_desc, .keep_all = TRUE) %>%
    dplyr::mutate(pe_id = dplyr::row_number()) %>%
    dplyr::select(pe_id, pe_desc)
  
  return(out)
}


#Funcion para crear archivos csv si no existen como base para iniciar
seed_csvs <- function(){
  if (!file.exists(f_sectores)) readr::write_csv(tibble(
    sector_id = 1:4, sector = c("I+D","Producción","Comercial","Administración")
  ), f_sectores)
  
  if (!file.exists(f_tipos)) readr::write_csv(tibble(
    tipo_id = 1:4, tipo = c("Nuevos Proyectos","PE","SM","Rutinas de oficina")
  ), f_tipos)
  
  if (!file.exists(f_proyectos)) readr::write_csv(tibble(
    proyecto_id = 1:3, proyecto = c("NP-Alpha","NP-Beta","NP-Gamma"), activo = c(TRUE, TRUE, FALSE)
  ), f_proyectos)
  
  if (!file.exists(f_servicios)) readr::write_csv(tibble(
    pe_id = 1:4,
    pe_desc = c("Mantenimiento Línea A","Asistencia Calidad","Soporte Comercial","Mejora Postventa"),
    mes = month(Sys.Date()), anio = year(Sys.Date())
  ), f_servicios)
  
  if (!file.exists(f_sm)) readr::write_csv(tibble(
    sm_id = 1:5, sm_desc = c("SM-001","SM-002","SM-003","SM-004","SM-005")
  ), f_sm)
  
  if (!file.exists(f_rutinas)) readr::write_csv(tibble(
    rutina_id = 1:5, rutina = c("Reunión diaria","Correo/Administrativo","Capacitación interna","Documentación","Limpieza de datos")
  ), f_rutinas)
  
  if (!file.exists(f_personal)) readr::write_csv(tibble(
    persona_id = 1:7,
    persona    = c("Tomás Del Bó","Marcos Masetro","Federico Dutruel","Alfredo Betti",
                   "Leonel Rodriguez","Martín Tamone","Gabriel Medina"),
    sector_id  = c(1,1,2,3,3,2,1),
    jornada_hs = 8.5,
    vacaciones_anuales = 14,
    activo = TRUE
  ), f_personal)
  
  if (!file.exists(f_imput)) readr::write_csv(tibble(
    imput_id    = integer(),
    fecha       = as.Date(character()),
    persona_id  = integer(),
    tipo_id     = integer(),
    proyecto_id = character(),
    pe_id       = character(),
    sm_id       = character(),
    rutina_id   = integer(),
    sector_id   = integer(),
    horas       = double(),
    observ      = character(),
    anio        = integer(),
    mes         = integer()
  ), f_imput)
}

seed_csvs()


# ---------- Carga de catálogos en memoria de forma segura----------
load_catalogs <- function(){
  personal <- safe_read(f_personal); if (!"activo" %in% names(personal)) personal$activo <- TRUE
  tipos <- safe_read(f_tipos); tipos$tipo[tipos$tipo == "Servicios (PE)"] <- "PE"
  
  proyectos <- {
    tmp <- try(load_np_from_excel(), silent = TRUE)
    if (inherits(tmp, "try-error") || is.null(tmp) || (is.data.frame(tmp) && nrow(tmp) == 0)) {
      safe_read(f_proyectos) |> dplyr::filter(is.na(activo) | activo == TRUE)
    } else tmp
  }
  
  servicios <- {
    tmp <- try(load_pe_from_excel(), silent = TRUE)
    if (inherits(tmp, "try-error") || is.null(tmp) || (is.data.frame(tmp) && nrow(tmp) == 0)) {
      safe_read(f_servicios)
    } else tmp
  }
  
  sm <- {
    tmp <- try(load_sm_from_excel(), silent = TRUE)
    if (inherits(tmp, "try-error") || is.null(tmp) || (is.data.frame(tmp) && nrow(tmp) == 0)) {
      safe_read(f_sm) |> dplyr::transmute(sm_id = sm_id, sm_desc = sm_desc)
    } else tmp
  }
  
  list(
    personal  = personal,
    sectores  = safe_read(f_sectores),
    tipos     = tipos,
    proyectos = proyectos,
    servicios = servicios,
    sm        = sm,
    rutinas   = safe_read(f_rutinas)
  )
}

