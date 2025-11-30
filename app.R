##### app.R ####

source("global.R")  


# --- Helper para normalizar imputaciones (tipos/fechas/orden) ---
normalize_imput_types <- function(df){
  if (is.null(df) || nrow(df) == 0) return(tibble())
  
  req_cols <- c(
    "imput_id","fecha","persona_id","tipo_id",
    "proyecto_id","pe_id","sm_id","rutina_id",
    "sector_id","horas","observ","anio","mes"
  )
  miss <- setdiff(req_cols, names(df))
  if (length(miss)) for (m in miss) df[[m]] <- NA
  
  df <- df %>%
    dplyr::mutate(
      imput_id     = as.integer(imput_id),
      fecha        = as.Date(fecha),
      persona_id   = as.integer(persona_id),
      tipo_id      = as.integer(tipo_id),
      proyecto_id  = as.character(proyecto_id),
      pe_id        = as.character(pe_id),
      sm_id        = as.character(sm_id),
      rutina_id    = as.integer(rutina_id),
      sector_id    = as.integer(sector_id),
      horas        = as.numeric(horas),
      observ       = as.character(observ),
      anio         = dplyr::coalesce(as.integer(anio), ifelse(!is.na(fecha), lubridate::year(fecha), NA_integer_)),
      mes          = dplyr::coalesce(as.integer(mes),  ifelse(!is.na(fecha), lubridate::month(fecha), NA_integer_))
    )
  
  df[, req_cols]
}

# ---- Paleta corporativa ----
pal_emp <- c("#0C2340","#153E75","#1E5BA3","#3478C0","#5B8DC9","#8EB6E5","#D2E3F9")


#interfaz del usuario con dashboard
ui <-  dashboardPage(
  dashboardHeader(title = "Carga de horas - I&D"),
  #Defino panel lateral con pestañas
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carga de horas", tabName = "carga", icon = icon("clock")), #Carga de horas
      menuItem("Tablero", tabName = "tablero", icon = icon("chart-pie")), #Visualizacion
      menuItem("Maestros", tabName = "maestros", icon = icon("table")), #Control y tablas
      menuItem("Usos", tabName = "usos", icon = icon("info-circle")) #Instrucciones y usos
    )
  ),
  #Defino layout de cada pestaña
  dashboardBody(
    useShinyjs(),
    tabItems(
      
      # ===================== CARGA =====================
      tabItem(tabName = "carga",
              fluidRow(
                box(width = 12, title = "Carga rápida", status = "primary", solidHeader = TRUE,
                    column(2, selectInput("persona_id", "Persona", choices = NULL)),
                    column(2,
                           airDatepickerInput(
                             inputId = "fecha",
                             label   = "Fecha",
                             value   = Sys.Date(),
                             language = "es",
                             autoClose = TRUE
                           )
                    ),
                    column(2, selectInput("tipo_id", "Tipo de tarea", choices = NULL)),
                    column(3, uiOutput("ui_detalle_tarea")),
                    column(2, numericInput("horas", "Horas", value = 2, min = 0, step = 0.25)),
                    column(12, textInput("observ", "Observaciones (opcional)", value = "")),
                    column(12, actionButton("agregar", "Agregar fragmento", icon = icon("plus"), class = "btn btn-success")),
                    br(),
                    column(12, h4("Fragmentos del día"), DTOutput("tbl_dia")),
                    br(),
                    column(12, actionButton("cerrar_dia", "Cerrar día", icon = icon("lock"), class = "btn btn-warning"))
                )
              )
      ),
      
      # ==================== TABLERO ====================
      tabItem(tabName = "tablero",
              # Filtros generales
              fluidRow(
                box(width = 12, title = "Filtros", status = "primary", solidHeader = TRUE,
                    column(2, numericInput("anio", "Año", value = year(Sys.Date()), min = 2020, step = 1)),
                    column(2, selectInput("mes", "Mes",
                                          choices = setNames(1:12, c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                                                                     "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")),
                                          selected = month(Sys.Date()))),
                    column(2, checkboxInput("acumulado", "Ver acumulado del año", value = FALSE)),
                    column(3, selectInput("sector_fil", "Sector", choices = c("Todos" = "ALL"))),
                    column(3, selectInput("persona_fil", "Persona", choices = c("Todos" = "ALL")))
                )
              ),
              # KPIs (sin ahorro)
              fluidRow(
                box(width = 4, title = "Horas totales", status = "info", solidHeader = TRUE,
                    h2(textOutput("kpi_total")), "(sin almuerzo)"),
                box(width = 4, title = "Horas productivas", status = "info", solidHeader = TRUE,
                    h2(textOutput("kpi_prod")), "(sin rutinas)"),
                box(width = 4, title = "% Rutinas", status = "info", solidHeader = TRUE,
                    h2(textOutput("kpi_rutinas")))
              ),
              # Gráficos (se mantiene torta y comparativos; se quita evolución/ahorro)
              fluidRow(
                box(width = 6, title = "Horas por tipo de tarea (torta)", status = "primary", solidHeader = TRUE,
                    plotly::plotlyOutput("pie_tipo", height = "360px")),
                box(width = 6, status = "primary", solidHeader = TRUE,
                    div(style = "padding: 4px 0 8px 0;", h4("Horas acumuladas por tarea", style="margin-top:0; margin-bottom:8px;")),
                    fluidRow(
                      column(4, selectInput("tipo_graf", "Tipo",
                                            choices = c("Nuevos Proyectos","PE","SM","Rutinas de oficina"),
                                            selected = "Nuevos Proyectos")),
                      column(4, uiOutput("ui_detalle_graf")),
                      column(3, selectInput("persona_graf", "Persona", choices = c("Todos" = "ALL"))),
                      column(1, br(), actionButton("btn_graf_actualizar", NULL, icon = icon("sync"), title = "Actualizar"))
                    ),
                    plotly::plotlyOutput("graf_acumulado", height = "360px")
                )
              ),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    h4("Comparativo de horas acumuladas por Proyecto / SM / PE", style="margin-top:0; margin-bottom:8px;"),
                    fluidRow(
                      column(4, selectInput("persona_cmp", "Persona", choices = c("Todos"="ALL"))),
                      column(4, sliderInput("topN_cmp", "Mostrar TOP N tareas", min = 5, max = 50, value = 15, step = 1)),
                      column(4, checkboxGroupInput("tipos_cmp", "Tipos incluidos",
                                                   choices = c("Nuevos Proyectos","PE","SM"),
                                                   selected = c("Nuevos Proyectos","PE","SM"),
                                                   inline = TRUE))
                    ),
                    fluidRow(
                      column(6, selectizeInput(
                        "tarea_cmp", "Proyecto/PE/SM (opcional)",
                        choices = c("Todos"="ALL"), multiple = TRUE,
                        options = list(placeholder = "Seleccioná uno o más…")
                      )),
                      column(6, checkboxInput("cmp_hist", "Usar todo el histórico (ignorar Año/Mes)", value = FALSE))
                    ),
                    plotly::plotlyOutput("graf_comparativo", height = "420px")
                )
              ),
              # Drill-down
              fluidRow(
                box(width = 12, title = "Drill-down: Detalle por proyecto / PE / SM", status = "primary", solidHeader = TRUE,
                    column(3, selectInput("tipo_det", "Tipo", choices = c("Nuevos Proyectos","PE","SM","Rutinas de oficina"))),
                    column(3, uiOutput("ui_detalle_drill")),
                    column(3, selectInput("persona_det", "Persona", choices = c("Todos" = "ALL"))),
                    column(3, selectInput("sector_det", "Sector", choices = c("Todos" = "ALL"))),
                    DTOutput("tbl_drill")
                )
              )
      ),
      
      # =================== MAESTROS ===================
      tabItem(tabName = "maestros",
              fluidRow(
                box(width = 7, title = "Personal (todas las personas — activas/inactivas)", status = "primary", solidHeader = TRUE,
                    DTOutput("cat_personal")),
                box(width = 5, title = "Editar / Agregar persona", status = "primary", solidHeader = TRUE,
                    textInput("m_pers_nombre", "Nombre y apellido"),
                    selectInput("m_pers_sector", "Sector", choices = NULL),
                    # (almuerzo eliminado del maestro)
                    numericInput("m_pers_jornada", "Jornada (hs)", value = 8.5, min = 1, step = 0.5),
                    numericInput("m_pers_vac", "Vacaciones anuales (días)", value = 14, min = 0, step = 1),
                    checkboxInput("m_pers_activo", "Activo", value = TRUE),
                    shinyjs::hidden(textInput("m_pers_id", label = NULL)),
                    actionButton("m_btn_guardar", "Guardar / Actualizar", icon = icon("save"), class = "btn btn-success"),
                    )
              ),
              fluidRow(
                box(width = 6, title = "Tipos / Sectores", status = "primary", solidHeader = TRUE,
                    DTOutput("cat_tipos"), br(), DTOutput("cat_sectores")),
                box(width = 6, title = "Proyectos / Servicios / SM / Rutinas", status = "primary", solidHeader = TRUE,
                    DTOutput("cat_proyectos"), br(), DTOutput("cat_servicios"), br(), DTOutput("cat_sm"), br(), DTOutput("cat_rutinas"))
              )
              # (Feriados eliminado de Maestros)
      ),
      
      # ===================== USOS =====================
      tabItem(tabName = "usos",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("circle-question"), "Cómo usar la app"),
                  status = "primary", solidHeader = TRUE,
                  HTML('
      <style>
        .uso-tip{background:#f6f8fa;border-left:4px solid #0d6efd;padding:10px 12px;margin:10px 0;border-radius:6px}
        .uso-badge{display:inline-block;padding:2px 8px;border-radius:999px;background:#e9ecef;margin-left:6px;font-size:12px}
        .uso-ico{width:18px;height:18px;margin-right:6px;vertical-align:middle}
        .uso-kv code{background:#0000000d;padding:2px 6px;border-radius:4px}
        .uso-small{color:#6c757d;font-size:13px}
      </style>

      <h4><i class="fa fa-compass uso-ico"></i>Vista general</h4>
      <p>La aplicación permite registrar y analizar la <strong>carga diaria de horas</strong> de trabajo.
      A partir de las imputaciones, el <strong>Tablero</strong> muestra indicadores y gráficos, y los
      <strong>Maestros</strong> mantienen los catálogos base (personas, sectores, tipos, proyectos, PE, SM y rutinas).</p>

      <div class="uso-tip"><i class="fa fa-lightbulb"></i> Consejo: mantené los maestros actualizados para facilitar la carga y evitar errores de selección.</div>

      <hr>
      <h4><i class="fa fa-clock uso-ico"></i> 1) Carga de horas</h4>
      <ul>
        <li>Seleccioná la <em>Persona</em>, la <em>Fecha</em>, el <em>Tipo de tarea</em> y el <em>detalle</em> correspondiente:
          <span class="uso-badge">NP</span>
          <span class="uso-badge">PE</span>
          <span class="uso-badge">SM</span>
          <span class="uso-badge">Rutina</span>.
        </li>
        <li>Indicá las <em>Horas</em> trabajadas y, si querés, agregá <em>Observaciones</em>.</li>
        <li>Usá <strong>Agregar fragmento</strong> para cargar registros parciales del día.</li>
        <li>El panel <em>Fragmentos del día</em> muestra los registros cargados temporalmente.</li>
        <li><strong>Cerrar día</strong> guarda todo lo pendiente en <code>data/imputaciones.csv</code> sin validación horaria previa.</li>
        <li class="uso-small">Campos guardados: <code>imput_id, fecha, persona_id, tipo_id, proyecto_id, pe_id, sm_id, rutina_id, sector_id, horas, observ, anio, mes</code>.</li>
      </ul>

      <hr>
      <h4><i class="fa fa-chart-pie uso-ico"></i> 2) Tablero</h4>
      <ul>
        <li>Usá los filtros superiores por <em>Año</em>, <em>Mes</em> o modo <em>Acumulado</em>, además de <em>Sector</em> y <em>Persona</em>.</li>
        <li>KPI disponibles:
          <span class="uso-badge">Horas totales</span>
          <span class="uso-badge">Horas productivas</span>
          <span class="uso-badge">% Rutinas</span>.
        </li>
        <li>Gráficos disponibles:
          <ul>
            <li><strong>Torta</strong> de horas por tipo de tarea.</li>
            <li><strong>Barras</strong> de horas acumuladas por proyecto/SM/PE o rutina.</li>
            <li><strong>Comparativo</strong> de TOP-N tareas seleccionadas.</li>
            <li><strong>Drill-down</strong> con detalle filtrable por tipo, persona y sector.</li>
          </ul>
        </li>
        <li>Los nombres de tareas (NP/PE/SM) se guardan como texto, garantizando independencia de los catálogos.</li>
      </ul>

      <hr>
      <h4><i class="fa fa-database uso-ico"></i> 3) Maestros generales</h4>
      <ul>
        <li>Permiten mantener los catálogos de:
          <span class="uso-badge">Personal</span>
          <span class="uso-badge">Sectores</span>
          <span class="uso-badge">Tipos</span>
          <span class="uso-badge">Proyectos</span>
          <span class="uso-badge">PE</span>
          <span class="uso-badge">SM</span>
          <span class="uso-badge">Rutinas</span>.
        </li>
        <li>Los cambios se reflejan automáticamente en los selectores de Carga de horas.</li>
     
      </ul>

      <hr>
      <h4><i class="fa fa-users-gear uso-ico"></i> 4) Maestro de Personal</h4>
      <ul>
        <li>Desde <strong>Maestros → Personal</strong> podés crear, editar o desactivar usuarios.</li>
        <li>Campos editables: <span class="uso-badge">Nombre</span> <span class="uso-badge">Sector</span> <span class="uso-badge">Jornada (hs)</span> <span class="uso-badge">Vacaciones</span> <span class="uso-badge">Activo/Inactivo</span>.</li>
        <li><strong>Guardar</strong> actualiza el CSV <code>data/personal.csv</code>.</li>
        <li><strong>Desactivar</strong> conserva el historial pero impide nuevas imputaciones.</li>
      </ul>

      <hr>
      <h4><i class="fa fa-link uso-ico"></i> 5) Identificadores y relaciones</h4>
      <ul>
        <li><code>imputaciones.csv</code>: contiene las imputaciones diarias, vinculadas por <code>persona_id</code>, <code>sector_id</code> y <code>tipo_id</code>.</li>
        <li>Los campos <code>proyecto_id</code>, <code>pe_id</code> y <code>sm_id</code> se almacenan como texto para mantener trazabilidad histórica.</li>
        <li><code>rutina_id</code> referencia al catálogo <strong>rutinas.csv</strong>.</li>
      </ul>

      <hr>
      <h4><i class="fa fa-folder-open uso-ico"></i> 6) Archivos y rutas</h4>
      <p>Los archivos CSV principales se guardan en <code>data/</code>:</p>
      <ul>
        <li><code>imputaciones.csv</code> (registro de horas)</li>
        <li><code>personal.csv</code> (personas)</li>
        <li><code>sectores.csv</code> (sectores)</li>
        <li><code>tipos_tarea.csv</code> (tipos de tarea)</li>
        <li><code>proyectos.csv</code>, <code>servicios_pe.csv</code>, <code>sm.csv</code>, <code>rutinas.csv</code></li>
      </ul>
      <p class="uso-small">Los archivos se crean automáticamente si no existen. Pueden editarse externamente en Excel o cualquier editor de texto.</p>
      ')
                )
              )
      )

  )
  )
)



#funcion server
server <- function(input, output, session){
  # ---------- Carga de catálogos ----------
  cats <- reactiveVal(load_catalogs())
  
  # ---------- Helper de etiquetas ----------
  #Funcion que toma el data frame que toma el id y lo guarda en una nueva
  #columna descriptiva
  with_labels <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(tibble())
    tipo_map <- if (nrow(cats()$tipos))     setNames(cats()$tipos$tipo,     cats()$tipos$tipo_id)   else c()
    ru_map   <- if (nrow(cats()$rutinas))   setNames(cats()$rutinas$rutina, cats()$rutinas$rutina_id) else c()
    df %>%
      dplyr::mutate(
        tipo_label = unname(tipo_map[as.character(.data$tipo_id)]),
        tarea_label = dplyr::case_when(
          tipo_label == "Nuevos Proyectos"   ~ as.character(.data$proyecto_id),
          tipo_label == "PE"                 ~ as.character(.data$pe_id),
          tipo_label == "SM"                 ~ as.character(.data$sm_id),
          tipo_label == "Rutinas de oficina" ~ unname(ru_map[as.character(.data$rutina_id)]),
          TRUE ~ NA_character_
        )
      )
  }

  #' ---------- Selectores iniciales ----------
  #' Coloca en los selectores los datos cargados en los catalogos
  observe({
    updateSelectInput(session, "tipo_id",
                      choices = setNames(cats()$tipos$tipo_id, cats()$tipos$tipo))
    updateSelectInput(session, "sector_fil",
                      choices = c("Todos" = "ALL", setNames(cats()$sectores$sector_id, cats()$sectores$sector)))
    updateSelectInput(session, "sector_det",
                      choices = c("Todos" = "ALL", setNames(cats()$sectores$sector_id, cats()$sectores$sector)))
    
    pers_act <- cats()$personal %>% dplyr::filter(is.na(activo) | activo == TRUE)
    updateSelectInput(session, "persona_id",
                      choices = setNames(pers_act$persona_id, pers_act$persona))
    updateSelectInput(session, "persona_fil",
                      choices = c("Todos" = "ALL", setNames(pers_act$persona_id, pers_act$persona)))
    updateSelectInput(session, "persona_det",
                      choices = c("Todos" = "ALL", setNames(pers_act$persona_id, pers_act$persona)))
    updateSelectInput(session, "m_pers_sector",
                      choices = setNames(cats()$sectores$sector_id, cats()$sectores$sector))
    
    updateSelectInput(session, "persona_graf",
                      choices = c("Todos" = "ALL", setNames(pers_act$persona_id, pers_act$persona)))
    updateSelectInput(session, "persona_cmp",
                      choices = c("Todos" = "ALL", setNames(pers_act$persona_id, pers_act$persona)))
  })  
  
  #' ---------- UI dependiente del tipo (carga rápida) ----------
  #' Muestra los catalogos segun el tipo de tarea seleccionado
  output$ui_detalle_tarea <- renderUI({
    req(input$tipo_id)
    t <- cats()$tipos %>% dplyr::filter(tipo_id == input$tipo_id) %>% dplyr::pull(tipo)
    if (t == "Nuevos Proyectos") {
      selectInput("detalle_id", "Proyecto (NP)", choices = setNames(cats()$proyectos$proyecto, cats()$proyectos$proyecto))
    } else if (t == "PE") {
      selectInput("detalle_id", "PE", choices = setNames(cats()$servicios$pe_desc, cats()$servicios$pe_desc))
    } else if (t == "SM") {
      selectInput("detalle_id", "SM", choices = setNames(cats()$sm$sm_desc, cats()$sm$sm_desc))
    } else if (t == "Rutinas de oficina") {
      selectInput("detalle_id", "Rutina", choices = setNames(cats()$rutinas$rutina_id, cats()$rutinas$rutina))
    } else NULL
  })
  # ---------- Buffer de fragmentos ----------
  frag <- reactiveVal(tibble())
  
  #Validacion de datos al agregar imputaciones y guardarlos en una taba
  observeEvent(input$agregar, {
    req(input$persona_id, input$fecha, input$tipo_id, input$detalle_id, input$horas)
    sector_id <- cats()$personal %>%
      dplyr::filter(persona_id == input$persona_id) %>% dplyr::pull(sector_id) %>% dplyr::first()
    tipo_txt <- cats()$tipos %>% dplyr::filter(tipo_id == input$tipo_id) %>% dplyr::pull(tipo)
    
    proyecto_id <- pe_id <- sm_id <- NA_character_
    rutina_id   <- NA_integer_
    if (tipo_txt == "Nuevos Proyectos")   proyecto_id <- as.character(input$detalle_id)
    if (tipo_txt == "PE")                 pe_id       <- as.character(input$detalle_id)
    if (tipo_txt == "SM")                 sm_id       <- as.character(input$detalle_id)
    if (tipo_txt == "Rutinas de oficina") rutina_id   <- as.integer(input$detalle_id)
    
    df <- tibble(
      imput_id     = as.integer(runif(1, 1e6, 2e6)),
      fecha        = as.Date(input$fecha),
      persona_id   = as.integer(input$persona_id),
      tipo_id      = as.integer(input$tipo_id),
      proyecto_id  = proyecto_id,
      pe_id        = pe_id,
      sm_id        = sm_id,
      rutina_id    = as.integer(rutina_id),
      sector_id    = as.integer(sector_id),
      horas        = as.numeric(input$horas),
      observ       = as.character(input$observ),
      anio         = lubridate::year(as.Date(input$fecha)),
      mes          = lubridate::month(as.Date(input$fecha))
    )
    frag(bind_rows(frag(), df))
  })
  
#'Tomo los fragmentos cargados y los renderizo para mistralos  
  output$tbl_dia <- renderDT({
    f <- frag()
    if (nrow(f) == 0) return(datatable(tibble(mensaje = "Sin fragmentos cargados"), options = list(dom = 't')))
    pers_map <- if (nrow(cats()$personal)) setNames(cats()$personal$persona, cats()$personal$persona_id) else c()
    f %>%
      with_labels() %>%
      dplyr::mutate(persona = unname(pers_map[as.character(persona_id)])) %>%
      dplyr::select(fecha, persona, tipo = tipo_label, detalle = tarea_label, horas, observ) %>%
      datatable(options = list(pageLength = 5), rownames = FALSE)
  })
  
  #Bloque de cerrar dia
  observeEvent(input$cerrar_dia, {
    shinyjs::disable("cerrar_dia"); on.exit(shinyjs::enable("cerrar_dia"), add = TRUE)
    f <- frag()
    if (nrow(f) == 0) {
      showModal(modalDialog(title = "Nada para cerrar", "No hay fragmentos para este día.", easyClose = TRUE))
      return(NULL)
    }
    
    #' Guarda los fragmentos del dia
    #' Limpia el buffer de fragmentos
    #' Muestra mensaje de dia cerrado
    #' Si falla muestra error
    tryCatch({
      save_append(f, f_imput)
      frag(f[0,])
      showModal(modalDialog(title = "Día cerrado", "Se guardaron las imputaciones.", easyClose = TRUE))
    }, error = function(e){
      showModal(modalDialog(title = "Error al cerrar el día", paste("Detalle:", conditionMessage(e)), easyClose = TRUE))
    })
  }, ignoreInit = TRUE)  
  
  
  # ---------- Tablero / KPIs ----------
  imput_all <- reactive({ normalize_imput_types(safe_read(f_imput)) })
  
  base_filtrada <- reactive({
    df <- imput_all(); if (nrow(df) == 0) return(tibble())
    if (isTRUE(input$acumulado)) {
      df <- df %>% dplyr::filter(anio == input$anio, mes <= as.integer(input$mes))
    } else {
      df <- df %>% dplyr::filter(anio == input$anio, mes == as.integer(input$mes))
    }
    if (!is.null(input$sector_fil) && input$sector_fil != "ALL")
      df <- df %>% dplyr::filter(sector_id == as.integer(input$sector_fil))
    if (!is.null(input$persona_fil) && input$persona_fil != "ALL")
      df <- df %>% dplyr::filter(persona_id == as.integer(input$persona_fil))
    df
  })
  output$kpi_total <- renderText({
    df <- base_filtrada(); if (nrow(df)==0) return("0")
    total <- sum(df$horas, na.rm = TRUE)
    sprintf("%.1f h", total)
  })
  
  output$kpi_prod <- renderText({
    df <- base_filtrada(); if (nrow(df)==0) return("0")
    rut_id <- cats()$tipos %>% dplyr::filter(tipo == "Rutinas de oficina") %>% dplyr::pull(tipo_id)
    prod <- df %>% dplyr::filter(tipo_id != rut_id) %>% summarise(h=sum(horas, na.rm = TRUE)) %>% dplyr::pull(h)
    sprintf("%.1f h", ifelse(length(prod)==0, 0, prod))
  })
  
  output$kpi_rutinas <- renderText({
    df <- base_filtrada(); if (nrow(df)==0) return("0%")
    rut_id <- cats()$tipos %>% dplyr::filter(tipo == "Rutinas de oficina") %>% dplyr::pull(tipo_id)
    tot <- sum(df$horas, na.rm = TRUE)
    rut <- df %>% dplyr::filter(tipo_id == rut_id) %>% summarise(h=sum(horas, na.rm = TRUE)) %>% dplyr::pull(h)
    pct <- ifelse(tot>0, 100*rut/tot, 0)
    sprintf("%.0f%%", pct)
  })
  
  
  # ---------- Poblar selector comparativo ----------
  observe({
    df_all <- imput_all()
    if (nrow(df_all) == 0) {
      updateSelectizeInput(session, "tarea_cmp", choices = c("Todos"="ALL"), server = TRUE)
      return(NULL)
    }
    lab <- with_labels(df_all) %>%
      dplyr::filter(!is.na(tarea_label), nzchar(tarea_label)) %>%
      dplyr::arrange(tarea_label) %>% dplyr::pull(tarea_label) %>% unique()
    updateSelectizeInput(session, "tarea_cmp", choices = c("Todos"="ALL", lab), server = TRUE, selected = "ALL")
  })
  
  
  # ---------- Torta ----------
  output$pie_tipo <- plotly::renderPlotly({
    df <- base_filtrada(); if (nrow(df)==0) return(NULL)
    df <- with_labels(df) %>%
      dplyr::group_by(tipo_label) %>%
      dplyr::summarise(horas = sum(horas, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(horas))
    plotly::plot_ly(
      df,
      labels = ~tipo_label,
      values = ~horas,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      textposition = "outside",
      hovertemplate = "%{label}: %{value:.1f} h (%{percent:.1%})<extra></extra>",
      marker = list(colors = pal_emp, line = list(color = "#FFFFFF", width = 1))
    ) %>%
      plotly::layout(
        title = list(text = "Horas por tipo de tarea", x = 0.5, y = 0.96, xanchor = "center"),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.15),
        margin = list(l = 20, r = 20, t = 70, b = 20),
        uniformtext = list(minsize = 12, mode = "hide"),
        template = "plotly_white"
      )
  })
  
  # ---------- Acumulado por tarea ----------
  output$ui_detalle_graf <- renderUI({
    t <- input$tipo_graf; if (is.null(t)) t <- "Nuevos Proyectos"
    if (t == "Nuevos Proyectos") {
      selectInput("detalle_graf", "Proyecto (NP)",
                  choices = c("Todos"="ALL", setNames(cats()$proyectos$proyecto, cats()$proyectos$proyecto)))
    } else if (t == "PE") {
      selectInput("detalle_graf", "PE",
                  choices = c("Todos"="ALL", setNames(cats()$servicios$pe_desc, cats()$servicios$pe_desc)))
    } else if (t == "SM") {
      selectInput("detalle_graf", "SM",
                  choices = c("Todos"="ALL", setNames(cats()$sm$sm_desc, cats()$sm$sm_desc)))
    } else {
      selectInput("detalle_graf", "Rutina",
                  choices = c("Todos"="ALL", setNames(cats()$rutinas$rutina_id, cats()$rutinas$rutina)))
    }
  })
  
  df_graf_base <- reactive({ base_filtrada() })
  
  df_graf_final <- eventReactive(input$btn_graf_actualizar, {
    df <- df_graf_base(); if (nrow(df) == 0) return(tibble())
    tipo_txt <- input$tipo_graf; if (is.null(tipo_txt)) tipo_txt <- "Nuevos Proyectos"
    tipo_id_sel <- cats()$tipos %>% dplyr::filter(tipo == tipo_txt) %>% dplyr::pull(tipo_id)
    df <- df %>% dplyr::filter(tipo_id == tipo_id_sel)
    
    col_det <- switch(tipo_txt,
                      "Nuevos Proyectos"   = "proyecto_id",
                      "PE"                 = "pe_id",
                      "SM"                 = "sm_id",
                      "Rutinas de oficina" = "rutina_id",
                      NA_character_)
    if (!is.null(input$detalle_graf) && input$detalle_graf != "ALL" && !is.na(col_det)) {
      if (tipo_txt %in% c("Nuevos Proyectos","PE","SM")) {
        df <- df %>% dplyr::filter(.data[[col_det]] == as.character(input$detalle_graf))
      } else {
        df <- df %>% dplyr::filter(.data[[col_det]] == as.integer(input$detalle_graf))
      }
    }
    if (!is.null(input$persona_graf) && input$persona_graf != "ALL") {
      df <- df %>% dplyr::filter(persona_id == as.integer(input$persona_graf))
    }
    
    df %>%
      dplyr::group_by(fecha) %>% dplyr::summarise(horas = sum(horas, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(fecha) %>% dplyr::mutate(acum = cumsum(horas))
  }, ignoreInit = TRUE)
  
  output$graf_acumulado <- plotly::renderPlotly({
    df <- df_graf_final(); if (is.null(df) || nrow(df) == 0) return(NULL)
    plotly::plot_ly() %>%
      add_bars(data = df, x = ~fecha, y = ~acum,
               name = "Horas acumuladas",
               marker = list(color = pal_emp[3], line = list(color = "#FFFFFF", width = 0.5)),
               opacity = 0.7,
               hovertemplate = "%{x|%d-%m-%Y}: %{y:.1f} h (acum)<extra></extra>"
      ) %>%
      add_trace(data = df, x = ~fecha, y = ~acum, type = "scatter", mode = "lines+markers",
                line = list(width = 3, shape = "linear", color = pal_emp[1]),
                marker = list(size = 6, color = pal_emp[1]),
                name = "Tendencia acumulada",
                hovertemplate = "%{x|%d-%m-%Y}: %{y:.1f} h (acum)<extra></extra>"
      ) %>%
      layout(
        title = list(text = "Horas acumuladas por tarea", x = 0.5, y = 0.96, xanchor = "center"),
        xaxis = list(title = "Fecha", tickformat = "%d-%m", showgrid = FALSE),
        yaxis = list(title = "Horas acumuladas", zeroline = FALSE),
        hovermode = "x unified",
        barmode = "overlay",
        bargap = 0.15,
        margin = list(l = 70, r = 20, t = 70, b = 60),
        template = "plotly_white",
        legend = list(orientation = "h", y = -0.2)
      )
  })
  # ---------- Drill-down ----------
  output$ui_detalle_drill <- renderUI({
    t <- input$tipo_det
    if (t == "Nuevos Proyectos") {
      selectInput("detalle_drill", "Proyecto (NP)", choices = c("Todos"="ALL", setNames(cats()$proyectos$proyecto, cats()$proyectos$proyecto)))
    } else if (t == "PE") {
      selectInput("detalle_drill", "Servicio (PE)", choices = c("Todos"="ALL", setNames(cats()$servicios$pe_desc, cats()$servicios$pe_desc)))
    } else if (t == "SM") {
      selectInput("detalle_drill", "SM", choices = c("Todos"="ALL", setNames(cats()$sm$sm_desc, cats()$sm$sm_desc)))
    } else {
      selectInput("detalle_drill", "Rutina", choices = c("Todos"="ALL", setNames(cats()$rutinas$rutina_id, cats()$rutinas$rutina)))
    }
  })
  
  output$tbl_drill <- renderDT({
    df <- base_filtrada(); if (nrow(df) == 0) return(datatable(tibble()))
    t <- input$tipo_det
    tipo_id_sel <- cats()$tipos %>% dplyr::filter(tipo == t) %>% dplyr::pull(tipo_id)
    df <- df %>% dplyr::filter(tipo_id == tipo_id_sel)
    
    if (!is.null(input$detalle_drill) && input$detalle_drill != "ALL") {
      if (t == "Nuevos Proyectos")   df <- df %>% dplyr::filter(proyecto_id == as.character(input$detalle_drill))
      if (t == "PE")                  df <- df %>% dplyr::filter(pe_id      == as.character(input$detalle_drill))
      if (t == "SM")                  df <- df %>% dplyr::filter(sm_id      == as.character(input$detalle_drill))
      if (t == "Rutinas de oficina")  df <- df %>% dplyr::filter(rutina_id  == as.integer(input$detalle_drill))
    }
    if (!is.null(input$persona_det) && input$persona_det != "ALL")
      df <- df %>% dplyr::filter(persona_id == as.integer(input$persona_det))
    if (!is.null(input$sector_det) && input$sector_det != "ALL")
      df <- df %>% dplyr::filter(sector_id == as.integer(input$sector_det))
    
    pers_map <- if (nrow(cats()$personal)) setNames(cats()$personal$persona, cats()$personal$persona_id) else c()
    sect_map <- if (nrow(cats()$sectores)) setNames(cats()$sectores$sector,  cats()$sectores$sector_id)  else c()
    
    df %>%
      with_labels() %>%
      dplyr::mutate(
        persona = unname(pers_map[as.character(persona_id)]),
        sector  = unname(sect_map[as.character(sector_id)])
      ) %>%
      dplyr::select(fecha, persona, sector, tipo = tipo_label, detalle = tarea_label, horas, observ) %>%
      dplyr::arrange(dplyr::desc(fecha)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ===== Comparativo: datos agregados =====
  df_cmp <- reactive({
    base <- if (isTRUE(input$cmp_hist)) imput_all() else base_filtrada()
    if (is.null(base) || nrow(base) == 0) return(tibble())
    df <- with_labels(base)
    
    if (!is.null(input$persona_cmp) && !identical(input$persona_cmp, "ALL"))
      df <- df %>% dplyr::filter(persona_id == as.integer(input$persona_cmp))
    if (!is.null(input$tipos_cmp) && length(input$tipos_cmp) > 0)
      df <- df %>% dplyr::filter(tipo_label %in% input$tipos_cmp)
    if (!is.null(input$tarea_cmp) && length(input$tarea_cmp) > 0 && !("ALL" %in% input$tarea_cmp))
      df <- df %>% dplyr::filter(tarea_label %in% input$tarea_cmp)
    
    df %>%
      dplyr::filter(!is.na(tarea_label)) %>%
      dplyr::group_by(tipo_label, tarea_label) %>%
      dplyr::summarise(horas = sum(horas, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(horas))
  })
  
  # ===== Comparativo: gráfico =====
  output$graf_comparativo <- plotly::renderPlotly({
    df <- df_cmp(); if (is.null(df) || nrow(df) == 0) return(NULL)
    topN <- ifelse(is.null(input$topN_cmp), 15, input$topN_cmp)
    df <- df %>% dplyr::slice_head(n = topN) %>% dplyr::arrange(horas)
    
    plotly::plot_ly(
      df, x = ~horas, y = ~tarea_label,
      color = ~tipo_label, type = "bar", orientation = "h",
      text = ~sprintf("%.1f h", horas), textposition = "outside",
      hovertemplate = "%{y}<br>%{x:.1f} h<br>%{marker.color}<extra></extra>",
      colors = pal_emp
    ) %>%
      plotly::layout(
        title = list(text = "Comparativo de horas acumuladas", x = 0.5, y = 0.96, xanchor = "center"),
        xaxis = list(title = "Horas", zeroline = FALSE),
        yaxis = list(title = "", categoryorder = "array", categoryarray = df$tarea_label),
        legend = list(orientation = "h"),
        margin = list(l = 220, r = 30, t = 70, b = 40),
        barmode = "group",
        template = "plotly_white"
      )
  })
  
  # ---------- Maestros: tablas y acciones ----------
  proxy_pers <- DT::dataTableProxy("cat_personal")
  
  output$cat_personal  <- DT::renderDT({
    df <- cats()$personal
    if (nrow(df) == 0) return(DT::datatable(tibble(mensaje = "Sin datos"), options = list(dom='t'), rownames = FALSE))
    DT::datatable(df, selection = "single", options = list(pageLength = 7), rownames = FALSE)
  })
  
  # >>> Maestros: al seleccionar una fila en "Personal", rellenar el formulario
  observeEvent(input$cat_personal_rows_selected, {
    idx <- input$cat_personal_rows_selected
    req(length(idx) == 1)
    
    row <- cats()$personal[idx, , drop = FALSE]
    
    # Campos esperados en cats()$personal:
    # persona_id, persona, sector_id, almuerza, jornada_hs, vacaciones_anuales, activo
    
    updateTextInput(   session, "m_pers_nombre",    value = row$persona)
    updateSelectInput( session, "m_pers_sector",    selected = row$sector_id)
    # updateCheckboxInput(session, "m_pers_almuerza", value = isTRUE(row$almuerza))
    updateNumericInput(session, "m_pers_jornada",   value = as.numeric(row$jornada_hs))
    updateNumericInput(session, "m_pers_vac",       value = as.numeric(row$vacaciones_anuales))
    updateCheckboxInput(session, "m_pers_activo",   value = isTRUE(row$activo))
    
    # Guardar el ID oculto para saber qué registro editar
    updateTextInput(session, "m_pers_id", value = as.character(row$persona_id))
  }, ignoreInit = TRUE)
  
  output$cat_tipos  <- DT::renderDT({
    df <- cats()$tipos
    if (nrow(df) == 0) {
      return(DT::datatable(
        tibble(mensaje = "Sin datos"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    DT::datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$cat_sectores  <- DT::renderDT({
    df <- cats()$sectores
    if (nrow(df) == 0) {
      return(DT::datatable(
        tibble(mensaje = "Sin datos"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    DT::datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$cat_proyectos <- DT::renderDT({
    df <- cats()$proyectos
    if (nrow(df) == 0) {
      return(DT::datatable(
        tibble(mensaje = "Sin datos"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    DT::datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$cat_servicios <- DT::renderDT({
    df <- cats()$servicios
    if (nrow(df) == 0) {
      return(DT::datatable(
        tibble(mensaje = "Sin datos"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    DT::datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$cat_sm        <- DT::renderDT({
    df <- cats()$sm
    if (nrow(df) == 0) {
      return(DT::datatable(
        tibble(mensaje = "Sin datos"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    DT::datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$cat_rutinas   <- DT::renderDT({
    df <- cats()$rutinas
    if (nrow(df) == 0) {
      return(DT::datatable(
        tibble(mensaje = "Sin datos"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    DT::datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })

  observeEvent(input$m_btn_guardar, {
    req(input$m_pers_nombre, input$m_pers_sector, input$m_pers_jornada, input$m_pers_vac)
    
    df <- cats()$personal
    
    if (nzchar(input$m_pers_id)) {
      # modificar existente
      pid <- as.integer(input$m_pers_id)
      df <- df %>%
        dplyr::mutate(
          persona            = ifelse(persona_id == pid, input$m_pers_nombre, persona),
          sector_id          = ifelse(persona_id == pid, as.integer(input$m_pers_sector), sector_id),
          jornada_hs         = ifelse(persona_id == pid, as.numeric(input$m_pers_jornada), jornada_hs),
          vacaciones_anuales = ifelse(persona_id == pid, as.numeric(input$m_pers_vac), vacaciones_anuales),
          activo             = ifelse(persona_id == pid, isTRUE(input$m_pers_activo), activo)
        )
    } else {
      # alta nueva
      new_id <- ifelse(nrow(df) > 0, max(df$persona_id, na.rm = TRUE) + 1L, 1L)
      nuevo <- tibble::tibble(
        persona_id         = new_id,
        persona            = input$m_pers_nombre,
        sector_id          = as.integer(input$m_pers_sector),
        jornada_hs         = as.numeric(input$m_pers_jornada),
        vacaciones_anuales = as.numeric(input$m_pers_vac),
        activo             = isTRUE(input$m_pers_activo)
      )
      df <- dplyr::bind_rows(df, nuevo)
      updateTextInput(session, "m_pers_id", value = as.character(new_id))
    }
    
    save_overwrite(df, f_personal)   # definida en global.R
    cats(load_catalogs())            # recarga catálogos desde disco
    DT::replaceData(proxy_pers, cats()$personal, resetPaging = FALSE)
    showNotification("Personal guardado/actualizado", type = "message")
  })
  
}

#publicacion
shinyApp(ui, server)
