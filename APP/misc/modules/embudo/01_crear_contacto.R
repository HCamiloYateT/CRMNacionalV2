# CrearContacto
CrearContactoUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    box(title = "Nuevo Contacto", width = 12, collapsible = FALSE,
        ListaDesplegable(ns("CampoAutorizaDatos"), label = Obligatorio("Autoriza Tratamiento de Datos"),
                         choices = c("SI", "NO"), selected = "SI", multiple = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("CampoAutorizaDatos"), "'] == 'SI'"),
          fluidRow(
            column(6, 
                   textInput(ns("CampoNit"), label = h6("NIT"),
                             placeholder = "Sin dígito de verificación", width = "100%")
                   ),
            column(6, 
                   textInput(ns("CampoRazonSocial"), label = h6("Razón Social"), width = "100%")
                   )
            ),
          fluidRow(
            column(12, 
                   FormatearTexto("* Debe diligenciar Razón Social, NIT o ambos",
                                  tamano_pct = 0.75, color = "grey")
                   )
            ),
          fluidRow(
            column(6, 
                   uiOutput(ns("ValidacionNit"))
                   ),
            column(6, 
                   uiOutput(ns("ValidacionRazonSocial"))
                   )
            ),
          fluidRow(
            column(6, 
                   ListaDesplegable(ns("CampoOrigen"), label = Obligatorio("Origen del Contacto"),
                                    choices = Choices()$origen, selected = NULL, multiple = FALSE)
                   ),
            column(6,
                   shinyjs::hidden(
                     div(id = ns("WrapDetalleOrigen"),
                         ListaDesplegable(ns("CampoDetalleOrigen"), label = Obligatorio("Detalle del Origen"),
                                          choices = "", selected = "", multiple = FALSE)
                         )
                     )
                   )
            ),
          racafeShiny::Boton(id = ns("BotonGuardar"), label = "Crear Contacto", icono = "save", align = "right", 
                             size = "xs", color_fondo = "#C11007", color_fuente = "#FFFFFF")
          )
        )
    )
}
CrearContacto <- function(id, usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    contador_creaciones <- reactiveVal(0)
    codigo_creado <- reactiveVal(NULL)
    detalle_es_requerido <- reactiveVal(FALSE)
    
    # Funciones ----
    # Normalizacion de texto.
    .NormalizarRazonSocial <- function(texto) {
      if (EsVacio(texto)) return(NA_character_)
      str_squish(texto) %>% str_to_upper()
    }
    # Validacion de campos obligatorios y unicidad de NIT antes de guardar
    .ValidarCamposFormulario <- function() {
      
      nit_ingresado <- input$CampoNit %||% ""
      nit_es_entero_valido <- !EsVacio(nit_ingresado) && EsEnteroPositivo(nit_ingresado)
      
      c("Debe diligenciar Razón Social, NIT o ambos" = EsVacio(input$CampoRazonSocial) && EsVacio(nit_ingresado),
        "El NIT debe ser un valor numérico válido" = !EsVacio(nit_ingresado) && !EsEnteroPositivo(nit_ingresado),
        "El campo Origen es obligatorio" = EsVacio(input$CampoOrigen),
        "El campo detalle del origen del contacto es obligatorio" = detalle_es_requerido() && EsVacio(input$CampoDetalleOrigen),
        "El NIT ya existe como contacto activo" = nit_es_entero_valido && nit_ingresado %in% todos_contactos()$PerCod,
        "El NIT ya existe en CRMNALMARLOT" = nit_es_entero_valido && nit_ingresado %in% nits_marlot()
      )
    }
    # Limpiar formulario.
    .LimpiarFormulario <- function() {
      updateTextInput(session = session, inputId = "CampoRazonSocial", value = "")
      updateTextInput(session = session, inputId = "CampoNit", value = "")
      updatePickerInput(session = session, inputId = "CampoAutorizaDatos", selected = "SI")
      updatePickerInput(session = session, inputId = "CampoOrigen", selected = "")
      updatePickerInput(session = session, inputId = "CampoDetalleOrigen", choices = "", selected = "")
    }
    
    # Reactivos ----
    ## Datos ----
    todos_contactos <- reactive({
      CargarDatos("CRMNALCONTACTO")
    })
    nits_marlot <- reactive({
      CargarDatos("CRMNALMARLOT")$CLIENTE
    })
    ## Inputs ----
    observeEvent(input$CampoOrigen, {
      req(input$CampoOrigen)
      t1 <- tryCatch(
        CargarDatos("CRMNALORILEAD") %>% filter(Estado == "A", Origen == input$CampoOrigen, !is.na(Detalle)),
        error = function(e) NULL
      )
      tiene_detalle <- !is.null(t1) && nrow(t1) > 0
      cho <- if (tiene_detalle) Unicos(t1$Detalle) else ""
      
      detalle_es_requerido(tiene_detalle)
      updatePickerInput(session, "CampoDetalleOrigen", choices = cho, selected = NULL,
                        options = shinyWidgets::pickerOptions(size = min(10, max(length(cho), 1)))
      )
      
      if (tiene_detalle) shinyjs::show("WrapDetalleOrigen") else shinyjs::hide("WrapDetalleOrigen")
    })
    # Validaciones ----
    output$ValidacionRazonSocial <- renderUI({
      tryCatch({
        .ValidarRazonSocial(razon_social_ingresada = input$CampoRazonSocial %||% "", codigo_contacto_actual = NULL)
      }, error = function(error) {
        FormatearTexto(paste0("* Error de validación: ", conditionMessage(error)),
                       negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.75)
      })
    })
    output$ValidacionNit <- renderUI({
      tryCatch({
        .ValidarNit(nit_ingresado = input$CampoNit %||% "", codigo_contacto_actual = NULL)
      }, error = function(error) {
        FormatearTexto(paste0("* Error de validación: ", conditionMessage(error)),
                       negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.75)
      })
    })
    
    # Observadores -----
    observeEvent(input$BotonGuardar, {
      condiciones_validacion <- .ValidarCamposFormulario()
      if (any(condiciones_validacion)) {
        for (nombre_condicion in names(condiciones_validacion)) {
          if (condiciones_validacion[[nombre_condicion]]) {
            showNotification(nombre_condicion, duration = 4, type = "error")
          }
        }
        return(invisible(NULL))
      }
      
      coincidencia_exacta <- .BuscarContactoPorRazonSocial(input$CampoRazonSocial, codigo_contacto_actual = NULL)
      
      texto_confirmacion <- if (!is.null(coincidencia_exacta)) {
        paste0("Ya existe un contacto con esta razón social (NIT: ", 
               coincidencia_exacta$nit %||% "sin NIT",
               "). ¿Desea continuar y guardar de todas formas?")
      } else {
        "¿Desea crear este contacto?"
      }
      
      racafeShiny::MostrarModalConfirmacion(ns = ns, titulo = "Confirmar guardado", texto = texto_confirmacion,
                                            id_cancelar = "BotonCancelarGuardado", id_confirmar = "BotonConfirmarGuardado",
                                            label_confirmar = "Crear Contacto", icono_confirmar = "floppy-disk")
    })
    observeEvent(input$BotonCancelarGuardado, { 
      removeModal() 
    })
    observeEvent(input$BotonConfirmarGuardado, {
      codigo <- generar_codigo_contacto()
      
      fila <- data.frame(CodContacto   = codigo,
                         UsuarioCrea   = usuario(), FechaHoraCrea = Sys.time(),
                         UsuarioMod    = usuario(), FechaHoraModi = Sys.time(),
                         AutorizaTD    = input$CampoAutorizaDatos,
                         PerRazSoc     = .NormalizarRazonSocial(input$CampoRazonSocial),
                         PerCod        = input$CampoNit,
                         Origen        = input$CampoOrigen, DetOrigen = input$CampoDetalleOrigen,
                         Estado        = "ACTIVO",
                         stringsAsFactors = FALSE) %>% 
        mutate(across(where(is.character) & !c("PerRazSoc"),
                      ~ str_to_upper(ifelse(. == "", NA_character_, .)))
        )
      
      tryCatch({
        AgregarDatos(fila, "CRMNALCONTACTO")
        removeModal()
        showNotification(paste("Contacto creado:", codigo), duration = 5, type = "message")
        .LimpiarFormulario()
        codigo_creado(codigo)
        contador_creaciones(contador_creaciones() + 1)
      }, error = function(error) {
        removeModal()
        .ManejarErrorAccion(error = error, operacion = "guardar el contacto", usuario = usuario())
      })
    })
    
    # Retorno ----
    list(n = reactive(contador_creaciones()), codigo = reactive(codigo_creado()))
  })
}

# App de Prueba -----
ui <- bs4DashPage(
  title = "Prueba Accion Crear",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    CrearContactoUI("AccionCrear")
  )
)

server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  CrearContacto("AccionCrear", usuario = usuario_sesion)
}
shinyApp(ui, server)