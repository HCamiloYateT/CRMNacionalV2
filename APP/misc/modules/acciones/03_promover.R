# Promover.
# Modulos Auxiliares ----

## FormularioAscenderProspecto ----
FormularioAscenderProspectoUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Ascender a Prospecto", width = 12, collapsible = FALSE,
        FormatearTexto("Vincula este contacto como alianza de uno o varios clientes existentes.",
                       tamano_pct = 0.8, color = "#64748B"),
        selectizeInput(ns("PRO_Alianzas"), label = Obligatorio("Cliente(s) Aliado(s)"),
                       choices = .choices_cliente_ppal(), selected = NULL, multiple = TRUE,
                       options = list(placeholder = "Seleccione uno o varios clientes")),
        textAreaInput(ns("PRO_Observacion"), label = h6("Observación"), value = "",
                      width = "100%", height = "60px"),
        racafeShiny::Boton(id = ns("PRO_Solicitar"), label = "Ascender", icono = "arrow-up",
                           align = "right", size = "xs", color_fondo = "#C11007", color_fuente = "#FFFFFF")
    )
  )
}
FormularioAscenderProspecto <- function(id, usuario, cod_contacto, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    observeEvent(input$PRO_Solicitar, {
      if (length(input$PRO_Alianzas) == 0) {
        showNotification("Debe seleccionar al menos un cliente aliado", type = "error", duration = 4)
        return(invisible(NULL))
      }
      # Modal centralizado de racafeShiny, igual que CrearContacto/Identificacion/
      # Relacionamiento, en vez de MostrarModalConClase + modalDialog manual
      racafeShiny::MostrarModalConfirmacion(
        ns = ns, titulo = "Confirmar ascenso a Prospecto",
        texto = paste0("¿Deseas ascender este contacto a Prospecto con ", length(input$PRO_Alianzas),
                       " alianza(s) registrada(s)?"),
        id_cancelar = "PRO_Cancelar", id_confirmar = "PRO_Confirmar",
        label_confirmar = "Ascender a Prospecto", icono_confirmar = "arrow-up"
      )
    })
    observeEvent(input$PRO_Cancelar, { removeModal() })
    observeEvent(input$PRO_Confirmar, {
      tryCatch({
        convertir_contacto_a_prospecto(cod_contacto(), input$PRO_Alianzas, usuario(), input$PRO_Observacion)
        removeModal()
        showNotification("Contacto ascendido a Prospecto exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al ascender el contacto:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    list(n = reactive(ret()))
  })
}

## FormularioAscenderLead ----
FormularioAscenderLeadUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Ascender a Lead", width = 12, collapsible = FALSE,
        ListaDesplegable(ns("ASC_LinNeg"), label = Obligatorio("Línea de Negocio"),
                         choices = Choices()$linneg, selected = NULL, multiple = FALSE),
        ListaDesplegable(ns("ASC_Segmento"), label = Obligatorio("Segmento"),
                         choices = Choices()$segmento, selected = NULL, multiple = FALSE),
        ListaDesplegable(ns("ASC_Asesor"), label = Obligatorio("Asesor"),
                         choices = Choices()$personas, selected = NULL, multiple = FALSE),
        racafeShiny::Boton(id = ns("ASC_Solicitar"), label = "Ascender", icono = "arrow-up",
                           align = "right", size = "xs", color_fondo = "#C11007", color_fuente = "#FFFFFF")
    )
  )
}
FormularioAscenderLead <- function(id, usuario, cod_contacto, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    .data_contacto <- if (!is.null(data_contacto)) {
      data_contacto
    } else {
      reactive({
        req(cod_contacto())
        cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto()))
        CargarDatos("CRMNALCONTACTO", condicion = cond)
      })
    }
    
    validar_campos_ascenso <- function() {
      c(
        "El campo Asesor es obligatorio" = EsVacio(input$ASC_Asesor),
        "El campo Segmento es obligatorio" = EsVacio(input$ASC_Segmento),
        "El campo Línea de Negocio es obligatorio" = EsVacio(input$ASC_LinNeg)
      )
    }
    observeEvent(input$ASC_Solicitar, {
      cond <- validar_campos_ascenso()
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      racafeShiny::MostrarModalConfirmacion(
        ns = ns, titulo = "Confirmar ascenso a Lead",
        texto = paste0("¿Deseas ascender este contacto a Lead con asesor ", input$ASC_Asesor,
                       ", segmento ", input$ASC_Segmento, " y línea de negocio ", input$ASC_LinNeg, "?"),
        id_cancelar = "ASC_Cancelar", id_confirmar = "ASC_Confirmar",
        label_confirmar = "Ascender a Lead", icono_confirmar = "arrow-up"
      )
    })
    observeEvent(input$ASC_Cancelar, { removeModal() })
    observeEvent(input$ASC_Confirmar, {
      tryCatch({
        convertir_contacto_a_lead(cod_contacto(), input$ASC_Asesor, input$ASC_Segmento, input$ASC_LinNeg, usuario())
        removeModal()
        showNotification("Contacto ascendido a Lead exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al ascender el contacto:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

## FormularioReclasificarLead ----
FormularioReclasificarLeadUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Reclasificar a Lead", width = 12, collapsible = FALSE,
        FormatearTexto("El volumen de este Prospecto ahora sí interesa para venta directa. Las alianzas ya registradas quedan como historial.",
                       tamano_pct = 0.8, color = "#64748B"),
        ListaDesplegable(ns("RLE_Asesor"), label = Obligatorio("Asesor"),
                         choices = Choices()$personas, selected = NULL, multiple = FALSE),
        ListaDesplegable(ns("RLE_Segmento"), label = Obligatorio("Segmento"),
                         choices = Choices()$segmento, selected = NULL, multiple = FALSE),
        ListaDesplegable(ns("RLE_LinNeg"), label = Obligatorio("Línea de Negocio"),
                         choices = Choices()$linneg, selected = NULL, multiple = FALSE),
        racafeShiny::Boton(id = ns("RLE_Solicitar"), label = "Reclasificar a Lead", icono = "arrow-up",
                           align = "right", size = "xs", color_fondo = "#C11007", color_fuente = "#FFFFFF")
    )
  )
}
FormularioReclasificarLead <- function(id, usuario, cod_contacto, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    .data_contacto <- if (!is.null(data_contacto)) {
      data_contacto
    } else {
      reactive({
        req(cod_contacto())
        cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto()))
        CargarDatos("CRMNALCONTACTO", condicion = cond)
      })
    }
    validar_campos_reclasificacion <- function() {
      c(
        "El campo Asesor es obligatorio" = EsVacio(input$RLE_Asesor),
        "El campo Segmento es obligatorio" = EsVacio(input$RLE_Segmento),
        "El campo Línea de Negocio es obligatorio" = EsVacio(input$RLE_LinNeg)
      )
    }
    observeEvent(input$RLE_Solicitar, {
      cond <- validar_campos_reclasificacion()
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      racafeShiny::MostrarModalConfirmacion(
        ns = ns, titulo = "Confirmar reclasificación a Lead",
        texto = paste0("¿Deseas reclasificar este Prospecto a Lead con asesor ", input$RLE_Asesor,
                       ", segmento ", input$RLE_Segmento, " y línea de negocio ", input$RLE_LinNeg, "?"),
        id_cancelar = "RLE_Cancelar", id_confirmar = "RLE_Confirmar",
        label_confirmar = "Reclasificar a Lead", icono_confirmar = "arrow-up"
      )
    })
    observeEvent(input$RLE_Cancelar, { removeModal() })
    observeEvent(input$RLE_Confirmar, {
      tryCatch({
        convertir_prospecto_a_lead(cod_contacto(), input$RLE_Asesor, input$RLE_Segmento, input$RLE_LinNeg, usuario())
        removeModal()
        showNotification("Prospecto reclasificado a Lead exitosamente", duration = 4, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al reclasificar:", conditionMessage(e)), duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

## FormularioVincularNit ----
FormularioVincularNitUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Vincular NIT de Facturación", width = 12, collapsible = FALSE,
        FormatearTexto("Úsalo cuando el lead entró con un NIT distinto al que finalmente factura.",
                       tamano_pct = 0.8, color = "#64748B"),
        textInput(ns("VIN_Nit"), label = Obligatorio("NIT que Factura"), width = "100%"),
        textAreaInput(ns("VIN_Observacion"), label = h6("Observación"), value = "", width = "100%", height = "60px"),
        racafeShiny::Boton(id = ns("VIN_Guardar"), label = "Vincular", icono = "link",
                           align = "right", size = "xs", color_fondo = "#C11007", color_fuente = "#FFFFFF")
    )
  )
}
FormularioVincularNit <- function(id, usuario, cod_contacto, data_contacto = NULL) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    .data_contacto <- if (!is.null(data_contacto)) {
      data_contacto
    } else {
      reactive({
        req(cod_contacto())
        cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(cod_contacto()))
        CargarDatos("CRMNALCONTACTO", condicion = cond)
      })
    }
    
    observeEvent(cod_contacto(), {
      req(cod_contacto())
      v <- cargar_vinculo_nit(cod_contacto())
      if (nrow(v) > 0) updateTextInput(session, "VIN_Nit", value = v$NitVinculado[[1]])
    })
    
    observeEvent(input$VIN_Guardar, {
      if (EsVacio(input$VIN_Nit) || !EsEnteroPositivo(input$VIN_Nit)) {
        showNotification("Ingrese un NIT válido", type = "error", duration = 4)
        return(invisible(NULL))
      }
      # Se homologa a modal de confirmacion, igual que los otros 3 formularios;
      # antes escribia directo sin confirmacion previa
      racafeShiny::MostrarModalConfirmacion(
        ns = ns, titulo = "Confirmar vinculación de NIT",
        texto = paste0("¿Deseas vincular el NIT ", input$VIN_Nit, " como NIT de facturación de este lead?"),
        id_cancelar = "VIN_Cancelar", id_confirmar = "VIN_Confirmar",
        label_confirmar = "Vincular", icono_confirmar = "link"
      )
    })
    
    observeEvent(input$VIN_Cancelar, { removeModal() })
    
    observeEvent(input$VIN_Confirmar, {
      tryCatch({
        registrar_vinculo_nit(cod_contacto(), input$VIN_Nit, usuario(), input$VIN_Observacion)
        removeModal()
        showNotification("NIT vinculado exitosamente", duration = 3, type = "message")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error al vincular:", conditionMessage(e)), duration = 5, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

# Modulo Principal ----
PromoverUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("Titulo")),
    uiOutput(ns("EstadoActual")),
    uiOutput(ns("SelectorDestino")),
    uiOutput(ns("Contenido"))
  )
}
Promover <- function(id, usuario, codigo_contacto) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    trigger_refresco <- reactiveVal(0)
    # Destino elegido cuando la etapa vigente es CONTACTO (unica etapa con
    # mas de un formulario posible); se resetea cada vez que cambia el
    # contacto para no arrastrar la eleccion de un contacto al siguiente
    destino_contacto <- reactiveVal(NULL)
    
    # Fuente unica de datos del contacto para todo el modulo, incluidos los
    # 3 formularios hijos que antes la reconsultaban por su cuenta (punto 1)
    data_contacto <- reactive({
      req(codigo_contacto())
      cond <- sprintf("CodContacto = '%s'", .rc_escapar_sql(codigo_contacto()))
      CargarDatos("CRMNALCONTACTO", condicion = cond)
    })
    
    etapa_contacto <- reactive({
      trigger_refresco()
      req(codigo_contacto())
      obtener_etapa_contacto(codigo_contacto())
    })
    
    observeEvent(codigo_contacto(), { destino_contacto(NULL) })
    
    output$Titulo <- renderUI({
      req(nrow(data_contacto()) > 0)
      h4(.titulo_identificacion(data_contacto()$PerCod, data_contacto()$PerRazSoc))
    })
    
    # Bloque de contexto: NIT, Razon Social, Etapa vigente y mensaje de
    # destino de la promocion — previo al selector/formulario
    output$EstadoActual <- renderUI({
      etapa <- etapa_contacto()
      req(!is.na(etapa))
      
      tags$div(
        style = "margin-bottom:14px;",
        tags$span(style = "font-size:13px; color:#374151; margin-right:6px;", "Etapa vigente:"),
        .badge_etapa(etapa),
        if (identical(etapa, "CONTACTO")) {
          tags$div(style = "margin-top:6px;",
                   FormatearTexto("Este contacto tiene dos caminos posibles: promoverlo a Lead o a Prospecto.",
                                  tamano_pct = 0.8, color = "#1C398E", negrita = TRUE))
        }
      )
    })
    
    # Selector de destino, solo visible cuando la etapa vigente es CONTACTO
    # (unica etapa con dos rutas posibles: Lead o Prospecto)
    output$SelectorDestino <- renderUI({
      etapa <- etapa_contacto()
      req(!is.na(etapa), etapa == "CONTACTO")
      
      ListaDesplegable(
        ns("CampoDestino"), label = Obligatorio("Promover a"),
        choices = c("Seleccione una opción" = "", "Lead" = "LEAD", "Prospecto" = "PROSPECTO"),
        selected = destino_contacto() %||% "", multiple = FALSE
      )
    })
    
    observeEvent(input$CampoDestino, {
      destino_contacto(if (identical(input$CampoDestino, "")) NULL else input$CampoDestino)
    }, ignoreNULL = FALSE)
    
    # Sub-modulos instanciados una sola vez; el renderUI solo decide cual
    # UI mostrar, no recrea servers en cada cambio de etapa/destino.
    # data_contacto se comparte con los 3 formularios que muestran titulo,
    # eliminando sus consultas independientes a CRMNALCONTACTO
    modulo_ascender_lead      <- FormularioAscenderLead("Ascender", usuario, codigo_contacto, data_contacto)
    modulo_ascender_prospecto <- FormularioAscenderProspecto("AscenderProspecto", usuario, codigo_contacto)
    modulo_reclasificar       <- FormularioReclasificarLead("Reclasificar", usuario, codigo_contacto, data_contacto)
    modulo_vincular           <- FormularioVincularNit("Vincular", usuario, codigo_contacto, data_contacto)
    
    observeEvent(modulo_ascender_lead$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_ascender_prospecto$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_reclasificar$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    observeEvent(modulo_vincular$n(), { trigger_refresco(trigger_refresco() + 1) }, ignoreInit = TRUE)
    
    output$Contenido <- renderUI({
      etapa <- etapa_contacto()
      req(!is.na(etapa))
      
      if (etapa == "CONTACTO") {
        # Sin eleccion aun en "Promover a" -> no se pinta ningun formulario
        if (is.null(destino_contacto())) return(NULL)
        return(if (identical(destino_contacto(), "PROSPECTO")) {
          FormularioAscenderProspectoUI(ns("AscenderProspecto"))
        } else {
          FormularioAscenderLeadUI(ns("Ascender"))
        })
      }
      
      switch(etapa,
             "PROSPECTO"  = FormularioReclasificarLeadUI(ns("Reclasificar")),
             "LEAD"       = FormularioVincularNitUI(ns("Vincular")),
             "CLIENTE"    = FormatearTexto("Este contacto ya es Cliente.", negrita = TRUE,
                                           color = "#198754", tamano_pct = 0.9),
             "DESCARTADO" = FormatearTexto("Este contacto está descartado; reactívelo antes de promoverlo.",
                                           negrita = TRUE, color = COLOR_ERROR, tamano_pct = 0.9),
             FormatearTexto("Etapa no reconocida para este contacto.", negrita = TRUE,
                            color = COLOR_ERROR, tamano_pct = 0.9)
      )
    })
    
    list(etapa = etapa_contacto)
  })
}

# App de prueba ----
ui <- bs4DashPage(
  title = "Prueba Accion Promover",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    box(
      title = "Promover Contacto (prueba)",
      width = 12,
      textInput("CodigoContactoPrueba", label = "Codigo de Contacto a promover", value = ""),
      PromoverUI("AccionPromover")
    )
  )
)

server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  codigo_contacto_prueba <- reactive(input$CodigoContactoPrueba)
  Promover("AccionPromover", usuario = usuario_sesion, codigo_contacto = codigo_contacto_prueba)
}

shinyApp(ui, server)