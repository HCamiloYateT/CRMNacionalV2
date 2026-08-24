# Descartar Oportunidad.
# Modulo Principal ----
DescartarOportunidadUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ResumenOportunidad")),
    box(title = "Descartar Oportunidad", width = 12, collapsible = FALSE,
        ListaDesplegable(ns("DES_Razon"), label = Obligatorio("Razón de Descarte"),
                         choices = .RAZONES_DESCARTE_OPORTUNIDAD, selected = NULL, multiple = FALSE),
        textAreaInput(ns("DES_Comentario"), label = h6("Comentario adicional"), value = "",
                      placeholder = "Detalle opcional del descarte", width = "100%", height = "70px"),
        racafeShiny::Boton(id = ns("DES_Solicitar"), label = "Descartar Oportunidad",
                           icono = "ban", align = "right", size = "xs",
                           color_fondo = "#C11007", color_fuente = "#FFFFFF")
    )
  )
}
DescartarOportunidad <- function(id, usuario, id_oportunidad) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    ret <- reactiveVal(0)
    
    oportunidad_actual <- reactive({
      req(id_oportunidad())
      cond <- sprintf("IdOportunidad = '%s'", .rc_escapar_sql(id_oportunidad()))
      CargarDatos("CRMNALCLOPT", condicion = cond)
    })
    
    output$ResumenOportunidad <- renderUI({
      req(nrow(oportunidad_actual()) > 0)
      op <- oportunidad_actual()
      .rc_card(
        borde_color = "#94A3B8", header = paste0(op$Categoria[[1]], " — ", op$Producto[[1]]),
        cuerpo = paste0("Sacos: ", op$Sacos[[1]], " | Margen: ", op$Margen[[1]],
                        " | Cumplimiento: ", format(op$FechaCumpOP[[1]], "%d/%m/%Y"))
      )
    })
    
    motivo_final <- reactive({
      comentario <- trimws(input$DES_Comentario %||% "")
      if (EsVacio(comentario)) input$DES_Razon else paste0(input$DES_Razon, " | ", comentario)
    })
    
    observeEvent(input$DES_Solicitar, {
      cond <- c("La razón de descarte es obligatoria" = EsVacio(input$DES_Razon))
      if (any(cond)) {
        sapply(names(cond[cond]), function(msg) showNotification(msg, duration = 4, type = "error"))
        return(invisible(NULL))
      }
      racafeShiny::MostrarModalConfirmacion(
        ns = ns, titulo = "Confirmar descarte de oportunidad",
        texto = paste0("¿Deseas descartar esta oportunidad? Motivo: ", motivo_final()),
        id_cancelar = "DES_Cancelar", id_confirmar = "DES_Confirmar",
        label_confirmar = "Descartar", icono_confirmar = "ban"
      )
    })
    
    observeEvent(input$DES_Cancelar, { removeModal() })
    
    observeEvent(input$DES_Confirmar, {
      tryCatch({
        descartar_oportunidad(id_oportunidad(), motivo_final(), usuario())
        removeModal()
        showNotification("Oportunidad descartada exitosamente", duration = 4, type = "message")
        updateTextAreaInput(session, "DES_Comentario", value = "")
        ret(ret() + 1)
      }, error = function(e) {
        removeModal()
        showNotification(paste0("Error al descartar la oportunidad: ", conditionMessage(e)),
                         duration = 6, type = "error")
      })
    })
    
    list(n = reactive(ret()))
  })
}

# App de prueba ----
ui <- bs4DashPage(
  title = "Prueba Accion Editar",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    useShinyjs(),
    includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
    box(
      title = "Editar Contacto (prueba)",
      width = 12,
      textInput("id_oportunidad", label = "Codigo de Contacto a editar", value = ""),
      DescartarOportunidadUI("AccionEditar")
    )
  )
)

server <- function(input, output, session) {
  usuario_sesion <- reactive("CMEDINA")
  id_oportunidad <- reactive(input$id_oportunidad)
  DescartarOportunidad("AccionEditar", usuario = usuario_sesion, id_oportunidad = id_oportunidad)
}

shinyApp(ui, server)
