function(input, output, session) {

  # Datos reactivos ----
  usuario <- reactive({
    if (is.null(session$user)) "HCYATE" else str_to_upper(session$user)
  })
  grupo <- reactive({
    if (is.null(session$group)) "ANALÍTICA" else stringr::str_to_upper(session$group)
  })
  # Outputs ----
  ## Header ----  
  output$user <- renderUI({
    FormatearTexto(paste(usuario()) %>% HTML, negrita = TRUE, tamano_pct = 0.75, alineacion = "center", color = "#999")
  })
  ## Modulos -----
  codigo_contacto_prueba <- reactive(input$CodigoContactoPrueba)
  Editar("Editar", usuario = usuario, codigo_contacto = codigo_contacto_prueba)
  Relacionamiento("Relacionamiento", usuario = usuario, codigo_contacto = codigo_contacto_prueba)
  Promover("Promover", usuario = usuario, codigo_contacto = codigo_contacto_prueba)
  Descartar("Descartar", usuario = usuario, codigo_contacto = codigo_contacto_prueba)
  Reactivar("Reactivar", usuario = usuario, codigo_contacto = codigo_contacto_prueba)
  CrearOportunidad("CrearOportunidad", usuario = usuario, codigo_contacto = codigo_contacto_prueba)
  
  ## Embudo ----
  EmbudoConversion("EmbudoConversion")
  KanbanEmbudo("Kanban", usuario)
  DetalleContacto("DetalleContacto", usuario)
  DetalleProspectos("DetalleProspectos", usuario)
  DetalleLeads("DetalleLeads", usuario)
  DetalleClientes("DetalleClientes", usuario)
  DetalleDescartados("DetalleDescartados")
  
  }