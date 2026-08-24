body <- bs4DashBody(
  useShinyjs(),
  includeCSS(paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/", "refs/heads/main/Styles/style.css")),
  use_waiter(),
  shinyjs::useShinyjs(),
  bs4TabItems(
    bs4TabItem(tabName = "ACC_Detalle", h4("Ver Detalle")),
    bs4TabItem(tabName = "ACC_Editar", EditarUI("Editar")),
    bs4TabItem(tabName = "ACC_Relacionamiento", RelacionamientoUI("Relacionamiento")),
    bs4TabItem(tabName = "ACC_Promover", PromoverUI("Promover")),
    bs4TabItem(tabName = "ACC_Oportunidad", CrearOportunidadUI("CrearOportunidad")),
    bs4TabItem(tabName = "ACC_Descartar", DescartarUI("Descartar")),
    bs4TabItem(tabName = "ACC_Reactivar", ReactivarUI("Reactivar")),
    # Generalidades ----
    bs4TabItem(tabName = "GE_Resumen", h4("Resumen Ejecutivo")),
    bs4TabItem(tabName = "GE_Evolucion", h4("Evolución del Negocio")),
    bs4TabItem(tabName = "GE_Participacion", h4("Participación y Composición")),
    bs4TabItem(tabName = "GE_Indicadores", h4("Indicadores de Mercado")),
    bs4TabItem(tabName = "GE_Comparacion", h4("Comparación de Indicadores")),
    # Gestión Comercial ----
    bs4TabItem(tabName = "GC_Oportunidades", h4("Oportunidades")),
    bs4TabItem(tabName = "GC_Pendientes", h4("Pendientes de Gestión")),
    bs4TabItem(tabName = "GC_Tareas", h4("Notas y Tareas")),
    bs4TabItem(tabName = "GC_Alertas", h4("Alertas y Notificaciones")),
    # Clientes ----
    bs4TabItem(tabName = "CL_Resumen", h4("Resumen")),
    bs4TabItem(tabName = "CL_EjecucionPpto", h4("Ejecución Presupuesto")),
    bs4TabItem(tabName = "CL_Potencial", h4("Potencial")),
    bs4TabItem(tabName = "CL_Comportamiento", h4("Comportamiento")),
    bs4TabItem(tabName = "CL_Fidelidad", h4("Fidelidad y Riesgo")),
    bs4TabItem(tabName = "CL_Historicos", h4("Históricos")),
    bs4TabItem(tabName = "CL_Cartera", h4("Cartera")),
    bs4TabItem(tabName = "CL_RFM", h4("Segmentación")),
    # Clientes a Recuperar ----
    bs4TabItem(tabName = "CR_Resumen", h4("Resumen")),
    bs4TabItem(tabName = "CR_Razones", h4("Razones de Pérdida")),
    bs4TabItem(tabName = "CR_Oportunidades", h4("Oportunidades")),
    bs4TabItem(tabName = "CR_Seguimiento", h4("Seguimiento")),
    bs4TabItem(tabName = "CR_RFM", h4("Segmentación")),
    # Prospección ----
    bs4TabItem(tabName = "PR_Kanban", KanbanEmbudoUI("Kanban")),
    bs4TabItem(tabName = "PR_Contactos", DetalleContactoUI("DetalleContacto")),
    bs4TabItem(tabName = "PR_Prospectos", DetalleProspectosUI("DetalleProspectos")),
    bs4TabItem(tabName = "PR_Leads", DetalleLeadsUI("DetalleLeads")),
    bs4TabItem(tabName = "PR_Descartados", DetalleDescartadosUI("DetalleDescartados")),
    bs4TabItem(tabName = "PR_Conversion", EmbudoConversionUI("EmbudoConversion")),
    bs4TabItem(tabName = "PR_Potencial", h4("Potencial de Mercado")),
    # Dinámica de Clientes ----
    bs4TabItem(tabName = "DC_Nuevos", h4("Clientes Nuevos")),
    bs4TabItem(tabName = "DC_Perdidos", h4("Clientes Perdidos")),
    # Cohortes de Clientes ----
    bs4TabItem(tabName = "CO_Retencion", h4("Retención")),
    bs4TabItem(tabName = "CO_Ventas", h4("Ventas")),
    bs4TabItem(tabName = "CO_Margen", h4("Margen")),
    bs4TabItem(tabName = "CO_Frecuencia", h4("Frecuencia")),
    # Gestión por Asesor ----
    bs4TabItem(tabName = "GA_Desempeno", h4("Desempeño")),
    bs4TabItem(tabName = "GA_EjecucionPpto", h4("Ejecución Presupuesto")),
    bs4TabItem(tabName = "GA_Bonificaciones", h4("Bonificaciones")),
    bs4TabItem(tabName = "GA_Mejora", h4("Oportunidades de Mejora")),
    # Información Cafetera ----
    bs4TabItem(tabName = "IC_Indicadores", h4("Mercado e Indicadores")),
    bs4TabItem(tabName = "IC_Inventarios", h4("Inventarios")),
    # Herramientas ----
    bs4TabItem(tabName = "HE_Calculadoras", h4("Calculadoras")),
    bs4TabItem(tabName = "HE_Conversion", h4("Tablas de Conversión")),
    bs4TabItem(tabName = "HE_Pendientes", h4("Pendientes de Lotes")),
    bs4TabItem(tabName = "HE_Informes", h4("Informes")),
    # Consulta Individual ----
    bs4TabItem(tabName = "IN_Consulta", h4("Consulta Individual"))
  )
)