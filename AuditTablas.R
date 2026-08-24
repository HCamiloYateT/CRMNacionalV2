# Auditoria ----
## Motor Generico ----
# Sin cambios respecto al original: son funciones agnosticas al esquema y
# candidatas a vivir en racafeModulos (ver nota al final del script).

# Inventaria un conjunto de tablas Racafe: filas, columnas y error de carga
# si aplica. Generico - no depende de ningun submodulo en particular
InventariarTablasRacafe <- function(nombres_tablas) {
  resultados <- lapply(nombres_tablas, function(nombre_tabla) {
    resultado_carga <- tryCatch({
      datos <- CargarDatos(nombre_tabla)
      list(registros = nrow(datos), columnas = ncol(datos), error = NA_character_)
    }, error = function(e) {
      list(registros = NA_integer_, columnas = NA_integer_, error = conditionMessage(e))
    })
    data.frame(Tabla = nombre_tabla, Registros = resultado_carga$registros,
               Columnas = resultado_carga$columnas, Error = resultado_carga$error,
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(resultados) %>% dplyr::arrange(dplyr::desc(Registros))
}

# Crea/reemplaza un conjunto de tablas vacias en Racafe - name = tabla,
# value = data.frame vacio con las columnas ya tipificadas
EscribirTablasVacias <- function(esquemas) {
  for (nombre_tabla in names(esquemas)) {
    EscribirDatos(esquemas[[nombre_tabla]], nombre_tabla)
  }
  invisible(TRUE)
}

# Puebla tablas ya existentes con datos simulados - name = tabla,
# value = data.frame con los registros a insertar
PoblarTablasSimuladas <- function(datos_simulados) {
  for (nombre_tabla in names(datos_simulados)) {
    racafeBD::AgregarDatos(datos_simulados[[nombre_tabla]], nombre_tabla)
  }
  invisible(datos_simulados)
}

## Mapa Submodulo -> Tablas ----
# Documenta, en un solo lugar, que submodulo (etapa/accion del flujo) es
# dueño de cada tabla. Sirve como fuente de verdad para el inventario
# completo y para detectar tablas huerfanas (sin submodulo mapeado)
.MAPA_SUBMODULO_TABLAS <- list(
  "Identificacion"   = c("CRMNALCONTACTO"),
  "Directorio"       = c("CRMNALDIRECTORIOEMPRESARIAL", "CRMNALDIRECTORIOTELEFONO",
                         "CRMNALDIRECTORIORED", "CRMNALDIRECTORIOPERSONA",
                         "CRMNALDIRECTORIOPERSONARED"),
  "Geografia"        = c("CRMNALUBICACION", "CRMNALSUCURSALES"),
  "Potencial"        = c("CRMNALPOTENCIAL"),
  "Relacionamiento"  = c("CRMNALRELACIONAMIENTO", "CRMNALRECORDATORIO"),
  "Promover"         = c("CRMNALPROSPECTOALIANZA", "CRMNALVINCULONIT",
                         "CRMNALHISTORIALETAPA", "CONTACTOLEAD"),
  "Oportunidades"    = c("CRMNALCLOPT"),
  "ConversionCliente" = c("CRMNALLEADCLIENTE"),
  "Descartar"        = c("CRMNALDESCARTE"),
  "CatalogosGlobales" = c("CRMNALPRODS")
)
# CRMNALPRODS es una tabla catalogo GLOBAL (linea de negocio/categoria/
# producto), administrada fuera del CRM de Contactos - Oportunidades solo
# la CONSULTA (CargarDatos) para la cascada LinNeg->Categoria->Producto,
# nunca la crea ni la escribe. Por eso no tiene esquema/simulacion propios
# en este script: crearla vacia aqui borraria el catalogo real
# CRMNALESTCUENTA, CRMNALRAZINTERES y CRMNALALIADOS se retiraron del
# inventario a pedido explicito: no se usan, y CRMNALALIADOS ya fue
# eliminada de la base de datos (confirmado por el usuario). Los "aliados"
# no son una tabla propia: son clientes de `data`, identificados por la
# combinacion (CliNitPpal, PerRazSoc) - mismo patron que ya usa
# .choices_cliente_ppal() en Generales.R.
# PENDIENTE (fuera de este script): Choices()$aliado, referenciado en
# .DETALLE_RAZON_DESCARTE para la razon "CLIENTE ALIADO NO ACEPTÓ LA
# RELACIÓN" (Descartar), probablemente sigue apuntando a
# CargarDatos("CRMNALALIADOS") dentro de la definicion de Choices() -
# no compartida en este hilo. Debe reescribirse para leer de
# `data %>% distinct(CliNitPpal, PerRazSoc)` en vez de la tabla eliminada,
# o esa razon de descarte seguira fallando con el mismo error
#
# NOTA sobre categorias: "CatalogosGlobales" agrupa tablas que no
# pertenecen a ningun submodulo del flujo de Contacto (no las crea/escribe
# ningun formulario) sino que son catalogos compartidos por todo el CRM.
# Reemplaza al antiguo cajon "SinSubmodulo", que era ambiguo entre "tabla
# huerfana sin dueño" y "tabla global por diseño"
# Descartar y Reactivar NO tienen tablas propias (confirmado con el cuerpo
# real de descartar_generico() y reactivar_contacto()): ambas funciones son
# la misma familia que registrar_transicion_etapa()/.insertar_conversion_lead()
# (Promover) - solo mueven Estado/EtapaPreDescarte en CRMNALCONTACTO
# (Identificacion) y dejan un renglon en CRMNALHISTORIALETAPA (Promover,
# ya cubierta en .esquema_tablas_vacias_promover()). Ninguna tabla nueva
# que crear ni simular para estos dos verbos.
# CRMNALDESCARTE queda en SinSubmodulo: no la referencia ningun codigo
# compartido en este hilo; el motivo de descarte/reactivacion vive como
# texto libre en CRMNALHISTORIALETAPA.Motivo, no en una tabla aparte

# Genera la tabla de mapeo Submodulo/Tabla usada solo para documentacion
.tabla_mapa_submodulos <- function() {
  dplyr::bind_rows(lapply(names(.MAPA_SUBMODULO_TABLAS), function(sm) {
    data.frame(Submodulo = sm, Tabla = .MAPA_SUBMODULO_TABLAS[[sm]], stringsAsFactors = FALSE)
  }))
}

# Genericos de simulacion de fechas/ids, reusados por todos los submodulos
# para no repetir sprintf/paste0 de prefijos en cada bloque
.id_prefijo <- function(prefijo, n) sprintf(paste0(prefijo, "-%05d"), seq_len(n))

## Identificacion ----
# CRMNALCONTACTO es la tabla nucleo, poblada por CrearContacto/Identificacion;
# no se crea vacia ni se simula aqui para no pisar datos reales de produccion.
# Solo se inventaria.

## Directorio ----
.esquema_tablas_vacias_directorio <- function() {
  list(
    CRMNALDIRECTORIOEMPRESARIAL = data.frame(
      CodContacto = character(), CorreoGeneral = character(),
      UsuarioMod = character(), FechaHoraModi = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ),
    CRMNALDIRECTORIOTELEFONO = data.frame(
      IdTelefono = character(), CodContacto = character(), Telefono = character(),
      UsuarioCrea = character(), FechaHoraCrea = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ),
    CRMNALDIRECTORIORED = data.frame(
      IdRed = character(), CodContacto = character(),
      TipoRedSocial = character(), UsuarioRed = character(),
      stringsAsFactors = FALSE
    ),
    CRMNALDIRECTORIOPERSONA = data.frame(
      IdPersona = character(), CodContacto = character(), Nombre = character(),
      Cargo = character(), Telefono = character(), Correo = character(),
      UsuarioCrea = character(), FechaHoraCrea = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ),
    CRMNALDIRECTORIOPERSONARED = data.frame(
      IdRed = character(), IdPersona = character(),
      TipoRedSocial = character(), UsuarioRed = character(),
      stringsAsFactors = FALSE
    )
  )
}
.simular_datos_directorio <- function(n_min = 50, n_max = 60, usr_simulado = "SIMULACION") {
  contactos_existentes <- CargarDatos("CRMNALCONTACTO") %>% pull(CodContacto)
  ahora <- Sys.time()
  .n <- function() sample(n_min:n_max, 1)
  
  n_empresarial <- .n()
  directorio_empresarial <- data.frame(
    CodContacto = sample(contactos_existentes, n_empresarial),
    CorreoGeneral = paste0("contacto", seq_len(n_empresarial), "@simulado.com"),
    UsuarioMod = usr_simulado, FechaHoraModi = ahora, stringsAsFactors = FALSE
  )
  
  n_telefono <- .n()
  directorio_telefono <- data.frame(
    IdTelefono = .id_prefijo("TL", n_telefono),
    CodContacto = sample(contactos_existentes, n_telefono, replace = TRUE),
    Telefono = paste0("3", sample(100000000:999999999, n_telefono)),
    UsuarioCrea = usr_simulado, FechaHoraCrea = ahora, stringsAsFactors = FALSE
  ) %>%
    group_by(CodContacto) %>% slice_head(n = .MAX_TELEFONOS_GENERAL) %>%
    ungroup() %>% as.data.frame(stringsAsFactors = FALSE)
  
  n_red <- .n()
  directorio_red <- data.frame(
    IdRed = .id_prefijo("RD", n_red),
    CodContacto = sample(contactos_existentes, n_red, replace = TRUE),
    TipoRedSocial = sample(.TIPOS_RED_SOCIAL, n_red, replace = TRUE),
    UsuarioRed = paste0("usuario_", seq_len(n_red)), stringsAsFactors = FALSE
  )
  
  n_persona <- .n()
  directorio_persona <- data.frame(
    IdPersona = .id_prefijo("PS", n_persona),
    CodContacto = sample(contactos_existentes, n_persona, replace = TRUE),
    Nombre = paste0("Persona Simulada ", seq_len(n_persona)),
    Cargo = sample(c("GERENTE", "COMPRAS", "LOGISTICA", "GERENCIA GENERAL", "COMERCIAL"),
                   n_persona, replace = TRUE),
    Telefono = paste0("3", sample(100000000:999999999, n_persona)),
    Correo = paste0("persona", seq_len(n_persona), "@simulado.com"),
    UsuarioCrea = usr_simulado, FechaHoraCrea = ahora, stringsAsFactors = FALSE
  ) %>%
    group_by(CodContacto) %>% slice_head(n = .MAX_PERSONAS_CONTACTO) %>%
    ungroup() %>% as.data.frame(stringsAsFactors = FALSE)
  
  n_persona_red <- min(.n(), nrow(directorio_persona))
  directorio_persona_red <- data.frame(
    IdRed = .id_prefijo("PR", n_persona_red),
    IdPersona = sample(directorio_persona$IdPersona, n_persona_red, replace = TRUE),
    TipoRedSocial = sample(.TIPOS_RED_SOCIAL, n_persona_red, replace = TRUE),
    UsuarioRed = paste0("usuario_", seq_len(n_persona_red)), stringsAsFactors = FALSE
  )
  
  list(
    CRMNALDIRECTORIOEMPRESARIAL = directorio_empresarial,
    CRMNALDIRECTORIOTELEFONO = directorio_telefono,
    CRMNALDIRECTORIORED = directorio_red,
    CRMNALDIRECTORIOPERSONA = directorio_persona,
    CRMNALDIRECTORIOPERSONARED = directorio_persona_red
  )
}

## Geografia ----
.esquema_tablas_vacias_geografia <- function() {
  list(
    CRMNALUBICACION = data.frame(
      CodContacto = character(), Pais = character(), Depto = character(),
      Mpio = character(), Localidad = character(), Barrio = character(),
      Direccion = character(), lat = double(), lng = double(),
      UsuarioMod = character(), FechaHoraModi = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ),
    CRMNALSUCURSALES = data.frame(
      IdSucursal = character(), CodContacto = character(), Nombre = character(),
      Tipo = character(), Depto = character(), Mpio = character(),
      Localidad = character(), Barrio = character(),
      Direccion = character(), lat = double(), lng = double(),
      EsPrincipal = numeric(), UsuarioCrea = character(),
      FechaHoraCrea = as.POSIXct(character()), stringsAsFactors = FALSE
    )
  )
}
.simular_datos_geografia <- function(n_min = 50, n_max = 60, usr_simulado = "SIMULACION") {
  contactos_existentes <- CargarDatos("CRMNALCONTACTO") %>% pull(CodContacto)
  deptos_muestra <- Unicos(CargarDatos("ANDIVIPOLA")$NomDep)
  ahora <- Sys.time()
  .n <- function() sample(n_min:n_max, 1)
  
  n_ubicacion <- .n()
  ubicacion <- data.frame(
    CodContacto = sample(contactos_existentes, n_ubicacion), Pais = "COLOMBIA",
    Depto = sample(deptos_muestra, n_ubicacion, replace = TRUE),
    Mpio = "SIMULADO", Localidad = "SIMULADA",
    Barrio = paste0("BARRIO SIMULADO ", sample(1:20, n_ubicacion, replace = TRUE)),
    Direccion = paste0("CALLE ", sample(1:100, n_ubicacion, replace = TRUE), " # SIMULADA"),
    lat = runif(n_ubicacion, 4.5, 6.5), lng = runif(n_ubicacion, -76, -73),
    UsuarioMod = usr_simulado, FechaHoraModi = ahora, stringsAsFactors = FALSE
  )
  
  n_sucursal <- .n()
  sucursales <- data.frame(
    IdSucursal = .id_prefijo("SD", n_sucursal),
    CodContacto = sample(contactos_existentes, n_sucursal, replace = TRUE),
    Nombre = paste0("Sede Simulada ", seq_len(n_sucursal)),
    Tipo = sample(.TIPOS_SEDE, n_sucursal, replace = TRUE),
    Depto = sample(deptos_muestra, n_sucursal, replace = TRUE),
    Mpio = "SIMULADO", Localidad = "SIMULADA",
    Barrio = paste0("BARRIO SIMULADO ", sample(1:20, n_sucursal, replace = TRUE)),
    Direccion = paste0("CARRERA ", sample(1:100, n_sucursal, replace = TRUE), " # SIMULADA"),
    lat = runif(n_sucursal, 4.5, 6.5), lng = runif(n_sucursal, -76, -73),
    EsPrincipal = sample(c(0, 1), n_sucursal, replace = TRUE, prob = c(0.8, 0.2)),
    UsuarioCrea = usr_simulado, FechaHoraCrea = ahora, stringsAsFactors = FALSE
  )
  
  list(CRMNALUBICACION = ubicacion, CRMNALSUCURSALES = sucursales)
}

## Potencial ----
.esquema_tablas_vacias_potencial <- function() {
  list(
    CRMNALPOTENCIAL = data.frame(
      CodContacto = character(), EsTostador = numeric(), AlianzaTostadora = numeric(),
      DetalleAlianza = character(), Maquila = numeric(), DetalleMaquila = character(),
      AniosOperacion = numeric(), TipoCompra = character(), CalidadesPreferidas = character(),
      ConsumoEsperado = numeric(), UnidadConsumo = character(), FrecuenciaCompra = character(),
      CertificacionesInteres = character(), Observaciones = character(),
      UsuarioMod = character(), FechaHoraModi = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )
  )
}
.simular_datos_potencial <- function(n_min = 50, n_max = 60, usr_simulado = "SIMULACION") {
  contactos_existentes <- CargarDatos("CRMNALCONTACTO") %>% pull(CodContacto)
  ahora <- Sys.time()
  n_potencial <- sample(n_min:n_max, 1)
  alianza <- sample(c(0, 1), n_potencial, replace = TRUE)
  maquila <- sample(c(0, 1), n_potencial, replace = TRUE)
  
  potencial <- data.frame(
    CodContacto = sample(contactos_existentes, n_potencial),
    EsTostador = sample(c(0, 1), n_potencial, replace = TRUE),
    AlianzaTostadora = alianza,
    DetalleAlianza = ifelse(alianza == 1, "ALIANZA SIMULADA", NA_character_),
    Maquila = maquila,
    DetalleMaquila = ifelse(maquila == 1, "MAQUILA SIMULADA", NA_character_),
    AniosOperacion = sample(1:40, n_potencial, replace = TRUE),
    TipoCompra = sample(.choices_potencial$tipo_compra, n_potencial, replace = TRUE),
    CalidadesPreferidas = "EXCELSO|SUPREMO",
    ConsumoEsperado = sample(10:500, n_potencial, replace = TRUE),
    UnidadConsumo = sample(.choices_potencial$unidad_consumo, n_potencial, replace = TRUE),
    FrecuenciaCompra = sample(.choices_potencial$frecuencia_compra, n_potencial, replace = TRUE),
    CertificacionesInteres = "ORGANICO|FAIR TRADE",
    Observaciones = "Registro simulado para pruebas",
    UsuarioMod = usr_simulado, FechaHoraModi = ahora, stringsAsFactors = FALSE
  )
  list(CRMNALPOTENCIAL = potencial)
}

## Relacionamiento ----
.esquema_tablas_vacias_relacionamiento <- function() {
  list(
    CRMNALRELACIONAMIENTO = data.frame(
      IdRelacionamiento = character(), CodContacto = character(), TipoGestion = character(),
      Comentario = character(), UsuarioCrea = character(), FechaHoraCrea = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ),
    CRMNALRECORDATORIO = data.frame(
      IdRecordatorio = character(), CodContacto = character(), Asesor = character(),
      FechaRecordatorio = as.POSIXct(character()), Canal = character(), Mensaje = character(),
      Enviado = numeric(), FechaHoraEnvio = as.POSIXct(character()), UsuarioCrea = character(),
      FechaHoraCrea = as.POSIXct(character()), stringsAsFactors = FALSE
    )
  )
}
.simular_datos_relacionamiento <- function(n_min = 50, n_max = 60, usr_simulado = "SIMULACION") {
  contactos_existentes <- CargarDatos("CRMNALCONTACTO") %>% pull(CodContacto)
  ahora <- Sys.time()
  .n <- function() sample(n_min:n_max, 1)
  
  n_relacionamiento <- .n()
  relacionamiento <- data.frame(
    IdRelacionamiento = .id_prefijo("RL", n_relacionamiento),
    CodContacto = sample(contactos_existentes, n_relacionamiento, replace = TRUE),
    TipoGestion = sample(.TIPOS_RELACIONAMIENTO, n_relacionamiento, replace = TRUE),
    Comentario = sample(
      c("Se realizo contacto comercial con el cliente.",
        "Cliente solicita informacion adicional sobre productos.",
        "Se realizo seguimiento a conversacion comercial previa.",
        "Cliente manifiesta interes en recibir una propuesta.",
        "Se revisaron necesidades y posibilidades de negocio.",
        "Se realizo llamada de seguimiento sin novedades.",
        "Cliente solicita ser contactado nuevamente.",
        "Se realizo reunion para revisar oportunidades comerciales."),
      n_relacionamiento, replace = TRUE
    ),
    UsuarioCrea = usr_simulado,
    FechaHoraCrea = ahora - days(sample(0:90, n_relacionamiento, replace = TRUE)) -
      hours(sample(0:8, n_relacionamiento, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  n_recordatorio <- .n()
  enviado <- sample(c(0, 1), n_recordatorio, replace = TRUE, prob = c(0.7, 0.3))
  fecha_recordatorio <- ahora + days(sample(-30:45, n_recordatorio, replace = TRUE)) +
    hours(sample(7:17, n_recordatorio, replace = TRUE))
  recordatorio <- data.frame(
    IdRecordatorio = .id_prefijo("RC", n_recordatorio),
    CodContacto = sample(contactos_existentes, n_recordatorio, replace = TRUE),
    Asesor = sample(Choices()$personas, n_recordatorio, replace = TRUE),
    FechaRecordatorio = fecha_recordatorio,
    Canal = sample(.CANALES_RECORDATORIO, n_recordatorio, replace = TRUE),
    Mensaje = sample(
      c("Realizar seguimiento comercial.", "Contactar al cliente para conocer avance.",
        "Enviar informacion solicitada.", "Programar reunion de seguimiento.",
        "Consultar interes en nueva oportunidad.", "Realizar llamada de seguimiento.",
        "Validar necesidades actuales del cliente.", "Retomar conversacion comercial pendiente."),
      n_recordatorio, replace = TRUE
    ),
    Enviado = enviado,
    FechaHoraEnvio = as.POSIXct(
      ifelse(enviado == 1, as.numeric(fecha_recordatorio), NA_real_), origin = "1970-01-01"
    ),
    UsuarioCrea = usr_simulado, FechaHoraCrea = ahora - days(sample(1:60, n_recordatorio, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  list(CRMNALRELACIONAMIENTO = relacionamiento, CRMNALRECORDATORIO = recordatorio)
}

## Promover ----
.esquema_tablas_vacias_promover <- function() {
  list(
    CRMNALPROSPECTOALIANZA = data.frame(
      IdAlianza = character(), CodContacto = character(), CodClienteAliado = character(),
      UsuarioCrea = character(), FechaHoraCrea = as.POSIXct(character()),
      Observacion = character(), stringsAsFactors = FALSE
    ),
    CRMNALVINCULONIT = data.frame(
      CodContacto = character(), NitVinculado = character(), UsuarioVinculo = character(),
      FechaHoraVinculo = as.POSIXct(character()), Observacion = character(),
      stringsAsFactors = FALSE
    ),
    CRMNALHISTORIALETAPA = data.frame(
      CodContacto = character(), EtapaAnterior = character(), EtapaNueva = character(),
      Usuario = character(), FechaHora = as.POSIXct(character()), Motivo = character(),
      stringsAsFactors = FALSE
    ),
    CONTACTOLEAD = data.frame(
      CodContacto = character(), FechaConversion = as.POSIXct(character()),
      CodLinNegocio = character(), LinNegocio = character(),
      Asesor = character(), Segmento = character(), stringsAsFactors = FALSE
    )
  )
}
.simular_datos_promover <- function(n_min = 20, n_max = 30, usr_simulado = "SIMULACION") {
  contactos_existentes <- CargarDatos("CRMNALCONTACTO") %>% pull(CodContacto)
  clientes_ppal <- names(.choices_cliente_ppal())
  ahora <- Sys.time()
  .n <- function() sample(n_min:n_max, 1)
  
  n_prospecto <- .n()
  n_lead_directo <- .n()
  contactos_prospecto <- sample(contactos_existentes, n_prospecto)
  contactos_lead_directo <- sample(setdiff(contactos_existentes, contactos_prospecto), n_lead_directo)
  
  n_reclasificados <- min(.n(), length(contactos_prospecto))
  contactos_reclasificados <- sample(contactos_prospecto, n_reclasificados)
  
  alianzas <- data.frame(
    IdAlianza = .id_prefijo("AL", length(contactos_prospecto)),
    CodContacto = contactos_prospecto,
    CodClienteAliado = sample(clientes_ppal, length(contactos_prospecto), replace = TRUE),
    UsuarioCrea = usr_simulado, FechaHoraCrea = ahora,
    Observacion = "Alianza simulada para pruebas", stringsAsFactors = FALSE
  )
  
  contactos_lead <- c(contactos_lead_directo, contactos_reclasificados)
  leads <- data.frame(
    CodContacto = contactos_lead,
    FechaConversion = ahora - days(sample(0:60, length(contactos_lead), replace = TRUE)),
    CodLinNegocio = sample(c("10000", "21000"), length(contactos_lead), replace = TRUE),
    LinNegocio = NA_character_,
    Asesor = sample(Choices()$personas, length(contactos_lead), replace = TRUE),
    Segmento = sample(Choices()$segmento, length(contactos_lead), replace = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    mutate(LinNegocio = ifelse(CodLinNegocio == "21000", "A LA MEDIDA", "CONVENCIONALES"))
  
  n_vinculo <- min(.n(), length(contactos_lead))
  contactos_vinculo <- sample(contactos_lead, n_vinculo)
  vinculos <- data.frame(
    CodContacto = contactos_vinculo,
    NitVinculado = as.character(sample(100000000:999999999, n_vinculo)),
    UsuarioVinculo = usr_simulado, FechaHoraVinculo = ahora,
    Observacion = "Vinculo simulado para pruebas", stringsAsFactors = FALSE
  )
  
  historial <- dplyr::bind_rows(
    data.frame(CodContacto = contactos_prospecto, EtapaAnterior = "CONTACTO",
               EtapaNueva = "PROSPECTO", Usuario = usr_simulado, FechaHora = ahora,
               Motivo = NA_character_, stringsAsFactors = FALSE),
    data.frame(CodContacto = contactos_lead_directo, EtapaAnterior = "CONTACTO",
               EtapaNueva = "LEAD", Usuario = usr_simulado, FechaHora = ahora,
               Motivo = NA_character_, stringsAsFactors = FALSE),
    data.frame(CodContacto = contactos_reclasificados, EtapaAnterior = "PROSPECTO",
               EtapaNueva = "LEAD", Usuario = usr_simulado, FechaHora = ahora + 1,
               Motivo = NA_character_, stringsAsFactors = FALSE)
  )
  
  list(CRMNALPROSPECTOALIANZA = alianzas, CONTACTOLEAD = leads,
       CRMNALVINCULONIT = vinculos, CRMNALHISTORIALETAPA = historial)
}

## Oportunidades ----
# CRMNALCLOPT es una unica tabla compartida por CrearOportunidad() y
# DescartarOportunidad(): esta ultima solo hace UPDATE (Descartado/Razon)
# sobre filas ya existentes, no agrega tabla nueva - por eso un solo
# esquema/simulador cubre ambas acciones del submodulo
.esquema_tablas_vacias_oportunidades <- function() {
  list(
    CRMNALCLOPT = data.frame(
      IdOportunidad = character(), CodContacto = character(), LinNegCod = numeric(),
      Categoria = character(), Producto = character(), FechaCumpOP = as.Date(character()),
      Sacos = numeric(), Margen = numeric(), Comentarios = character(), Etapa = character(),
      Descartado = character(), Razon = character(), UsuarioCrea = character(),
      FechaHoraCrea = as.POSIXct(character()), stringsAsFactors = FALSE
    )
  )
}
.simular_datos_oportunidades <- function(n_min = 50, n_max = 60, usr_simulado = "SIMULACION") {
  contactos_existentes <- CargarDatos("CRMNALCONTACTO") %>% pull(CodContacto)
  prods <- CargarDatos("CRMNALPRODS") %>% filter(Excluir != "SI")
  ahora <- Sys.time()
  n_op <- sample(n_min:n_max, 1)
  
  # Muestrea filas completas de CRMNALPRODS (no columnas por separado) para
  # que Categoria/Producto/LinNeg queden coherentes entre si, igual que
  # exige la cascada LinNeg -> Categoria -> Producto de CrearOportunidad
  filas_prod <- prods[sample(seq_len(nrow(prods)), n_op, replace = TRUE), ]
  descartado <- sample(c("SI", "NO"), n_op, replace = TRUE, prob = c(0.2, 0.8))
  
  oportunidades <- data.frame(
    IdOportunidad = .id_prefijo("OP", n_op),
    CodContacto = sample(contactos_existentes, n_op, replace = TRUE),
    LinNegCod = sapply(filas_prod$LinNeg, function(ln) as.numeric(determinar_linea_negocio(ln)$cod)),
    Categoria = filas_prod$Categoria,
    Producto = filas_prod$Producto,
    FechaCumpOP = Sys.Date() + sample(-30:60, n_op, replace = TRUE),
    Sacos = round(runif(n_op, 5, 300), 2),
    Margen = sample(200:2000, n_op, replace = TRUE),
    Comentarios = NA_character_,
    Etapa = sample(c("CONTACTO", "PROSPECTO", "LEAD", "CLIENTE"), n_op, replace = TRUE),
    Descartado = descartado,
    Razon = ifelse(descartado == "SI", sample(.RAZONES_DESCARTE_OPORTUNIDAD, n_op, replace = TRUE), NA_character_),
    UsuarioCrea = usr_simulado, FechaHoraCrea = ahora - days(sample(0:60, n_op, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  list(CRMNALCLOPT = oportunidades)
}

## ConversionCliente ----
# CRMNALLEADCLIENTE es escrita unicamente por detectar_conversion_leads()
# (Generales Embudo): detecta automaticamente cuando un Lead (o su NIT
# vinculado) ya tiene primera factura en `data`, y en ese mismo paso llama
# a registrar_transicion_etapa(..., "LEAD", "CLIENTE", ...) -> Promover.
# Es un submodulo propio y no se fusiona con Promover porque el trigger es
# automatico (deteccion por facturacion), no una accion manual de usuario
# como las demas transiciones de Promover
#
# AJUSTE PENDIENTE (anotado por el usuario): un "Cliente" real es la
# combinacion NIT + Linea de Negocio, no solo el NIT/CodContacto. Hoy
# detectar_conversion_leads() (codigo compartido) NO propaga LinNeg al
# insertar en CRMNALLEADCLIENTE, aunque CONTACTOLEAD ya trae CodLinNegocio/
# LinNegocio para ese mismo CodContacto. El esquema de abajo YA incluye las
# columnas para cuando se actualice la funcion real; el simulador las
# propaga desde CONTACTOLEAD por join, como referencia de la logica que
# detectar_conversion_leads() deberia aplicar
# CONFIRMADO por el usuario: no existen casos donde el NIT vinculado
# facture bajo una linea de negocio distinta a la que tenia el Lead. Por
# eso el join basta contra CONTACTOLEAD (por CodContacto) - NO se necesita
# cruzar contra `data`/primera_factura_por_nit para resolver LinNeg
.esquema_tablas_vacias_conversioncliente <- function() {
  list(
    CRMNALLEADCLIENTE = data.frame(
      CodContacto = character(), CodLinNegocio = character(), LinNegocio = character(),
      NitFacturacion = character(), FechaConversion = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )
  )
}
.simular_datos_conversioncliente <- function(n_min = 10, n_max = 20, usr_simulado = "SIMULACION") {
  # Solo Leads activos son candidatos a "ya facturaron" - mismo filtro que
  # detectar_conversion_leads() (Etapa == "LEAD"), para no simular una
  # conversion de un contacto que nunca fue Lead
  leads_activos <- derivar_etapa_actual() %>% filter(Etapa == "LEAD")
  if (nrow(leads_activos) == 0) {
    return(list(CRMNALLEADCLIENTE = data.frame(
      CodContacto = character(), CodLinNegocio = character(), LinNegocio = character(),
      NitFacturacion = character(), FechaConversion = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )))
  }
  
  # Linea de negocio propagada desde CONTACTOLEAD, no fabricada aparte -
  # asi el simulador queda coherente con la anotacion de que Cliente es
  # NIT + LinNeg, y sirve de referencia para el join que necesitara
  # detectar_conversion_leads() cuando se actualice
  lin_neg_lead <- CargarDatos("CONTACTOLEAD") %>%
    select(CodContacto, CodLinNegocio, LinNegocio)
  
  n_conv <- min(sample(n_min:n_max, 1), nrow(leads_activos))
  contactos_conv <- sample(leads_activos$CodContacto, n_conv)
  
  data.frame(CodContacto = contactos_conv, stringsAsFactors = FALSE) %>%
    left_join(lin_neg_lead, by = "CodContacto") %>%
    mutate(
      NitFacturacion = as.character(sample(100000000:999999999, n_conv)),
      FechaConversion = Sys.time() - days(sample(0:60, n_conv, replace = TRUE))
    ) %>%
    list(CRMNALLEADCLIENTE = .)
  # NOTA: a diferencia de detectar_conversion_leads() real, este simulador
  # NO inserta la fila espejo en CRMNALHISTORIALETAPA (Promover). Se deja
  # asi a proposito: si se quiere que estos contactos tambien aparezcan
  # como CLIENTE en derivar_etapa_actual(), ejecutar despues
  # registrar_transicion_etapa(cod, "LEAD", "CLIENTE", usr_simulado) por
  # cada CodContacto, igual que hace la funcion real - no se duplica aqui
  # para no arrastrar logica de negocio dentro de un simulador de datos
}

## Descartar / Reactivar ----
# CRMNALDESCARTE — TABLA DE PRUEBA
.esquema_tablas_vacias_descartar <- function() {
  list(
    CRMNALDESCARTE = data.frame(
      CodContacto = character(), Etapa = character(), Razon1 = character(),
      Razon2 = character(), Razon3 = character(), UsuarioMod = character(),
      FechaHoraModi = as.POSIXct(character()), stringsAsFactors = FALSE
    )
  )
}
.simular_datos_descartar <- function(n_min = 20, n_max = 30, usr_simulado = "SIMULACION") {
  # Solo contactos actualmente DESCARTADO son candidatos - upsert coherente
  # con el estado vigente, igual que .obtener_etapa_pre_descarte()
  descartados <- CargarDatos("CRMNALCONTACTO") %>% filter(Estado == "DESCARTADO")
  if (nrow(descartados) == 0) {
    return(list(CRMNALDESCARTE = data.frame(
      CodContacto = character(), Etapa = character(), Razon1 = character(),
      Razon2 = character(), Razon3 = character(), UsuarioMod = character(),
      FechaHoraModi = as.POSIXct(character()), stringsAsFactors = FALSE
    )))
  }
  if (!"EtapaPreDescarte" %in% names(descartados)) descartados$EtapaPreDescarte <- "CONTACTO"
  
  n_reg <- min(sample(n_min:n_max, 1), nrow(descartados))
  filas <- descartados[sample(seq_len(nrow(descartados)), n_reg), ]
  
  # Razon muestreada del catalogo correspondiente a la EtapaPreDescarte de
  # cada contacto, no de un catalogo generico - respeta que .RAZONES_DESCARTE
  # esta particionado por etapa
  razones <- vapply(filas$EtapaPreDescarte, function(et) {
    catalogo <- .RAZONES_DESCARTE[[et]] %||% .RAZONES_DESCARTE[["CONTACTO"]]
    sample(catalogo, 1)
  }, character(1))
  
  list(CRMNALDESCARTE = data.frame(
    CodContacto = filas$CodContacto,
    Etapa = filas$EtapaPreDescarte,
    Razon1 = razones,
    Razon2 = "Registro simulado para pruebas",
    Razon3 = NA_character_,
    UsuarioMod = usr_simulado,
    FechaHoraModi = Sys.time() - days(sample(0:60, n_reg, replace = TRUE)),
    stringsAsFactors = FALSE
  ))
}

# reactivar_contacto() (confirmado): solo CRMNALCONTACTO + CRMNALHISTORIALETAPA,
# misma familia que registrar_transicion_etapa()/.insertar_conversion_lead()
# (Promover). Nada que agregar aqui para ese verbo.

# Ejecucion ----

## 0. Inventario completo, ordenado por submodulo/etapa
mapa_submodulos <- .tabla_mapa_submodulos()
inventario_racafe <- InventariarTablasRacafe(unlist(.MAPA_SUBMODULO_TABLAS, use.names = FALSE)) %>%
  dplyr::left_join(mapa_submodulos, by = "Tabla") %>%
  dplyr::arrange(Submodulo, dplyr::desc(Registros))
inventario_racafe

## 1. Creacion de tablas vacias, por submodulo (orden = etapa del flujo)
EscribirTablasVacias(.esquema_tablas_vacias_directorio())
EscribirTablasVacias(.esquema_tablas_vacias_geografia())
EscribirTablasVacias(.esquema_tablas_vacias_potencial())
EscribirTablasVacias(.esquema_tablas_vacias_relacionamiento())
EscribirTablasVacias(.esquema_tablas_vacias_promover())
EscribirTablasVacias(.esquema_tablas_vacias_oportunidades())
EscribirTablasVacias(.esquema_tablas_vacias_conversioncliente())
EscribirTablasVacias(.esquema_tablas_vacias_descartar())
# Reactivar: sin esquema propio, ver nota arriba

## 2. Simulacion de datos, en orden de dependencia:
##    Directorio/Geografia/Potencial/Relacionamiento dependen solo de
##    CRMNALCONTACTO (ya poblada en produccion); Promover ademas depende
##    de .choices_cliente_ppal(), que a su vez lee CRMNALCONTACTO;
##    ConversionCliente depende de que existan Leads activos; Descartar
##    depende de que existan contactos con Estado == "DESCARTADO" en
##    produccion (ningun simulador de este script cambia el Estado de
##    CRMNALCONTACTO, se respeta como dato real)
PoblarTablasSimuladas(.simular_datos_directorio())
PoblarTablasSimuladas(.simular_datos_geografia())
PoblarTablasSimuladas(.simular_datos_potencial())
PoblarTablasSimuladas(.simular_datos_relacionamiento())
PoblarTablasSimuladas(.simular_datos_promover())
PoblarTablasSimuladas(.simular_datos_oportunidades())

# ConversionCliente: se captura el resultado para completar el paso que el
# simulador deja pendiente a proposito (ver nota en .simular_datos_conversioncliente) -
# sin este paso, derivar_etapa_actual() nunca mostraria estos contactos como
# CLIENTE, y no habria clientes para probar Descartar/Reactivar
conversion_cliente_simulada <- .simular_datos_conversioncliente()
PoblarTablasSimuladas(conversion_cliente_simulada)
invisible(lapply(conversion_cliente_simulada$CRMNALLEADCLIENTE$CodContacto, function(cod) {
  registrar_transicion_etapa(cod, "LEAD", "CLIENTE", "SIMULACION")
}))

PoblarTablasSimuladas(.simular_datos_descartar())

## 3. Verificacion final: mismo inventario, para confirmar que cada
##    submodulo quedo poblado tras el paso 2
InventariarTablasRacafe(unlist(.MAPA_SUBMODULO_TABLAS, use.names = FALSE)) %>%
  dplyr::left_join(mapa_submodulos, by = "Tabla") %>%
  dplyr::arrange(Submodulo, dplyr::desc(Registros))