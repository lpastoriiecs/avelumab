library(shiny)
library(shinyjs)
source("login.R")
iButton <- function(ibId) {
  return(tags$i(class = "glyphicon glyphicon-info-sign", id = ibId, width = 200))
}
iLabel <- function(labelDesc, ibId) {
  return(div(
    class = "label-group",
    tags$label(labelDesc),
    iButton(ibId)
  ))
}

iForm <- function(labelDesc, ibId, input) {
  return(div(
    class = "form-group",
    iLabel(labelDesc, ibId),
    input
  ))
}

iForm2 <- function(ibId, input) {
  return(div(
    class = "form-group",
    iLabel(etiquetas[[ibId]], ibId),
    input
  ))
}
iFormSlider <- function(slider, Id) {
  return(div(
    class = "form-group",
    iLabel(etiquetas[[Id]], Id),
    slider,
  ))
}
fNumInput <- function(Id) {
  return(div(
    class = "form-group",
    iLabel(etiquetas[[Id]], Id),
    numInput(Id)
  ))
}

rateTable <- function(numTech, hTemporal, techName, techId, sufijo, ancho, resto = 0, restoName = "", restoId = "") {
  header <- fluidRow(
    column(3, div("Tecnologia", class = "rtHeader fCol")),
    column(9, fluidRow(
      lapply(1:hTemporal, function(i) {
        column(ancho, div(paste("Año", i), class = "rtHeader"))
      })
    ))
  )
  filas <- lapply(1:numTech, function(i) {
    fluidRow(
      column(3, div(techName[i], class = "rtName")),
      column(9, fluidRow(
        lapply(1:hTemporal, function(j) {
          column(ancho, autonumericInput(
            paste0(techId[i], sufijo, j),
            label = NULL,
            value = NULL,
            class = "rtInput",
            align = "center",
            currencySymbol = "%", currencySymbolPlacement = "s", decimalCharacter = ",", digitGroupSeparator = ".", minimumValue = 0, maximumValue = 100, decimalPlaces = selected_decimales[[paste0(techId[i], sufijo, j)]]
          ))
        })
      ))
    )
  })
  if (resto == 1) {
    filas <- c(filas, list(fluidRow(
      column(3, div(restoName, class = "rtName")),
      column(9, fluidRow(
        lapply(1:hTemporal, function(j) {
          column(ancho, div(
            id = paste0(restoId, sufijo, j),
            class = "rtInput rtRemainder",
            "0 %"
          ))
        })
      ))
    )))
  }
  # Empaquetar todas las filas dentro de un contenedor
  return(do.call(div, c(list(class = "rtContainer"), list(header), filas)))
}

numInput <- function(iId) {
  print(iId)
  return(
    switch(selected_opciones[[iId]],
      autonumericInput(iId, label = NULL, value = 0, align = "center", currencySymbol = "$", currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", decimalPlaces = selected_decimales[[iId]]),
      autonumericInput(iId, label = NULL, value = 0, align = "center", currencySymbol = "%", currencySymbolPlacement = "s", decimalCharacter = ",", digitGroupSeparator = ".", minimumValue = 0, maximumValue = 100, decimalPlaces = selected_decimales[[iId]]),
      autonumericInput(iId, label = NULL, value = 0, align = "center", decimalCharacter = ",", digitGroupSeparator = ".", decimalPlaces = selected_decimales[[iId]]),
    )
  )
}
panelResPrincipales <- tags$div(
  class = "panelRes",
  div(
    class = "tablas-escenarios",
    div(
      class = "tablaResultados",
      h4("Impacto", class = "tabla-titulo"),
      div(
        class = "tablawrapper",
        tableOutput("tablaMain")
      )
    ),
    div(class = "pieTabla", ),
    div(class = "pieTabla", ),
  ),
 div(
    style = "margin-top:20px; width:100%; text-align:center;",
    bslib::card(
      bslib::card_header(
        "Impacto PMPM - Gráfico comparativo entre intervenciones",
        class = "bg-light text-dark fw-bold"
      ),
      bslib::card_body(
        plotOutput("grhPMPMComparativo", height = "600px", width = "100%")
      )
    )
  ),
)
panelResAlternativos <- uiOutput("panelAlternativo")

panelResCostos <- tags$div(
  class = "panelRes",
  div(
    class = "tablas-escenarios",
    div(
      class = "tablaResultados",
      h4("Escenario Actual", class = "tabla-titulo"),
      tableOutput("tablaCostos")
    ),
  ),
  div(
    class = "tablas-escenarios",
    div(
      class = "tablaResultados",
      h4("Escenario Proyectado", class = "tabla-titulo"),
      tableOutput("tablaCostosProy")
    ),
  ),
  div(
    class = "tablas-escenarios",
    div(
      class = "tablaResultados",
      h4("Diferencia de Costos", class = "tabla-titulo"),
      tableOutput("tablaCostosDiff")
    ),
  ),
)
panelResCostosR <- tags$div(
  class = "panelRes",
  plotOutput("grhCostosR", height = "550px", width = "100%")
)

panelcitoOutput <- function(titulo, valor, color, icono) {
  return(tags$div(
    class = "panelcitoOutput", style = paste0("background-color: ", color, ";"),
    tags$div(class = "panelcitoTitulo", titulo),
    tags$div(class = "panelcitoFilaInferior", tags$i(class = icono), tags$div(class = "panelcitoValor", valor))
  ))
}

# Variante horizontal: valor (+ unidad opcional abajo) a la izquierda, descripción a la derecha
panelFilaOutput <- function(descripcion, valor, unidad = NULL, color = COLOR_PRIMARIO) {
  tags$div(
    class = "panelFilaOutput",
    style = paste0("background-color: ", color, ";"),
    tags$div(
      class = "panelFilaValorWrapper",
      tags$div(class = "panelFilaValor", valor),
      if (!is.null(unidad)) tags$div(class = "panelFilaUnidad", unidad)
    ),
    tags$div(class = "panelFilaDesc", descripcion)
  )
}


panelCoC <- div(
  class = "panelRes",
  div(
    class = "tablas-escenarios",
    div(
      class = "tablaResultados",
      h4("Costo de Cuidado", class = "tabla-titulo"),
      tableOutput("tablaCoc")
    )
  ),
  div(
    style = "margin-top:20px; width:100%; text-align:center;",
    bslib::card(
      class = "mb-4",
      bslib::card_header(
        "Costo Total por Intervención",
        class = "bg-light text-dark fw-bold"
      ),
      bslib::card_body(
        div(
          style = "max-width: 400px; margin: 0 auto 20px auto;",
          selectInput(
            inputId = "selectorComponenteCosto",
            label = "Seleccione el Componente de Costo:",
            choices = c(
              "Total",
              "Costos de Adquisición",
              "Costos de Administración",
              "Costos de Efectos Adversos",
              "Costos de Manejo de la Enfermedad y Monitoreo",
              "Costos de Adquisición y Administración de Tratamientos Subsecuentes",
              "Costos de Monitoreo de tratamientos Subsecuentes"
            ),
            selected = "Total"
          )
        ),
        plotOutput("grhCOCTotal", height = "600px", width = "100%")
      )
    )
  ),
  div(
    style = "margin-top:20px; width:100%; text-align:center;",
    bslib::card(
      bslib::card_header(
        "Proporción de Costos por Intervención",
        class = "bg-light text-dark fw-bold"
      ),
      bslib::card_body(
        plotOutput("grhCOC", height = "600px", width = "100%")
      )
    )
  )
)

panelVisualizador <- div(
  id = "visualizadorPanel",
  actionButton("toggleParamBarTop", label = NULL, icon = tags$i(class = "fa fa-chevron-down"), class = "menu-ocultar-top"),
  div(
    class = "colInputs visCollapsed", id = "colInputs",
    # Botón para mostrar/ocultar el sidebar
    tags$div(
      class = "input-bar",
      actionButton("toggleParamBar", label = NULL, icon = tags$i(class = "fas fa-chevron-left"), class = "menu-ocultar"),
      tags$h4("Parámetros", class = "input-title")
    ),
    tags$div(
      id = "input-content",
      selectInput(
        inputId = "perspectiva",
        label = "Selecciona un Sector:",
        choices = sectores,
        selected = "PAMI"
      ),
      fNumInput("nAfiliados"),
      iForm(
        "Horizonte Temporal", "tHT",
        sliderInput("tHT", NULL, value = 0, min = 1, max = 5, )
      ),
      fNumInput("pAnualGrowth"),
      fNumInput("cAvelumab"),
      fNumInput("cEV"),
      fNumInput("cPembro"),
      fNumInput("cNivolumab"),
      div(
        style = "display:flex; gap:0px;",
        prettyCheckbox("bInflacion", "Ajustar costos por Inflación", shape = "square", status = "primary", outline = TRUE, icon = icon("check", class = "chkIcon"), value = TRUE),
        iButton("inflacion")
      )
    ),
  ),
  div(
    id = "colRes",
    class = "colRes",
    h3("Indicadores Promedio Por Año"),
    div(
      class = "resTabCarteles",
      panelFilaOutput("tratados con Avelumab (Incremental)", textOutput("deltaIndicador1"), COLOR_PRIMARIO, unidad = "pacientes"),
      panelcitoOutput("Impacto Presupuestario", textOutput("deltaIndicador2"), COLOR_PRIMARIO, "fas fa-dollar-sign"),
      panelcitoOutput("Impacto Presupuestario", textOutput("deltaIndicador3"), COLOR_PRIMARIO, "fas fa-dollar-sign"),
      panelFilaOutput("el impacto presupuestario utilizando Cisplatino+Nivolumab", textOutput("deltaIndicador4"), COLOR_PRIMARIO, unidad = "veces"),
      panelFilaOutput("el impacto presupuestario utilizando Enfortumab Vedotin + Pembrolizumab", textOutput("deltaIndicador5"), COLOR_PRIMARIO, unidad = "veces"),
      # panelcitoOutput("Indicador", textOutput("deltaIndicador6"), COLOR_PRIMARIO, "fas fa-dollar-sign"),
    ),
    div(
      id = "resTabDiv",
      navset_card_underline(
        id = "resTab",
        title = "Resultados",
        nav_panel("Principales", panelResPrincipales),
        nav_panel("Costos Resumidos", panelResCostosR),
        nav_panel("Costos Detallados", panelResCostos),
        nav_panel("Alternativos", panelResAlternativos),
        nav_panel("Costo de Cuidado", panelCoC),
        full_screen = TRUE
      )
    ),
  )
)


panelIntroduccion <- div(
  id = "introduccionPanel",
  div(
    class = "introPanel",
    includeMarkdown("www/introduccion.rmd"),
    actionButton("cmdEmpezar", "Empezar", class = "cmdPrimary")
  )
)

panelConfiguracion <- div(
  id = "configuracionPanel",
  div(
    class = "card-wrapper",
    div(
      class = "card-wrapper-header",
      h3("Costos de Tratamiento"),
      actionButton("toggle_costos", "−", class = "card-wrapper-min")
    ),
    div(
      id = "card-costos", class = "card-wrapper-body",
      fluidRow(
        column(
          3,
          fNumInput("cCisplatino"),
        ),
        column(
          3,
          fNumInput("cCarboplatino"),
        ),
        column(
          3,
          fNumInput("cGemcitabine"),
        ),
        column(
          3,
          fNumInput("cAdministracion"),
        )
      )
    )
  ),
  div(
    class = "card-wrapper",
    div(
      class = "card-wrapper-header",
      h3("Tasas de Mercado"),
      actionButton("toggle_tasas", "−", class = "card-wrapper-min")
    ),
    div(
      id = "card-tasas", class = "card-wrapper-body",
      fluidRow(
        column(
          6,
          h4("Escenario Actual"),
          h5("Primera Linea"),
          rateTable(2, 5, c("Enfortumab Vedotin + Pembrolizumab", "Nivolumab"), c("msEVPE", "msNIVE"), "B", 2, resto = 1, restoName = "Quimioterapia", restoId = "QMT"),
          h5("Mantenimiento dado que recibió quimioterapia basda en platinos"),
          rateTable(1, 5, c("Avelumab"), c("msAVEE"), "B", 2, resto = 1, restoName = "BSC", restoId = "BSC"),
        ),
        column(
          6,
          h4("Escenario Proyectado"),
          h5("Primera Linea"),
          rateTable(2, 5, c("Enfortumab Vedotin + Pembrolizumab", "Nivolumab"), c("msEVPE", "msNIVE"), "P", 2, resto = 1, restoName = "Quimioterapia", restoId = "QMT"),
          h5("Mantenimiento dado que recibió quimioterapia basda en platinos"),
          rateTable(1, 5, c("Avelumab"), c("msAVEE"), "P", 2, resto = 1, restoName = "BSC", restoId = "BSC"),
        )
      )
    )
  ),
  div(
    class = "card-wrapper",
    div(
      class = "card-wrapper-header",
      h3("Costos de Efectos Adversos"),
      actionButton("toggle_ea", "−", class = "card-wrapper-min")
    ),
    div(
      id = "card-ea", class = "card-wrapper-body",
      fluidRow(
        column(
          4,
          fNumInput("cEA_Diarrea"),
          fNumInput("cEA_Plqd"),
          fNumInput("cEA_Rash"),
          # fNumInput("uTBCTratadaMDR"),
        ),
        column(
          4,
          fNumInput("cEA_ITU"),
          fNumInput("cEA_GBd"),
          fNumInput("cEA_Neutd"),
          # fNumInput("pMuerteMDR")
        ),
        column(
          4,
          fNumInput("cEA_Anemia"),
          fNumInput("cEA_Hiperglu"),
          fNumInput("cEA_Pnp"),
          # fNumInput("pBCPEspecificidad")
        )
      )
    )
  ),
  div(
    class = "card-wrapper",
    div(
      class = "card-wrapper-header",
      h3("Tratamientos Subsecuentes"),
      actionButton("toggle_subsecuentes", "−", class = "card-wrapper-min")
    ),
    div(
      id = "card-subsecuentes", class = "card-wrapper-body",
      fluidRow(
        column(
          4,
          fNumInput("pSD_AVE"),
          fNumInput("pSD_NIV"),
          fNumInput("cVinflunine"),
        ),
        column(
          4,
          fNumInput("pSD_BSC"),
          fNumInput("pSD_EVP"),
          fNumInput("cPaclitaxel"),
        ),
        column(
          4,
          fNumInput("pSD_QMTNR"),
          fNumInput("cAtezolizumab"),
          fNumInput("cErdafitinib"),
        )
      ),
    )
  ),
)


panelEscenarios <- div(
  id = "escenariosPanel",
  h3("Escenarios")
)
panelReporte <- div(
  id = "reportesPanel",
  h3("Reportes")
)

panelAPP <- div(
  id = "appPanel",
  div(
    class = "app-container",
    # Botón para mostrar/ocultar el sidebar
    tags$div(
      class = "top-bar",
      tags$div(
        class = "top-bar-component",
        actionButton("toggleSidebar", label = NULL, icon = tags$i(class = "fa icon-bars"), class = "menu-btn"),
        tags$h3("AIP Avelumab", class = "title")
      ),
      tags$div(
        class = "top-bar-component",
        tags$img(src = "imagenes/LOGO_IECS.png", height = "50px")
      )
    ),
    tags$div(
      class = "top-bar-shadow-blocker",
    ),
    # Contenedor general
    div(
      class = "main-wrapper",
      div(
        id = "sidebar", class = "sidebar collapsed",
        actionLink("navIntroduccion", label = tagList(tags$i(class = "fa fa-book-open"), span("Introduccion")), class = "sidebar-link active"),
        actionLink("navVisualizador", label = tagList(tags$i(class = "fa fa-chart-bar"), span("Visualizador")), class = "sidebar-link"),
        actionLink("navConfiguracion", label = tagList(tags$i(class = "fas fa-tools"), span("Configuración")), class = "sidebar-link")
        # actionLink("navEscenarios", label = tagList(tags$i(class = "fas icon-save"), span("Escenarios")), class = "sidebar-link"),
        # actionLink("navReporte", label = tagList(tags$i(class = "fa fa-file-alt"), span("Reporte")), class = "sidebar-link")
      ),
      tabsetPanel(
        id = "content",
        tabPanel("Introduccion", value = "Introduccion", panelIntroduccion),
        tabPanel("Visualizador", value = "Visualizador", panelVisualizador),
        tabPanel("Configuracion", value = "Configuracion", panelConfiguracion)
        # tabPanel("Escenarios", value = "Escenarios", panelEscenarios),
        # tabPanel("Reporte", value = "Reporte", panelReporte),
      )
    )
  )
)

ui <- fluidPage(
  useWaiter(),
  useSweetAlert(),
  useShinyFeedback(),
  theme = bs_theme(
    version = 5,
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("login-styles.css?v=", as.integer(Sys.time()))),
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("main.css?v=", as.integer(Sys.time()))),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"), # Cargar Font Awesome desde CDN
    tags$script(src = paste0("script.js?v=", as.integer(Sys.time()))),
  ),
  useShinyjs(),
  (mod_login_ui(LOGIN_MODULO)),
  hidden(panelAPP),
  # hidden(mod_login_ui(LOGIN_MODULO)),
  # (panelAPP)
)
