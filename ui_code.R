library(shiny)
library(shinyjs)
source("login.R")
iButton <- function(ibId) {
  return (tags$i(class = "glyphicon glyphicon-info-sign", id = ibId ,width = 200))
}
iLabel <- function(labelDesc, ibId) {
  return (div(class = "label-group",
              tags$label(labelDesc),
              iButton(ibId)
              ))
}

iForm <- function(labelDesc, ibId, input) {
  return (div(class = "form-group",
              iLabel(labelDesc, ibId),
              input
              ))
}

iForm2 <- function(ibId, input) {
  return (div(class = "form-group",
              iLabel(etiquetas[[ibId]], ibId),
              input
  ))
}
iFormSlider <- function(slider, Id) {
  return(div(class = "form-group",
         iLabel(etiquetas[[Id]], Id),
          slider,
         ))
}
fNumInput <- function(Id)
{
  return (div(class = "form-group",
              iLabel(etiquetas[[Id]], Id),
              numInput(Id)
  ))  
}

rateTable <- function(numTech, hTemporal, techName, techId, sufijo, ancho) {

  header <- fluidRow(
      column(3, div("Tecnologia", class = "rtHeader fCol")),
      column(9,fluidRow(
    lapply(1:hTemporal, function(i) {
      column(ancho, div(paste("Año", i), class = "rtHeader"))
    })))
  )
  filas <- lapply(1:numTech, function(i) {
      fluidRow(
        column(3, div(techName[i], class = "rtName")),
        column(9, fluidRow(
        lapply(1:hTemporal, function(j) {
          column(ancho, autonumericInput(
            paste0(techId[i], j, sufijo),
            label = NULL,
            value = NULL,
            class = "rtInput",
            align = "center",
            currencySymbol = "%", currencySymbolPlacement = "s", decimalCharacter = ",", digitGroupSeparator = ".", minimumValue = 0, maximumValue = 100, decimalPlaces = selected_decimales[[paste0(techId[i], j, sufijo)]]
          ))
        })))
      )
  })

  # Empaquetar todas las filas dentro de un contenedor
  return(do.call(div, c(list(class = "rtContainer"), list(header), filas)))
}

numInput <- function(iId)
{
  print(iId)
  return (
    switch(selected_opciones[[iId]],
           autonumericInput(iId, label = NULL, value = 0, align = "center", currencySymbol = "$", currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", decimalPlaces = selected_decimales[[iId]]),
           autonumericInput(iId, label = NULL, value = 0, align = "center", currencySymbol = "%", currencySymbolPlacement = "s", decimalCharacter = ",", digitGroupSeparator = ".", minimumValue = 0, maximumValue = 100, decimalPlaces = selected_decimales[[iId]]),
           autonumericInput(iId, label = NULL, value = 0, align = "center", decimalCharacter = ",", digitGroupSeparator = ".", decimalPlaces = selected_decimales[[iId]]),
           )
  )
}
panelResPrincipales <- tags$div(class = "panelRes",
                              div(class = "tablas-escenarios",
                                  div(class="tablaResultados",
                                      h4("Impacto", class = "tabla-titulo"),
                                      div(class="tablawrapper",
                                          tableOutput("tablaMain"))),
                                      div(class = "pieTabla",
                                      ),
                                      div(class = "pieTabla",
                                      ),
                              )
)
panelResResumidos <- tags$div(class = "panelRes",
                                div(class = "tablas-escenarios",
                                  div(class="tablaResultados",
                                  h4("Resultados Dalys", class = "tabla-titulo"),
                                  div(class="tablawrapper",
                                  tableOutput("tablaDalys"))),
                                  # div(class="tablaResultados",
                                  # h4("Escenario Proyectado", class = "tabla-titulo"),
                                  # div(class="tablawrapper",
                                  # tableOutput("tablaResProy"))),
                                  # div(class="tablaResultados",
                                  # h4("Diferencia", class = "tabla-titulo"),
                                  # div(class="tablawrapper",
                                  # tableOutput("tablaResDif"))),
                                  # div(class = "pieTabla",
                                  #     textOutput("pieTablaResumidos")
                                  # )
                                  )
                                  )
                                  
panelResCostos <- tags$div(class = "panelRes",
                                div(class = "tablas-escenarios",
                                    div(class="tablaResultados",
                                        h4("Resultados Costos", class = "tabla-titulo"),
                                        tableOutput("tablaCostos")),
                                    # div(class = "pieTabla",
                                    #     textOutput("pieTablaDetallados")
                                    # )
                                )
)
panelResSanitarios <- tags$div(class = "panelRes",
                               div(class = "tablas-escenarios",
                                   div(class="tablaResultados",
                                       h4("Resultados Sanitarios", class = "tabla-titulo"),
                                       tableOutput("tablaSanitaria")),
                               )
)

panelcitoOutput <- function(titulo, valor, color, icono) {
  return(tags$div(class = "panelcitoOutput", style = paste0("background-color: ", color, ";"),
  tags$div(class = "panelcitoTitulo", titulo),
  tags$div(class = "panelcitoFilaInferior",tags$i(class = icono), tags$div(class = "panelcitoValor", valor))
))
}


# marketShareFila <- function(i) {
#    if (i < 5) {
#    return (div(class = "fila-tasa",
#              div(id = paste0("tasaGroup", i), class = "tasa-input",
#               tags$label(paste("Año", i)),
#                div(class = "input-group-custom",
#                 autonumericInput(paste0("msTeplizumab", i), NULL, value = 0, min = 0, max = 100, step = 1, width = "70px", currencySymbol = "%", currencySymbolPlacement ="s", decimalCharacter = ",", digitGroupSeparator = ".", decimalPlaces = 1, align = "center")
#                )
#              ),
#              div(id = paste0("tasaGroup", i + 1), class = "tasa-input",
#                  tags$label(paste("Año", i + 1)),
#                  div(class = "input-group-custom",
#                   autonumericInput(paste0("msTeplizumab", i + 1), NULL, value = 0, min = 0, max = 100, step = 1, width = "70px", currencySymbol = "%", currencySymbolPlacement ="s", decimalCharacter = ",", digitGroupSeparator = "." , decimalPlaces = 1, align = "center")
#                  )
#              )
#           )
#        )
#    } else {
#      return (div(class = "fila-tasa",
#                div(id = paste0("tasaGroup", i), class = "tasa-input",
#                    tags$label(paste("Año", i)),
#                    div(class = "input-group-custom",
#                      autonumericInput(paste0("msTeplizumab", i), NULL, value = 0, min = 0, max = 100, step = 1, width = "70px", currencySymbol = "%", currencySymbolPlacement ="s", decimalCharacter = ",", digitGroupSeparator = "." , decimalPlaces = 1, align = "center")
#                    )
#                 ),
#               )
#             )
#    }
#  }


panelVisualizador <- div(id = "visualizadorPanel",
                         actionButton("toggleParamBarTop", label = NULL, icon = tags$i(class = "fa fa-chevron-down"), class = "menu-ocultar-top"),
                         div(class = "colInputs visCollapsed", id = "colInputs",
                             # Botón para mostrar/ocultar el sidebar
                             tags$div(
                               class = "input-bar",
                               actionButton("toggleParamBar", label = NULL, icon = tags$i(class = "fas fa-chevron-left"), class = "menu-ocultar"),
                               tags$h4("Parámetros", class = "input-title")
                             ),
                             tags$div(id = "input-content",
                               selectInput(
                                 inputId = "perspectiva",
                                 label = "Selecciona un Sector:",
                                 choices = sectores,
                                 selected = "PAMI"
                               ),
                                fNumInput("nAfiliados"),
                                div(style = "display:flex; gap:0px;",
                                prettyCheckbox("bInflacion", "Ajustar costos por Inflación", shape = "square", status = "primary", outline = TRUE, icon = icon("check", class ="chkIcon"), value = TRUE),
                                iButton("inflacion") 
                                  
                                )

                            ),
                           ),
                           div(id = "colRes",
                             class = "colRes",
                             h3("Diferencias entre el escenario actual y el proyectado"),
                             div(class = "resTabCarteles",
                                 panelcitoOutput("Tratamientos Exitosos", textOutput("deltaExitosos"), COLOR_PRIMARIO, "fas fa-syringe"),
                                 panelcitoOutput("Muertes", textOutput("deltaMuertes"), COLOR_PRIMARIO, "fas fa-dollar-sign"),
                                 panelcitoOutput("LTFU", textOutput("deltaLTFU"), COLOR_PRIMARIO, "fas fa-procedures"),
                                 panelcitoOutput("Ly Perdidos", textOutput("deltaLyLost"), COLOR_PRIMARIO, "fas fa-dollar-sign"),
                                 panelcitoOutput("Costos de Testeo", textOutput("deltaCostoTesteo"), COLOR_PRIMARIO, "fas fa-dollar-sign"),
                                 panelcitoOutput("Otros Costos", textOutput("deltaOtrosCostos"),COLOR_PRIMARIO, "fas fa-dollar-sign"),
                                 ),
                             div(id = "resTabDiv",
                              navset_card_underline(
                                 id = "resTab",
                                 title = "Resultados",
                                 nav_panel("Principales", panelResPrincipales),
                                 #nav_panel("Dalys", panelResResumidos),
                                 nav_panel("Costos", panelResCostos),
                                 #nav_panel("Sanitarios", panelResSanitarios),
                                 footer = card_body("IP PMPM = Impacto Presupuestario Por Miembro Por Mes, IPC = Indice de Precios al Consumidor", class="tabfooter"),
                                 full_screen = TRUE
                               )
                             ),
                          )
)




panelIntroduccion <- div(id ="introduccionPanel", 
                         div(class = "introPanel",
                             includeMarkdown("www/introduccion.rmd"),
                             actionButton("cmdEmpezar", "Empezar", class = "cmdPrimary")
                         )
                         )

panelConfiguracion <- div(id = "configuracionPanel",
                              div(class = "card-wrapper",
                                  div(class = "card-wrapper-header",
                                        h3("Parámetros Epidemiológicos"),
                                        actionButton("toggle_epidemiologicos", "−", class = "card-wrapper-min")
                                      ),
                                  div(id = "card-epidemiologicos", class = "card-wrapper-body",
                                      fluidRow(
                                                column(4,
                                                       #  fNumInput("uTBCNoTratada"),
                                                       #  fNumInput("uTBCTratada"),
                                                       # fNumInput("uTBCNoTratadaMDR"),
                                                       # fNumInput("uTBCTratadaMDR"),
                                                       
                                                       ),
                                                column(4,
                                                       #  fNumInput("pTtoExitoso"),
                                                       # fNumInput("pMuerte"),
                                                       # fNumInput("pTtoExitosoMDR"),
                                                       # fNumInput("pMuerteMDR")

                                                       ),
                                                column(4,
                                                        # fNumInput("pNAATSensibilidad"),
                                                        # fNumInput("pNAATEspecificidad"),
                                                        # fNumInput("pBCPSensibilidad"),
                                                        # fNumInput("pBCPEspecificidad")
                                                       )
                                              )
                                      )
                                  ),
                              div(class = "card-wrapper",
                                  div(class = "card-wrapper-header", 
                                      h3("Parámetros Económicos"),
                                      actionButton("toggle_economicos", "−", class = "card-wrapper-min")
                                  ),
                                  div(id = "card-economicos", class = "card-wrapper-body",
                                  fluidRow(
                                            column(4, 
                                                    # fNumInput("cBCP"),
                                                    # fNumInput("cNAAT"),
                                                   ),
                                          
                                            column(4, 
                                                    # fNumInput("cCultivo"),
                                                    # fNumInput("cAntibiograma")
                                                   ),
                                            column(4, 
                                                    # fNumInput("cTratamiento"),
                                                    # fNumInput("cTratamientoMDR"),
                                                   )
                                          ),
                                  )
                              ),
                          )


panelEscenarios <- div(id = "escenariosPanel",
                          h3("Escenarios")
)
panelReporte <- div(id = "reportesPanel",
                       h3("Reportes")
)

panelAPP <- div(id = "appPanel",
              div(class = "app-container",
                # Botón para mostrar/ocultar el sidebar
                tags$div(
                  class = "top-bar",
                  tags$div(class = "top-bar-component",
                    actionButton("toggleSidebar", label = NULL, icon = tags$i(class = "fa icon-bars"), class = "menu-btn"),
                    tags$h3("Avelumab", class = "title")
                  ),
                  tags$div(class = "top-bar-component",
                    tags$img(src = "imagenes/LOGO_IECS.png", height = "50px")
                  )
                ),
                tags$div(
                  class="top-bar-shadow-blocker",
                ),
                # Contenedor general
                div(class = "main-wrapper",
                  div(id = "sidebar", class = "sidebar collapsed",
                    actionLink("navIntroduccion", label = tagList(tags$i(class = "fa fa-book-open"), span("Introduccion")), class = "sidebar-link active"),
                    actionLink("navVisualizador", label = tagList(tags$i(class = "fa fa-chart-bar"), span("Visualizador")), class = "sidebar-link"),
                    actionLink("navConfiguracion", label = tagList(tags$i(class = "fas fa-tools"), span("Configuración")), class = "sidebar-link")
                    #actionLink("navEscenarios", label = tagList(tags$i(class = "fas icon-save"), span("Escenarios")), class = "sidebar-link"),
                    #actionLink("navReporte", label = tagList(tags$i(class = "fa fa-file-alt"), span("Reporte")), class = "sidebar-link")
                  ),
                  tabsetPanel(id = "content",
                    tabPanel("Introduccion", value = "Introduccion", panelIntroduccion),                         
                    tabPanel("Visualizador", value = "Visualizador", panelVisualizador),
                    tabPanel("Configuracion", value = "Configuracion", panelConfiguracion)
                    #tabPanel("Escenarios", value = "Escenarios", panelEscenarios),
                    #tabPanel("Reporte", value = "Reporte", panelReporte),
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
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),  # Cargar Font Awesome desde CDN
    tags$script(src = paste0("script.js?v=", as.integer(Sys.time()))),
  ),
  useShinyjs(),
  #(mod_login_ui(LOGIN_MODULO)),
  #hidden(panelAPP),
  hidden(mod_login_ui(LOGIN_MODULO)),
  (panelAPP)
  
  )
