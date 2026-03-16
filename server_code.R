source("login.R")
source("Avelumab.R")
PERSPECTIVA_SELECTA <- "PAMI"
cargarDatos()
selected_opciones <<- mutables_opciones[[PERSPECTIVA_SELECTA]]
# (selected_opciones)
selected_inflacion <<- mutables_inflacion[[PERSPECTIVA_SELECTA]]
selected_decimales <<- mutables_decimales[[PERSPECTIVA_SELECTA]]

cargarEtiquetasTooltips()

server <- function(input, output, session) {
  ############################################################ CODIGO DE LOGIN ##############################################################################
  # funcion reactiva
  ajustaInflacion <- reactiveVal(TRUE)
  app_visible <- reactiveVal(FALSE) # Cambiar esto a false
  # app_visible <- reactiveVal(TRUE)
  showingWaiter <- reactiveVal(TRUE)
  primerCorrida <- TRUE
  # Modulos
  user_logged <- mod_login_server(LOGIN_MODULO, APLICACION_ID, app_visible)

  session$sendCustomMessage("inicializar-tooltips", tooltip_list)
  # Funciones encapsuladas que manejan UI general


  informacionInflacion <- obtenerInflacion(FECHA_COSTOS)

  if (!is.null(informacionInflacion)) {
    modificadorInflacion <- informacionInflacion[[1]]
    fechaInflacion <- informacionInflacion[[2]]
  } else {
    modificadorInflacion <- 1
  }
  sMutables <- list()

  for (c in sectores) {
    sMutables[[c]] <- ajustarDatos(mutables[[c]], modificadorInflacion, mutables_inflacion[[c]])
  }

  user_logged <- reactiveVal(TRUE)
  observeEvent(input$"toggle_tasa", {
    toggleClass("card-tasa", "collapsed") # Alterna clase collapsed
    # Cambia el texto del botón
    current <- input$toggle_tasa %% 2
    updateActionButton(session, "toggle_tasa", label = ifelse(current == 1, "+", "−"))
  })
  observeEvent(input$"toggle_subsecuentes", {
    toggleClass("card-subsecuentes", "collapsed") # Alterna clase collapsed
    # Cambia el texto del botón
    current <- input$toggle_subsecuentes %% 2
    updateActionButton(session, "toggle_subsecuentes", label = ifelse(current == 1, "+", "−"))
  })
  observeEvent(input$"toggle_ea", {
    toggleClass("card-ea", "collapsed") # Alterna clase collapsed
    # Cambia el texto del botón
    current <- input$toggle_ea %% 2
    updateActionButton(session, "toggle_ea", label = ifelse(current == 1, "+", "−"))
  })
  observeEvent(input$"toggle_costos", {
    toggleClass("card-costos", "collapsed") # Alterna clase collapsed
    # Cambia el texto del botón
    current <- input$toggle_costos %% 2
    updateActionButton(session, "toggle_costos", label = ifelse(current == 1, "+", "−"))
  })
  observeEvent(input$"toggle_tasas", {
    toggleClass("card-tasas", "collapsed") # Alterna clase collapsed
    # Cambia el texto del botón
    current <- input$toggle_tasas %% 2
    updateActionButton(session, "toggle_tasas", label = ifelse(current == 1, "+", "−"))
  })
  loginObservers <- function() {
    observe({
      if (app_visible()) {
        shinyjs::show("appPanel")
        runjs("ajustarTopBar();")
      } else {
        shinyjs::hide("appPanel")
      }
    })

    observeEvent(input$cmdLogout, {
      user_logged(FALSE)
      app_visible(FALSE)
      shinyjs::runjs(sprintf('$("#%s").show();', paste(LOGIN_MODULO, "-loginPanel", sep = "")))
    })
  }

  loginObservers()
  ############################################################ CODIGO DE LOGIN ##############################################################################
  validar_inputs <- function() {
    return(TRUE)
  }
  switch_tab <- function(tab_id, link_id) {
    if (active_tab() == "navConfiguracion") {
      if (!validar_inputs()) {
        return()
      }
    }

    updateTabsetPanel(session, "content", selected = tab_id)

    # Eliminar la clase activa anterior
    prev_id <- active_tab()


    if (!is.null(prev_id)) {
      shinyjs::removeClass(selector = paste0("#", prev_id), class = "active")
    }

    # Agregar clase activa al nuevo
    shinyjs::addClass(selector = paste0("#", link_id), class = "active")
    active_tab(link_id) # Guardar como actual
  }

  active_tab <- reactiveVal("navIntroduccion")

  observe({
    if (user_logged()) {
      req(cargoParametros)
      # Aquí puedes hacer algo más, como mostrar contenido exclusivo para usuarios logueados
      # shinyjs::hide("login1-loginPanel")
      waiter_show(
        html = div(style = "font-size: 50px;", class = "spinner"),
        color = COLOR_PRIMARIO
      )
      isolate(resultado_modelo())
    }
  })

  observeEvent(input$cmdEmpezar, {
    switch_tab("Visualizador", "navVisualizador")
  })
  observeEvent(input$navIntroduccion, {
    switch_tab("Introduccion", "navIntroduccion")
  })
  observeEvent(input$navVisualizador, {
    switch_tab("Visualizador", "navVisualizador")
  })
  observeEvent(input$navConfiguracion, {
    switch_tab("Configuracion", "navConfiguracion")
  })
  observeEvent(input$navEscenarios, {
    switch_tab("Escenarios", "navEscenarios")
  })
  observeEvent(input$navReporte, {
    switch_tab("Reporte", "navReporte")
  })


  params <- reactiveValues()
  #  params_inmut <- NULL
  cargoParametros <<- NULL
  flags_actualizando <- reactiveValues()
  # Inicializo todos en FALSE
  for (nombre in names(mutables[[sectores[[1]]]])) {
    flags_actualizando[[nombre]] <- FALSE
  }


  # Corre modelo
  # debounce: espera 1,5 s sin cambios antes de invalidar resultado_modelo
  params_debounced <- debounce(reactive(reactiveValuesToList(params)), 1500)

  resultado_modelo <- reactive({
    params_debounced()  # dependencia con delay
    req(isolate(active_tab() != "navConfiguracion"))
    req(cargoParametros)
    if (user_logged()) {
      if (primerCorrida == FALSE) {
        waiter_show(
          html = div(style = paste0("color: ", COLOR_PRIMARIO, "; font-size: 50px;", class = "spinner")),
          color = "rgba(255, 255, 255, 0.2)" # fondo casi transparente
        )
        showingWaiter(TRUE)
      }
      res <- correrModelo(isolate(reactiveValuesToList(params)))
      showingWaiter(FALSE)
      return(res)
    } else {
      return(NULL)
    }
  })

  # Actualizamos el Parametro al modificar un input
  for (nombre in names(mutables[[sectores[[1]]]])) {
    local({
      nombre_local <- nombre
      observeEvent(input[[nombre_local]],
        {
          if (isTRUE(flags_actualizando[[nombre_local]])) {
            flags_actualizando[[nombre_local]] <- FALSE
            return()
          }
          valor <- switch(selected_opciones[[nombre_local]],
            input[[nombre_local]],
            input[[nombre_local]] * 0.01,
            input[[nombre_local]],
            as.numeric(input[[nombre_local]]),
            input[[nombre_local]]
          )
          if (!is.null(valor)) {
            params[[nombre_local]] <- valor
          }
          checkRemanente("BSC", nombre_local, c("msAVEE"), input)
          checkRemanente("QMT", nombre_local, c("msEVPE", "msNIVE"), input)
        },
        ignoreInit = TRUE
      )
    })
  }

  checkRemanente <- function(id, nombre, tecnologias, parametros, multiplicador = 1) {
    grupos <- unlist(lapply(tecnologias, function(i) {
      c(paste0(i, "B", 1:5), paste0(i, "P", 1:5))
    }))
    if (nombre %in% grupos) {
      sufijo <- substr(nombre, nchar(nombre) - 1, nchar(nombre))
      suma <- sum(sapply(paste0(tecnologias, sufijo), function(x) parametros[[x]]))
      rem <- max(0, 100 - suma * multiplicador)
      shinyjs::html(
        id = paste0(id, sufijo),
        html = paste0(formatC(rem, format = "f", digits = 2, decimal.mark = ","), "%")
      )
    }
  }


  inflacionarParametros <- function() {
    if (ajustaInflacion() == TRUE) {
      selected_modif <- sMutables[[input$perspectiva]]
    } else {
      selected_modif <- mutables[[input$perspectiva]]
    }

    # Cargar modificables a reactiveValues
    for (name in names(selected_modif)) {
      if (selected_inflacion[[name]] == 1) {
        params[[name]] <- selected_modif[[name]]
      }
    }
    for (nombre in names(params)) {
      if (nombre %in% names(input) && nombre != "perspectiva") {
        if (selected_inflacion[[nombre]] == 1) {
          flags_actualizando[[nombre]] <- TRUE
          if (selected_opciones[[nombre]] <= 3) {
            updateNumericInput(session, inputId = nombre, value = switch(selected_opciones[[nombre]],
              params[[nombre]],
              params[[nombre]] * 100,
              params[[nombre]]
            ))
          } else if (selected_opciones[[nombre]] == 4) {
            updatePrettyCheckbox(session, inputId = nombre, value = params[[nombre]])
          } else if (selected_opciones[[nombre]] == 5) {
            updateSliderInput(session, inputId = nombre, value = params[[nombre]])
          }
        }
      } else {
        # if (substr(nombre, 1, nchar(nombre) - 1) == "msTeplizumab")
        # {
        #  print(paste("Deberia haber asignado TRUE a", nombre))
        #  flags_actualizando[[nombre]] <- TRUE
        #  updateNumericInput(session, inputId = paste0("t", nombre), value = params[[nombre]] * 100)
        #
        # }
      }
    }
    cargoParametros <<- TRUE
  }
  actualizarParametros <- function() {
    if (ajustaInflacion() == TRUE) {
      selected_modif <- sMutables[[input$perspectiva]]
    } else {
      selected_modif <- mutables[[input$perspectiva]]
    }
    selected_opciones <<- mutables_opciones[[input$perspectiva]]
    selected_inflacion <<- mutables_inflacion[[input$perspectiva]]
    selected_decimales <<- mutables_decimales[[input$perspectiva]]
    # Cargar modificables a reactiveValues
    for (name in names(selected_modif)) {
      params[[name]] <- selected_modif[[name]]
    }
    for (nombre in names(params)) {
      if (nombre %in% names(input) && nombre != "perspectiva") {
        flags_actualizando[[nombre]] <- TRUE
        if (selected_opciones[[nombre]] <= 3) {
          updateNumericInput(session, inputId = nombre, value = switch(selected_opciones[[nombre]],
            params[[nombre]],
            params[[nombre]] * 100,
            params[[nombre]]
          ))
        } else if (selected_opciones[[nombre]] == 4) {
          updatePrettyCheckbox(session, inputId = nombre, value = params[[nombre]])
        } else if (selected_opciones[[nombre]] == 5) {
          updateSliderInput(session, inputId = nombre, value = params[[nombre]])
        }
      } else {
        # if (substr(nombre, 1, nchar(nombre) - 1) == "msTeplizumab")
        # {
        #  print(paste("Deberia haber asignado TRUE a", nombre))
        #  flags_actualizando[[nombre]] <- TRUE
        #  updateNumericInput(session, inputId = paste0("t", nombre), value = params[[nombre]] * 100)
        #
        # }
      }
      checkRemanente("BSC", nombre, c("msAVEE"), params, multiplicador = 100)
      checkRemanente("QMT", nombre, c("msEVPE", "msNIVE"), params, multiplicador = 100)
    }

    cargoParametros <<- TRUE
  }
  # Cambia la perspectiva actualiza parametros
  observeEvent(input$perspectiva, {
    req(input$perspectiva)
    actualizarParametros()
  })
  observeEvent(input$bInflacion, {
    ajustaInflacion(input$bInflacion)
    inflacionarParametros()
    if (modificadorInflacion != 1 && ajustaInflacion() == TRUE) {
      pieTabla(gsub("%1", fechaInflacion, PIE_DE_TABLA1))
    } else {
      pieTabla(PIE_DE_TABLA2)
    }
  })
  if (modificadorInflacion != 1) {
    pieTabla <- reactiveVal(gsub("%1", fechaInflacion, PIE_DE_TABLA1))
  } else {
    pieTabla <- reactiveVal(PIE_DE_TABLA2)
  }
  output$pieTablaPrincipales <- renderText({
    pieTabla()
  })
  output$pieTablaResumidos <- renderText({
    pieTabla()
  })
  output$pieTablaDetallados <- renderText({
    pieTabla()
  })


  # INDICADOREs-----------------------------------------
  output$deltaIndicador1 <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaIndicador1
  })
  output$deltaIndicador2 <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaIndicador2
  })
  output$deltaIndicador3 <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaIndicador3
  })
  output$deltaIndicador4 <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaIndicador4
  })
  output$deltaIndicador5 <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaIndicador5
  })
  # output$deltaIndicador6 <- renderText({
  #   req(resultado_modelo())
  #   resultado_modelo()$indicadores$deltaIndicador6
  # })
  output$grhCOC <- renderPlot({
    req(resultado_modelo())

    df <- resultado_modelo()$tablaCocRaw

    df_long <- df |>
      pivot_longer(
        cols = -Categorias,
        names_to = "intervencion",
        values_to = "valor"
      ) |>
      dplyr::filter(Categorias != "Total") |>
      dplyr::group_by(intervencion) |>
      dplyr::mutate(porcentaje = valor / sum(valor)) |>
      dplyr::ungroup() |>
      dplyr::mutate(intervencion = factor(intervencion, levels = c(
        "Quimioterapia + Avelumab",
        "Cisplatino + Nivolumab",
        "Enfortumab Vedotin + Pembrolizumab",
        "Quimioterapia"
      )))

    df_long$Categorias <- factor(
      df_long$Categorias,
      levels = rev(unique(df_long$Categorias))
    )

    ggplot(
      df_long,
      aes(
        x = intervencion,
        y = porcentaje,
        fill = Categorias
      )
    ) +
      geom_col(width = 0.7, color = "white") +
      geom_label(
        data = dplyr::filter(df_long, porcentaje >= 0.10),
        aes(label = scales::percent(porcentaje, accuracy = 0.1)),
        position = position_stack(vjust = 0.5),
        color = "white",
        fill = NA,
        label.size = 0,
        fontface = "bold",
        size = 6
      ) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
      scale_fill_manual(values = c(
        "Costos de Adquisición" = "#004B87",
        "Costos de Administración" = "#307BB2",
        "Costos de Efectos Adversos" = "#60ABDD",
        "Costos de Manejo de la Enfermedad y Monitoreo" = "#FF8C00",
        "Costos de Adquisición y Administración de Tratamientos Subsecuentes" = "#FFB347",
        "Costos de Monitoreo de tratamientos Subsecuentes" = "#FFD580",
        "Total" = "#444444"
      )) +
      labs(
        x = "Intervención",
        y = "Porcentaje del costo total",
        fill = "Componentes de Costos"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold", color = "black"),
        axis.text.y = element_text(size = 14, face = "bold", color = "black")
      )
  })
  output$grhCOCTotal <- renderPlot({
    req(resultado_modelo())

    df <- resultado_modelo()$tablaCocRaw

    df_long <- df |>
      pivot_longer(
        cols = -Categorias,
        names_to = "intervencion",
        values_to = "valor"
      )

    componente <- "Total"
    if (!is.null(input$selectorComponenteCosto) && input$selectorComponenteCosto != "") {
      componente <- input$selectorComponenteCosto
    }

    df_filtrado <- df_long |>
      dplyr::filter(Categorias == componente) |>
      dplyr::mutate(intervencion = factor(intervencion, levels = c(
        "Quimioterapia + Avelumab",
        "Cisplatino + Nivolumab",
        "Enfortumab Vedotin + Pembrolizumab",
        "Quimioterapia"
      )))

    ggplot(
      df_filtrado,
      aes(
        x = intervencion,
        y = valor,
        fill = intervencion
      )
    ) +
      geom_col(width = 0.7, color = "white") +
      geom_label(
        aes(y = valor / 2, label = paste0("$", format(valor / 1e6, big.mark = ".", decimal.mark = ",", digits = 1, nsmall = 1), " M")),
        color = "white",
        fill = "black",
        label.size = NA,
        alpha = 0.7,
        fontface = "bold",
        size = 5
      ) +
      scale_y_continuous(
        labels = function(x) paste0("$", format(x / 1e6, big.mark = ".", decimal.mark = ","), " M"),
        breaks = scales::pretty_breaks(n = 10)
      ) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
      scale_fill_manual(values = c(
        "Quimioterapia + Avelumab" = "#f5b85e",
        "Enfortumab Vedotin + Pembrolizumab" = "#014EA3",
        "Cisplatino + Nivolumab" = "#396ba0",
        "Quimioterapia" = "#5c7b9c"
      )) +
      labs(
        x = "Intervención",
        y = "Costo de Cuidado (Millones de pesos)",
        fill = "Intervención"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 14, face = "bold", color = "black"),
        axis.text.y = element_text(size = 14, face = "bold", color = "black"),
        panel.grid.major.y = element_line(color = "grey85"),
        panel.grid.minor.y = element_line(color = "grey95")
      )
  })
  # #GRAFICO PMPM-----------------------------------------
  # output$grhPMPM <- renderPlot({
  #   req(resultado_modelo())
  #   width <- session$clientData$output_grhPMPM_width
  #   req(width)
  #
  #   texto_angle <- if (width < 576) 45 else 0
  #   texto_size  <- if (width < 576) 10 else 13
  #
  #   valores <- resultado_modelo()$graficos$PMPM[c(1:(resultado_modelo()$indicadores$horizonteTemporal + 1))]
  #
  #   etiquetas <- c(paste0("Año ", 1:resultado_modelo()$indicadores$horizonteTemporal), "Promedio")
  #   df <- data.frame(
  #     anio  = factor(etiquetas, levels = etiquetas),
  #     valor = valores,
  #     grupo = ifelse(etiquetas == "Promedio", "promedio", "anual")
  #   )
  #
  #   min_valor <- min(df$valor) * 1.25
  #   max_valor <- max(df$valor) * 1.25
  #
  #   ggplot(df, aes(x = anio, y = valor, fill = grupo)) +
  #     geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  #     geom_hline(aes(yintercept = 13.96, color = "Umbral de Alto Impacto Presupuestario ($13,9)"), linetype = "dashed", linewidth = 0.8) +
  #     geom_hline(aes(yintercept = 27.91, color = "Umbral de Muy Alto Impacto Presupuestario ($27,9)"), linetype = "dashed", linewidth = 0.8) +
  #     geom_col(width = 0.6) +
  #     geom_text(
  #       aes(
  #         label = paste0("$", formatC(valor, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")),
  #         vjust = ifelse(valor < 0, 1.2, -0.7)  # etiquetas arriba o abajo según signo
  #       ),
  #       size = 5.2, color = "black", fontface = "bold"
  #     ) +
  #     scale_color_manual(
  #       name = "Líneas de referencia",
  #       values = c("Umbral de Alto Impacto Presupuestario ($13,9)" = "orange", "Umbral de Muy Alto Impacto Presupuestario ($27,9)" = "red")
  #     ) +
  #     scale_fill_manual(values = c("anual" = COLOR_GRAFICO1, "promedio" = COLOR_GRAFICO2), guide = "none") +
  #     scale_y_continuous(
  #       limits = c(min(0, min_valor), max(0, max_valor, 28)),
  #       labels = scales::label_dollar(
  #         accuracy = 0.01,
  #         prefix = "$",
  #         big.mark = ".",
  #         decimal.mark = ","
  #       ),
  #       expand = expansion(mult = c(0.05, 0.1))  # 5% abajo, 10% arriba
  #     ) +
  #     labs(
  #       title = "Impacto Presupuestario PMPM",
  #       x = NULL,
  #       y = "IP PMPM"
  #     ) +
  #     theme_minimal(base_size = 14) +
  #     theme(
  #       plot.title = element_text(hjust = 0.5, face = "bold"),
  #       axis.text.y = element_text(color = "gray20"),
  #       axis.text.x = element_text(color = "black", size = texto_size, face = "bold",
  #                                  hjust = if (texto_angle > 0) 1 else 0.5, angle = texto_angle),
  #       axis.title.y = element_text(margin = margin(r = 10)),
  #       panel.grid.major.x = element_blank(),
  #       legend.position = "top"
  #     )
  # })  #GRAFICO PMPM-----------------------------------------
  #
  # #TABLAS PMPM-----------------------------------------
  output$tablaCostos <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostos
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$tablaCostosProy <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosProy
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$tablaCostosDiff <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosDiff
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )

  output$tablaCostosR <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosR
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$tablaCostosProyR <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosProyR
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$tablaCostosDiffR <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosDiffR
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )

  output$grhCostosR <- renderPlot({
    req(resultado_modelo())

    basal <- resultado_modelo()$tablaCostosRRaw |>
      dplyr::filter(Categorias != "Total") |>
      tidyr::pivot_longer(-Categorias, names_to = "Año", values_to = "Valor") |>
      dplyr::mutate(Escenario = "Actual")

    proy <- resultado_modelo()$tablaCostosProyRRaw |>
      dplyr::filter(Categorias != "Total") |>
      tidyr::pivot_longer(-Categorias, names_to = "Año", values_to = "Valor") |>
      dplyr::mutate(Escenario = "Proyectado")

    df <- dplyr::bind_rows(basal, proy) |>
      dplyr::filter(Año != "Promedio") |>
      dplyr::mutate(
        Escenario = factor(Escenario, levels = c("Proyectado", "Actual")),
        Año = factor(Año, levels = sort(unique(Año)))
      )

    # Orden categorías desc por valor promedio
    orden_cat <- df |>
      dplyr::group_by(Categorias) |>
      dplyr::summarise(media = mean(Valor), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(media)) |>
      dplyr::pull(Categorias)

    # En barras horizontales ggplot apila: último nivel = izquierda.
    # Para mayor→menor de izq a der: nivel 1 = menor, último = mayor → rev(orden_cat)
    n <- length(orden_cat)
    # Azules IECS para Actual, naranjas IECS para Proyectado
    blues   <- colorRampPalette(c("#BBDEFB", "#014EA3"))(n)
    oranges <- colorRampPalette(c("#FFD166", "#f5b85e", "#E55812"))(n)
    names(blues)   <- paste0(rev(orden_cat), "_Actual")
    names(oranges) <- paste0(rev(orden_cat), "_Proyectado")
    color_map <- c(blues, oranges)

    df$fill_var <- factor(
      paste0(df$Categorias, "_", df$Escenario),
      levels = c(paste0(rev(orden_cat), "_Actual"), paste0(rev(orden_cat), "_Proyectado"))
    )

    # Etiquetas legibles para la leyenda (solo nombre del campo, sin escenario)
    abrev <- function(x) {
      x <- sub("Costos de ", "", x)
      x <- sub("Manejo de la Enfermedad y Monitoreo", "Manejo y Monitoreo", x)
      x <- sub("tratamientos Subsecuentes", "Subsecuentes", x)
      x
    }
    legend_labels <- setNames(
      c(abrev(rev(orden_cat)), abrev(rev(orden_cat))),
      c(paste0(rev(orden_cat), "_Actual"), paste0(rev(orden_cat), "_Proyectado"))
    )

    # Totales por año + escenario
    totales <- df |>
      dplyr::group_by(Año, Escenario) |>
      dplyr::summarise(Total = sum(Valor), .groups = "drop")

    ggplot(df, aes(y = Escenario, x = Valor, fill = fill_var)) +
      geom_col(width = 0.65, color = "white") +
      geom_text(
        data = totales,
        aes(y = Escenario, x = Total, label = paste0("$", format(round(Total / 1e6, 1), big.mark = ".", decimal.mark = ","), " M")),
        inherit.aes = FALSE,
        hjust = -0.1,
        fontface = "bold",
        size = 4,
        color = "grey20"
      ) +
      facet_grid(Año ~ ., switch = "y", scales = "free_y", space = "free") +
      scale_x_continuous(
        labels = function(x) paste0("$", format(x / 1e6, big.mark = ".", decimal.mark = ","), " M"),
        breaks = scales::pretty_breaks(n = 6),
        expand = expansion(mult = c(0, 0.18))
      ) +
      scale_fill_manual(
        values = color_map,
        labels = legend_labels,
        guide  = guide_legend(nrow = 2, byrow = TRUE)
      ) +
      labs(
        x    = "Costo Total (Millones de pesos)",
        y    = NULL,
        fill = "Componente"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 13),
        legend.text  = element_text(size = 11),
        strip.text.y.left = element_text(angle = 0, face = "bold", size = 13, color = "black"),
        strip.placement = "outside",
        panel.spacing = unit(1, "lines"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        panel.grid.major.x = element_line(color = "grey85"),
        panel.grid.minor.x = element_line(color = "grey95"),
        panel.grid.major.y = element_blank()
      )
  })

  output$tablaCostosEVP <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosEVP
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$tablaCostosEVPDiff <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosEVPDiff
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$tablaCostosNIV <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosNIV
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$tablaCoc <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCoc
    },
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )

  output$tablaCostosNIVDiff <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaCostosNIVDiff
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$tablaMain <- renderTable(
    {
      req(resultado_modelo())
      resultado_modelo()$tablaMain
    },
    ,
    striped = FALSE,
    bordered = FALSE,
    hover = FALSE,
    spacing = "s",
    class = "tablaResultados"
  )
  output$ICERINFO <- renderText({
    resultado_modelo()$infoICER
  })
  output$ROIINFO <- renderText({
    resultado_modelo()$infoROI
  })

  output$grhPMPMComparativo <- renderPlot({
    req(resultado_modelo())
    req(!is.null(resultado_modelo()$tablaCostosNIV))

    df <- resultado_modelo()$tablaPMPM

    orden_años <- c(
      setdiff(unique(df$Año), "Promedio"),
      "Promedio"
    )
    df$Año <- factor(df$Año, levels = orden_años)
    df$Intervencion <- factor(df$Intervencion, levels = c("Avelumab", "Cisplatino + Nivolumab", "Enfortumab Vedotin + Pembrolizumab"))

    ancho_plot <- session$clientData$output_grhPMPMComparativo_width

    p <- ggplot(df, aes(x = Año, y = Valor, fill = Intervencion)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "white")

    if (is.null(ancho_plot) || ancho_plot >= 500) {
      p <- p + geom_label(
        aes(y = Valor / 2, group = Intervencion, label = paste0("$", format(round(Valor, 0), big.mark = ".", decimal.mark = ","))),
        position = position_dodge(width = 0.75),
        vjust = 0.5,
        hjust = 0.5,
        color = "white",
        fill = "black",
        label.size = 0,
        alpha = 0.45,
        fontface = "bold",
        size = 5
      )
    }

    p +
      geom_hline(
        data = data.frame(
          yintercept = c(7.38, 14.75, 29.52),
          Umbral = factor(
            c("Moderado Impacto Presupuestario",
              "Alto Impacto Presupuestario",
              "Muy Alto Impacto Presupuestario"),
            levels = c("Moderado Impacto Presupuestario",
                       "Alto Impacto Presupuestario",
                       "Muy Alto Impacto Presupuestario")
          )
        ),
        aes(yintercept = yintercept, color = Umbral),
        linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE
      ) +
      annotate("text", x = Inf, y =  7.38, label = "Moderado",  hjust = 1.05, vjust = -0.5, color = "#2e9e44", size = 3.8, fontface = "bold") +
      annotate("text", x = Inf, y = 14.75, label = "Alto",       hjust = 1.05, vjust = -0.5, color = "#e6a817", size = 3.8, fontface = "bold") +
      annotate("text", x = Inf, y = 29.52, label = "Muy Alto",   hjust = 1.05, vjust = -0.5, color = "#d32f2f", size = 3.8, fontface = "bold") +
      scale_color_manual(
        values = c(
          "Moderado Impacto Presupuestario" = "#2e9e44",
          "Alto Impacto Presupuestario"     = "#e6a817",
          "Muy Alto Impacto Presupuestario" = "#d32f2f"
        ),
        name = "Umbrales de Impacto Presupuestario",
        guide = guide_legend(override.aes = list(linewidth = 1.2), order = 2)
      ) +
      scale_y_continuous(
        labels = function(x) paste0("$", format(x, big.mark = ".", decimal.mark = ",")),
        breaks = scales::pretty_breaks(n = 6),
        expand = expansion(mult = c(0, 0.08))
      ) +
      scale_fill_manual(
        values = c(
          "Avelumab"                         = "#f5b85e",
          "Cisplatino + Nivolumab"           = "#396ba0",
          "Enfortumab Vedotin + Pembrolizumab" = "#014EA3"
        ),
        guide = guide_legend(order = 1)
      ) +
      labs(
        x = NULL,
        y = "Impacto Presupuestario PMPM (por miembro, por mes)",
        fill = "Intervención",
        color = NULL,
        caption = "El gráfico refleja el impacto presupuestario por miembro por mes (PMPM) de incorporar Avelumab, en comparación con escenarios hipotéticos en los que la misma cantidad de pacientes fuera tratada con Enfortumab Vedotin + Pembrolizumab o con Cisplatino + Nivolumab."
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.box = "vertical",
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 13),
        legend.key.size = unit(1.2, "cm"),
        axis.text.x = element_text(size = 13, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid.major.y = element_line(color = "grey85"),
        panel.grid.minor.y = element_line(color = "grey95"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(size = 11, color = "grey40", hjust = 0, margin = margin(t = 12))
      )
  })
  output$panelAlternativo <- renderUI({
    req(resultado_modelo())
    if (is.null(resultado_modelo()$tablaCostosNIV)) {
      tags$p("No se estimaron escenarios alternativos debido a que no existe un incremento de pacientes con Avelumab.")
    } else {
      tags$div(
        class = "panelRes",
        h3("Enfortumab Vedotin + Pembrolizumab", class = "tabla-titulo"),
        div(
          class = "tablas-escenarios",
          div(
            class = "tablaResultados",
            h4("Escenario Proyectado", class = "tabla-titulo"),
            tableOutput("tablaCostosEVP")
          )
        ),
        div(
          class = "tablas-escenarios",
          div(
            class = "tablaResultados",
            h4("Diferencia de Costos", class = "tabla-titulo"),
            tableOutput("tablaCostosEVPDiff")
          )
        ),
        h3("Cisplatino + Nivolumab", class = "tabla-titulo"),
        div(
          class = "tablas-escenarios",
          div(
            class = "tablaResultados",
            h4("Escenario Proyectado", class = "tabla-titulo"),
            tableOutput("tablaCostosNIV")
          )
        ),
        div(
          class = "tablas-escenarios",
          div(
            class = "tablaResultados",
            h4("Diferencia de Costos", class = "tabla-titulo"),
            tableOutput("tablaCostosNIVDiff")
          )
        ),
      )
    }
  })

  observe({
    if (showingWaiter() == FALSE) {
      if (primerCorrida == TRUE) {
        waiter_hide()
        primerCorrida <<- FALSE
      } else {
        waiter_hide()
      }
    }
  })
}
