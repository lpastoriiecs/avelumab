source("login.R")
source("Avelumab.R")

cargarDatos()

cargarEtiquetasTooltips()

server <- function(input, output, session) {
  ############################################################ CODIGO DE LOGIN ##############################################################################
  #funcion reactiva
  ajustaInflacion <- reactiveVal(TRUE)
  #app_visible <- reactiveVal(FALSE) #Cambiar esto a false
  app_visible <- reactiveVal(TRUE)
  showingWaiter <- reactiveVal(TRUE)
  primerCorrida <- TRUE
  #Modulos
  #user_logged <- mod_login_server(LOGIN_MODULO, APLICACION_ID, app_visible)

  session$sendCustomMessage("inicializar-tooltips", tooltip_list)
  #Funciones encapsuladas que manejan UI general
  
  
  informacionInflacion <- obtenerInflacion(FECHA_COSTOS)
  if (!is.null(informacionInflacion)) {
    modificadorInflacion <- informacionInflacion[[1]]
    fechaInflacion <- informacionInflacion[[2]]
  } else {
    modificadorInflacion <- 1
  }
  print("Deberiamos ajustar datos")
  sMutables <- list()
  
  for (c in sectores) {
    sMutables[[c]] <- ajustarDatos(mutables[[c]], 2, modificadorInflacion)
  }

  user_logged <- reactiveVal(TRUE)
  observeEvent(input$"toggle_tasa",
               {
                 toggleClass("card-tasa", "collapsed")  # Alterna clase collapsed
                 # Cambia el texto del botón
                 current <- input$toggle_tasa %% 2
                 updateActionButton(session, "toggle_tasa", label = ifelse(current == 1, "+", "−"))
               })
  observeEvent(input$"toggle_economicos",
               {
                 toggleClass("card-economicos", "collapsed")  # Alterna clase collapsed
                 # Cambia el texto del botón
                 current <- input$toggle_economicos %% 2
                 updateActionButton(session, "toggle_economicos", label = ifelse(current == 1, "+", "−"))
               })
  observeEvent(input$"toggle_epidemiologicos",
               {
                 toggleClass("card-epidemiologicos", "collapsed")  # Alterna clase collapsed
                 # Cambia el texto del botón
                 current <- input$toggle_epidemiologicos %% 2
                 updateActionButton(session, "toggle_epidemiologicos", label = ifelse(current == 1, "+", "−"))
               })

  loginObservers <- function()
  {
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
      shinyjs::runjs(sprintf('$("#%s").show();', paste(LOGIN_MODULO, "-loginPanel", sep ="")))
    })
  }

  loginObservers()
  ############################################################ CODIGO DE LOGIN ##############################################################################
  validar_inputs <- function()
  {
    return(TRUE)
  }
  switch_tab <- function(tab_id, link_id) {
    print("Corre switch_tab")
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
    active_tab(link_id)  # Guardar como actual
  }
  
  active_tab <- reactiveVal("navIntroduccion")
  
  observe({
    if (user_logged()) {
      req(cargoParametros)
      print("✅ Usuario logueado, acceso concedido")
      # Aquí puedes hacer algo más, como mostrar contenido exclusivo para usuarios logueados
      #shinyjs::hide("login1-loginPanel")
      waiter_show(
        html = div(style ="font-size: 50px;", class="spinner"),
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
  

  #Corre modelo
  resultado_modelo <- reactive({
    print("Intento correr modelo")
#    print(params_inmut)
    req(isolate(active_tab() != "navConfiguracion"))
    print(paste("Cargo Parmetros:", cargoParametros))
    req(cargoParametros)
    print("ok")
    print('Avanzo')
    if (user_logged()) {
    if (primerCorrida == FALSE){
      waiter_show(
        html = div(style = paste0("color: ", COLOR_PRIMARIO, "; font-size: 50px;", class="spinner")),
        color = "rgba(255, 255, 255, 0.2)"  # fondo casi transparente
      )
      showingWaiter(TRUE)
    }
    res <- correrModelo(reactiveValuesToList(params))
    #res <- correrFuncion(reactiveValuesToList(params))
    showingWaiter(FALSE)
    return(res)
    } else { 
      return(NULL)
  }
  })
  
  #Actualizamos el Parametro al modificar un input
  for (nombre in names(mutables[[sectores[[1]]]])) {
    print(paste("Actualiza al modificar input: ", nombre))
    local({
      nombre_local <- nombre
      observeEvent(input[[nombre_local]], {
        if (isTRUE(flags_actualizando[[nombre_local]])) {
          flags_actualizando[[nombre_local]] <- FALSE
          return()
        }
        valor <- switch(mutables_opciones[[nombre_local]],
                        input[[nombre_local]],
                        input[[nombre_local]] * 0.01,
                        input[[nombre_local]],
                        as.numeric(input[[nombre_local]])
                        )
        if (!is.null(valor)) {
          params[[nombre_local]] <- valor
        }
      }, ignoreInit = TRUE)
    })
  }
  inflacionarParametros <- function() {
    if (ajustaInflacion() == TRUE) {

        selected_modif <- sMutables[[input$perspectiva]]
        
    } else {
      selected_modif <- mutables[[input$perspectiva]]
    }
    
    # Cargar modificables a reactiveValues
    for (name in names(selected_modif)) {
      if (mutables_inflacion[[name]] == 1){
        params[[name]] <- selected_modif[[name]]
      }
    }
    for (nombre in names(params)) {
      if (nombre %in% names(input) && nombre != "perspectiva") {
        if (mutables_inflacion[[nombre]] == 1){
          flags_actualizando[[nombre]] <- TRUE
          if (mutables_opciones[[nombre]] <= 3) {
            updateNumericInput(session, inputId = nombre, value = switch(mutables_opciones[[nombre]],
                                                                       params[[nombre]],
                                                                       params[[nombre]] * 100,
                                                                       params[[nombre]]))
          } else if (mutables_opciones[[nombre]] == 4) {
            updatePrettyCheckbox(session, inputId = nombre, value = params[[nombre]])
          }
        
        }
      } else {
        
        #if (substr(nombre, 1, nchar(nombre) - 1) == "msTeplizumab")
        #{
        #  print(paste("Deberia haber asignado TRUE a", nombre))
        #  flags_actualizando[[nombre]] <- TRUE
        #  updateNumericInput(session, inputId = paste0("t", nombre), value = params[[nombre]] * 100)
        #  
        #}
      }
    }
    cargoParametros <<- TRUE
    print("Actualizo parametros")
  }
  actualizarParametros <- function() {
    if (ajustaInflacion() == TRUE) {
      selected_modif <- sMutables[[input$perspectiva]]
    } else {
      selected_modif <- mutables[[input$perspectiva]]
    }
    print("function actualizar parametros")
    # Cargar modificables a reactiveValues
    for (name in names(selected_modif)) {
      print(paste("Asigno valor", selected_modif[[name]], "a ", name))
      params[[name]] <- selected_modif[[name]]
    }
    for (nombre in names(params)) {
      if (nombre %in% names(input) && nombre != "perspectiva") {
        flags_actualizando[[nombre]] <- TRUE
        if (mutables_opciones[[nombre]] <= 3) {
          updateNumericInput(session, inputId = nombre, value = switch(mutables_opciones[[nombre]],
                                                                       params[[nombre]],
                                                                       params[[nombre]] * 100,
                                                                       params[[nombre]]))
        } else if (mutables_opciones[[nombre]] == 4) {
          updatePrettyCheckbox(session, inputId = nombre, value = params[[nombre]])
        }
        
      } else {
        
        #if (substr(nombre, 1, nchar(nombre) - 1) == "msTeplizumab")
        #{
        #  print(paste("Deberia haber asignado TRUE a", nombre))
        #  flags_actualizando[[nombre]] <- TRUE
        #  updateNumericInput(session, inputId = paste0("t", nombre), value = params[[nombre]] * 100)
        #  
        #}
      }
    }

    cargoParametros <<- TRUE
    print("Actualizo parametros")
  }
  #Cambia la perspectiva actualiza parametros
  observeEvent(input$perspectiva, {
    req(input$perspectiva)
    actualizarParametros()
    
  })
  observeEvent(input$bInflacion, {
    ajustaInflacion(input$bInflacion)
    print("modifico")
    inflacionarParametros()
    if (modificadorInflacion != 1 && ajustaInflacion() == TRUE) {
      pieTabla(gsub("%1", fechaInflacion, PIE_DE_TABLA1))
    } else {
      pieTabla(PIE_DE_TABLA2)
    }
    print(pieTabla)
  })
  if (modificadorInflacion != 1) {
    pieTabla <- reactiveVal(gsub("%1", fechaInflacion, PIE_DE_TABLA1))
  } else {
    pieTabla <- reactiveVal(PIE_DE_TABLA2)
  }
  output$pieTablaPrincipales <- renderText({pieTabla()})
  output$pieTablaResumidos <- renderText({pieTabla()})
  output$pieTablaDetallados <- renderText({pieTabla()})
  

  #INDICADOREs-----------------------------------------
  output$deltaExitosos <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaExitosos
  })
  output$deltaMuertes <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaMuertes    
  })
  output$deltaLTFU <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaLTFU
  })
  output$deltaLyLost <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaLyLost
  })
  output$deltaCostoTesteo <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaCostoTesteo
  })
  output$deltaOtrosCostos <- renderText({
    req(resultado_modelo())
    resultado_modelo()$indicadores$deltaOtrosCostos
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
   output$tablaCostos <- renderTable({
     req(resultado_modelo())  
     resultado_modelo()$tablaCostos
   }, , striped = FALSE, bordered = FALSE, hover = FALSE, spacing = "s", class = "tablaResultados")
  output$tablaDalys <- renderTable({
    req(resultado_modelo())
    resultado_modelo()$tablaDalys
  }, , striped = FALSE, bordered = FALSE, hover = FALSE, spacing = "s", class = "tablaResultados")
   output$tablaSanitaria <- renderTable({
     req(resultado_modelo())  
     resultado_modelo()$tablaSanitaria 
   }, , striped = FALSE, bordered = FALSE, hover = FALSE, spacing = "s", class = "tablaResultados")
   output$tablaMain <- renderTable({
     req(resultado_modelo())  
     resultado_modelo()$tablaMain 
   }, , striped = FALSE, bordered = FALSE, hover = FALSE, spacing = "s", class = "tablaResultados")
   output$ICERINFO <- renderText({
     resultado_modelo()$infoICER
   })
   output$ROIINFO <- renderText({
     resultado_modelo()$infoROI
   })
  
  observe({
    if (showingWaiter() == FALSE){
      if (primerCorrida == TRUE){
        waiter_hide()
        primerCorrida <<- FALSE
      } else {
        waiter_hide()
      }
    }
  })
}

