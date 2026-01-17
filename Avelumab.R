# HAy diferencias en manejo de enfermedad de EVP y en Adquisicion de Nivolumab

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

INFO_ROI_NOAPLICA <- "El retorno de inversión no fue estimado debido a que la intervención es menos costosa que el comparador."
ICER_C_SUP_IZQ <- "La intervención es menos costosa y menos efectiva, el valor presente representa el RCEI del comparador contra la intervención. Por tanto, la intervención será costo-efectiva si RCEI está por encima del umbral."

MAX_AÑOS_MUERTES <- 5

#[ROI/ICER] Modifique esta parte para implementarlo en una funcion simple
estimarRoi <- function(inversion, diferencia_otros_costos){
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Estima el ROI, solo si corresponde.%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 23/12 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
  
  if (inversion > 0)
  { 
    return(list(valor = ((diferencia_otros_costos - inversion) / inversion), info = ""))
  } else {
    return(list(valor = "-", info = INFO_ROI_NOAPLICA))
  }
}

interpretacionDecision <- function(deltaCosto, deltaBeneficio) {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Estima el ICER, si es costo-ahorrativo devuelve Dominante para evitar confusión con ICER negativo.%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
  
  if (deltaCosto <= 0 & deltaBeneficio > 0)
  {
    return(list(valor = "Dominante", info = ""))
  } else {
    if (deltaCosto <= 0 & deltaBeneficio < 0) {
      return(list(valor = "Menos Efectiva", info = ICER_C_SUP_IZQ))
      #[ROI/ICER]
      #Lo correcto aca sería devolver return(deltaCosto/deltaBeneficio) pero con mención tal vez poner un *
    } else {
      if (deltaCosto > 0 & deltaBeneficio < 0) {
        return(list(valor = "Dominada", info = ""))
      } else {
        if (deltaCosto > 0 & deltaBeneficio > 0) {
          return(list(valor = deltaCosto/deltaBeneficio, info = ""))
        }
      }
    }
  }
}

formatear_pesos <- function(x, decimales = 0) {
  if (is.numeric(x)){
    return(paste0("$", formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales)))} else {
    return(x)
  }
}
formatear_epi <- function(x, decimales = 2) {
  formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales)
}
formatear_porcentaje <- function(x, decimales = 2) {
  if (is.numeric(x)){
    return(paste0(formatC(x*100, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales), " %"))} else 
  { return(x)}
}
procesarResultados <- function(basal, proyectado, parametros) {
  

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Recibe los resultados de cada escenario y prepara los outputs%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  crear_tabla_diff <- function(tabla1, tabla2)
  {
    cols_anios <- setdiff(names(tabla1), "Categorias")
    tabla_costos_diff <- tabla1
    tabla_costos_diff[cols_anios] <-
      tabla2[cols_anios] - tabla1[cols_anios]
    
    tabla_costos_diff
  }
  crear_tabla_costos <- function(costos, categorias, campos, tHT, formatear) {
    
    tabla <- data.frame(
      Categorias = categorias,
      stringsAsFactors = FALSE
    )
    
    for (i in seq_len(tHT)) {
      tabla[[paste0("Año ", i)]] <-
        sapply(campos, \(campo) costos[[i]]$total[[campo]])
    }
    tabla$Promedio <- sapply(campos, \(campo) costos$total$promedio[[campo]])
    
    if (formatear) {
      tabla[-1] <- lapply(tabla[-1], formatear_pesos)
    }
    
    tabla
  }
  formatear_tabla <- function(
    tabla,
    filas_pesos = integer(),
    filas_porcentaje = integer(),
    filas_epi = integer(),
    decimales = NULL
  ) {
    
    if (!is.null(decimales)) {
      stopifnot(length(decimales) == nrow(tabla))
    }
    
    tabla[-1] <- lapply(tabla[-1], function(col) {
      
      res <- as.character(col)
      
      # PESOS
      if (length(filas_pesos) > 0) {
        for (f in filas_pesos) {
          d <- if (!is.null(decimales) && !is.na(decimales[f])) decimales[f] else 0
          res[f] <- do.call(
            formatear_pesos,
            list(col[f], d)
          )
        }
      }
      
      # PORCENTAJE
      if (length(filas_porcentaje) > 0) {
        for (f in filas_porcentaje) {
          d <- if (!is.null(decimales) && !is.na(decimales[f])) decimales[f] else 2
          res[f] <- do.call(
            formatear_porcentaje,
            list(col[f], d)
          )
        }
      }
      
      # EPIDEMIOLOGÍA
      if (length(filas_epi) > 0) {
        for (f in filas_epi) {
          d <- if (!is.null(decimales) && !is.na(decimales[f])) decimales[f] else 2
          res[f] <- do.call(
            formatear_epi,
            list(col[f], d)
          )
        }
      }
      
      res
    })
    
    tabla
  }
  agregarIPPorcentual <- function(tabla, tabla_basal) {
    
    fila_ip <- which(tabla$Categorias == "Impacto Presupuestario")
    
    nueva_fila <- tabla[fila_ip, , drop = FALSE]
    nueva_fila$Categorias <- "Impacto Presupuestario (%)"
    for (c in 2:ncol(tabla)) {
      nueva_fila[[c]] <- tabla[[c]][fila_ip] / tabla_basal[[c]][fila_ip]
    }
    rbind(tabla, nueva_fila)

  }
  agregarIPPMPM <- function(tabla, cohorte) {
    
    fila_ip <- which(tabla$Categorias == "Impacto Presupuestario")
    nueva_fila <- tabla[fila_ip, , drop = FALSE]
    nueva_fila$Categorias <- "Impacto Presupuestario PMPM"
    for (c in 2:ncol(tabla)) {

      nueva_fila[[c]] <- as.numeric(tabla[[c]][fila_ip]) / cohorte / 12
    }
    rbind(tabla, nueva_fila)
    
  }
  #Indicadores principales de ejemplo para el visualizador.
  indicadores <- list(deltaIndicador1 = formatear_epi(1),
                      deltaIndicador2 = formatear_epi(2),
                      deltaIndicador3 = formatear_epi(3),
                      deltaIndicador4 = formatear_epi(4) , 
                      deltaIndicador5 = formatear_pesos(5),
                      deltaIndicador6 = formatear_pesos(6)
  )

  # #Eventos sanitarios
  # tabla_sanitaria <- data.frame(
  #   Categorias = c("Tratamientos Exitosos", "Muertes", "LTFU", "Tratamientos Innecesarios", "Años Perdidos", "Disutilidad", "Dalys"),
  #   Escenario_Actual = c(basal$tratamientosExitosos, basal$muertes, basal$ltfu, basal$tratamientosInnecesarios, basal$lyLost, basal$disLost, basal$dalys),
  #   Escenario_Proyectado = c(proyectado$tratamientosExitosos, proyectado$muertes, proyectado$ltfu, proyectado$tratamientosInnecesarios, proyectado$lyLost, proyectado$disLost, proyectado$dalys),
  #   Diferencia =  c(proyectado$tratamientosExitosos - basal$tratamientosExitosos, proyectado$muertes - basal$muertes, proyectado$ltfu - basal$ltfu, proyectado$tratamientosInnecesarios - basal$tratamientosInnecesarios, proyectado$lyLost - basal$lyLost, proyectado$disLost - basal$disLost, proyectado$dalys - basal$dalys)
  # )
  # #Formateamos la tabla
  # tabla_sanitaria[] <- lapply(tabla_sanitaria, function(col) {
  #   if (is.numeric(col)) formatear_epi(col) else col
  # })
  # #Nombramos los headers
  # colnames(tabla_sanitaria) <- c("Categorias", 
  #                                "Comparador", 
  #                                "Intervención", 
  #                                "Diferencia")  
  bCostos <- basal$costos$PorAño
  pCostos <- proyectado$costos$PorAño
  
   #Resultados Costos
  #Tabla costos detallados
  campos_costos_detallados <- c(
    "adquisicion",
    "administracion",
    "eAdversos",
    "Enf",
    "subseEnf",
    "subseDrugs",
    "total"
  )
  
  categorias_costos_detallados <- c("Costos de Adquisición", "Costos de Administración", "Costos de Efectos Adversos", "Costos de Manejo de la Enfermedad y Monitoreo", "Costos de Monitoreo de tratamientos Subsecuentes", "Costos de Adquisición y Administración de Tratamientos Subsecuentes", "Total")
  
  tabla_costos <- crear_tabla_costos(costos = bCostos, categorias = categorias_costos_detallados, campos = campos_costos_detallados, tHT = parametros$tHT, formatear = FALSE)
  tabla_costosProy <- crear_tabla_costos(costos = pCostos, categorias = categorias_costos_detallados, campos = campos_costos_detallados, tHT = parametros$tHT, formatear = FALSE)
  tabla_costosDiff <- crear_tabla_diff(tabla1 = tabla_costos, tabla2 = tabla_costosProy)
  
  tabla_costos <- formatear_tabla(tabla_costos,  filas_pesos = c(1,2,3,4,5,6,7))
  tabla_costosProy <- formatear_tabla(tabla_costosProy,  filas_pesos = 1:7)
  tabla_costosDiff <- formatear_tabla(tabla_costosDiff,  filas_pesos = 1:7)
  
  #Tabla costos resumidos
  campos_costos_resumidos <- c(
    "drogas",
    "eAdversos",
    "EnfTodo",
    "subseDrugs",
    "total"
  )
  
  categorias_costos_resumidos <- c("Costos de Drogas", "Costos de Efectos Adversos", "Costos de Manejo de la Enfermedad y Monitoreo", "Costos de tratamientos Subsecuentes", "Total")
  
  tabla_costosR <- crear_tabla_costos(costos = bCostos, categorias = categorias_costos_resumidos, campos = campos_costos_resumidos, tHT = parametros$tHT, formatear = FALSE)
  tabla_costosProyR <- crear_tabla_costos(costos = pCostos, categorias = categorias_costos_resumidos, campos = campos_costos_resumidos, tHT = parametros$tHT, formatear = FALSE)
  tabla_costosDiffR <- crear_tabla_diff(tabla1 = tabla_costosR, tabla2 = tabla_costosProyR)
  
  tabla_costosR <- formatear_tabla(tabla_costosR,  filas_pesos = 1:5)
  tabla_costosProyR <- formatear_tabla(tabla_costosProyR,  filas_pesos = 1:5)
  tabla_costosDiffR <- formatear_tabla(tabla_costosDiffR,  filas_pesos = 1:5)
  
  
  categoria_main <- c("Costos de Drogas de Primera linea", "Costos de Drogas de segunda linea", "Otros costos sanitarios", "Impacto Presupuestario")
  campos_main <- c("drogas", "subseDrugs", "otros", "total")
  
  tabla_mainB <- crear_tabla_costos(costos = bCostos, categorias = categoria_main, campos = campos_main, tHT = parametros$tHT, formatear = FALSE)
  tabla_mainP <- crear_tabla_costos(costos = pCostos, categorias = categoria_main, campos = campos_main, tHT = parametros$tHT, formatear = FALSE)
  tabla_main <- crear_tabla_diff(tabla1 = tabla_mainB, tabla2 = tabla_mainP)
  tabla_main <- agregarIPPorcentual(tabla = tabla_main, tabla_basal = tabla_mainB)
  
  tabla_main <- agregarIPPMPM(tabla = tabla_main, cohorte = parametros$nAfiliados)
  tabla_main <- formatear_tabla(tabla_main, filas_pesos = c(1, 2, 3, 4, 6), filas_porcentaje = c(5), decimales = c(NA, NA, NA, NA, NA, 2))
  #Devolvemos resultados
  resultado = list(
    indicadores = indicadores,
    tablaCostos = tabla_costos,
    tablaCostosProy = tabla_costosProy,
    tablaCostosDiff = tabla_costosDiff,
    
    tablaCostosR = tabla_costosR,
    tablaCostosProyR = tabla_costosProyR,
    tablaCostosDiffR = tabla_costosDiffR,

    tablaMain = tabla_main
  )
  #Modificado 23/12 -->>>
  return(resultado)  
  
  
}



descontar <- function(años, tasa)
{
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Trae al presente valor futuro de x cantidad de años%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
  
  return(1 + ((1 - (1 + tasa) ^ (- (años - 1) )) / tasa))
}
descontarValorCiclo <- function(valor, tasa, ciclo) {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Descuento clásico de un valor x al ciclo y.%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
  
  return(valor / ((1 + tasa) ^ ciclo))
}
descontarAñosFuturos <- function(valor, tasa, cicloInicial, duracion, valorInicial) {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Descuenta un valor en el tiempo, por una duración dada.%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Sirve para calcular años de vida perdido que empiezan a contar mas adelante que el primer año%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  return(
    
    (valorInicial / ((1 + tasa) ^ (cicloInicial - 1))) + 
      (valor / ((1 + tasa) ^ (cicloInicial - 1)) * (1 - (1 + tasa) ^ -(duracion - cicloInicial)) / tasa)
    
  )
  
}
descontarMensual <- function(valor, mes_inicio, duracion, tasa_anual) {
  
  # Convertir tasa anual a mensual
  tasa_mensual <- (1 + tasa_anual)^(1/12) - 1
  
  # Caso sin descuento
  if (tasa_mensual == 0) {
    return(valor * duracion)
  }
  
  # Valor presente del flujo mensual
  vp <- (valor / tasa_mensual) *
    (1 - (1 + tasa_mensual)^(-duracion)) /
    (1 + tasa_mensual)^(mes_inicio - 1)
  
  return(vp)
}


correrModelo <- function(parametros) {

  
  cohortes <- estimarPoblacion(parametros)
  print(cohortes)
  cohorteBasal <- distribuirPoblacion(cohortes, parametros, basal = 1)
  cohorteProyectada <- distribuirPoblacion(cohortes, parametros, basal = 0)
  costosMensualesDrogas <- estimarCostosDrogas(parametros)
  tratamientosDuraciones <- estimarTiempos(parametros)
  eAdversos <- estimarEfectosAdversos(parametros)
  costosMensualesSubsecuentes <- estimarCostosSubsecuentes(parametros, costosMensualesDrogas$subsecuentes)
  print(costosMensualesSubsecuentes)
  print("RESULTADOS BASAL")
  costosBasal <- estimarCostos(cohorteBasal, costosMensualesDrogas, tratamientosDuraciones, eAdversos, costosMensualesSubsecuentes, parametros)
  print("RESULTADOS PROYECTADO")
  costosProyectado <- estimarCostos(cohorteProyectada, costosMensualesDrogas, tratamientosDuraciones, eAdversos, costosMensualesSubsecuentes, parametros)
  
  
  
  
  
  
  #Inicia correr modelo
  resultadosBasal <- list(
    costos = costosBasal,
    cohorte = cohorteBasal
  )
  resultadosProyectado <- list(
    costos = costosProyectado,
    cohorte = cohorteProyectada
  )
  
  
  return(procesarResultados(resultadosBasal, resultadosProyectado, parametros))
}
estimarCostos <- function(cohorte, cDrogas, tDuraciones, eAdversos, cSubsecuentes, parametros) {
  estrategias <- c("AVE", "NIV", "BSC", "EVP", "QMTNR")
  rCostos <- list()
  for (año in 1:parametros$tHT) {
    #cohortes
    peCostos <- list()
    for (e in estrategias) {
      paCostos <- list()
      for (a in 1:parametros$tHT) {
        costos <- list()
        
        tInduccion <- max(0, min(tDuraciones[[e]]$dInduccion - ((a - 1) * 12), 12))

        tMantenimiento <- max(0, 
                                min(tDuraciones[[e]]$dMantenimiento + tDuraciones[[e]]$iMantenimiento, a * 12) -
                                max(tDuraciones[[e]]$iMantenimiento, (a - 1) * 12)
                               
                              )

        tSobrevida <- max(0,
                          min(tDuraciones[[e]]$sobrevida, a * 12) -
                          ((a - 1) * 12)
                          )
        
        
        tPreP_OnT <- min(tDuraciones[[e]]$pfs, max(tDuraciones[[e]]$iMantenimiento, tDuraciones[[e]]$dInduccion) + tDuraciones[[e]]$dMantenimiento)
        tPreP_OffT <- tDuraciones[[e]]$pfs - tPreP_OnT
        tProg <- tDuraciones[[e]]$pfs
        
       # tPosP_iOnT <- max(tProg, max(tDuraciones[[e]]$iMantenimiento, tDuraciones[[e]]$dInduccion) + tDuraciones[[e]]$dMantenimiento)
       # tPosP_fOnT <- min(tDuraciones[[e]]$sobrevida, tPosP_iOnT  + cSubsecuentes[[e]]$duracion)
       # tPos_dOnT <- tPosP_fOnT - tPosP_iOnT
       # tPosP_OffT <- tDuraciones[[e]]$sobrevida - tPosP_fOnT
        
        tOnT_Induccion <- max(0,
                              min(tDuraciones[[e]]$dInduccion, a * 12) -
                              ((a - 1) * 12)
                              )
        tOnT_Mantenimiento <- max(0,
                                min(tDuraciones[[e]]$dMantenimiento + tDuraciones[[e]]$iMantenimiento, a * 12) -
                                max(tDuraciones[[e]]$iMantenimiento, (a - 1) * 12)
                              )
        tOffT <- max(0,
                     min(tDuraciones[[e]]$pfs, a * 12) - 
                     max(ifelse(e=="QMTNR", 5.2,tPreP_OnT), ((a - 1) * 12))
                     )
        
        iST <- max(max(tDuraciones[[e]]$iMantenimiento, tDuraciones[[e]]$dInduccion) + tDuraciones[[e]]$dMantenimiento, tDuraciones[[e]]$pfs)
        dST <- min(tDuraciones[[e]]$sobrevida - iST,  cSubsecuentes[[e]]$duracion)

        tSTOn <- max(0,
                     min(iST + dST, a * 12) -
                     max(iST, (a - 1) * 12)
                     )
        tSTOff <- max(0,
                      min(tDuraciones[[e]]$sobrevida, a * 12) -
                      max(iST + dST, (a - 1) * 12)
        )

        # Costos de Adquisición Primera Linea
          costos$adquisicion <- cohorte[[año]][[e]] * (cDrogas$primeraLinea$adquisicion$induccion[[e]] * tInduccion + cDrogas$primeraLinea$adquisicion$mantenimiento[[e]] * tMantenimiento)
          
        # Costos de Adminsitración Primera Linea
          costos$administracion <- cohorte[[año]][[e]] * (cDrogas$primeraLinea$administracion$induccion[[e]] * tInduccion + cDrogas$primeraLinea$administracion$mantenimiento[[e]] * tMantenimiento)     
        
        # Costos de Drogas primera linea
          costos$drogas <- costos$adquisicion + costos$administracion
        # Costos de Efectos Adversos
          costos$eAdversos <- ifelse(a == 1, eAdversos[[e]], 0) * cohorte[[año]][[e]]
        # Costos de manejo de la enfermedad
          costos$preOnT <- ((parametros[[paste0("cPreP_OnT_m", e, "I")]] * tOnT_Induccion + parametros[[paste0("cPreP_OnT_o", e, "I")]] * (a == 1)) * cohorte[[año]][[e]]
          +  (parametros[[paste0("cPreP_OnT_m", e, "M")]] * tOnT_Mantenimiento + parametros[[paste0("cPreP_OnT_o", e, "M")]] * (tDuraciones[[e]]$iMantenimiento >= ((a - 1) * 12) && tDuraciones[[e]]$iMantenimiento <= a * 12)) * cohorte[[año]][[e]])
          costos$preOffT <-  ifelse(tOffT > 0 ,(parametros[[paste0("cPreP_OffT_m")]] * tOffT + parametros[[paste0("cPreP_OffT_o")]] * (tPreP_OffT >= ((a - 1) * 12) && tPreP_OffT <= (a * 12)  ) ) *   cohorte[[año]][[e]],0)
          costos$Enf <- costos$preOnT + costos$preOffT
          
        # Costos de Tratamientos subsecuentes
          costos$subsecOnT <- (ifelse(tProg >= (a-1) * 12 && tProg <= a * 12, parametros$cProgresion_o, 0) + ((tSTOn * parametros$cPosp_OnT_m + parametros$cPosp_OnT_o * (iST >= (a - 1) * 12 && iST <= a * 12)) *  parametros[[paste0("pSD_", e)]]) + (1 - parametros[[paste0("pSD_", e)]]) * tSTOn * parametros$cPosp_OffT_m) * cohorte[[año]][[e]]
          costos$subseOffT <- ifelse(tSTOff > 0, (tSTOff * parametros$cPosp_OffT_m + parametros$cPosp_OffT_o * (iST + dST >= (a - 1) * 12 && iST + dST <= a * 12)), 0) * cohorte[[año]][[e]]
          costos$subseEnf <- costos$subsecOnT  + costos$subseOffT 
          costos$subseDrugs <- tSTOn * cSubsecuentes[[e]]$costo * cohorte[[año]][[e]]

          costos$EnfTodo <- costos$Enf + costos$subseEnf
          costos$otros <- costos$EnfTodo + costos$eAdversos
          
        # Costos totales
          costos$total <- costos$drogas + costos$Enf + costos$subseEnf + costos$subseDrugs + costos$eAdversos
        paCostos[[a]] <- costos
        #print(paste(e, "-", "Pob", cohorte[[año]][[e]],"Costos en año", a, "de cohorte", año, ": Adquisicion:", costos$adquisicion, "Administracion:", costos$administracion, "Ea", costos$eAdversos))      
      }
      peCostos[[e]] <- paCostos
    }
    rCostos[[año]] <- peCostos
  }
  resCostos <- list(porCohorte = rCostos)
  
  camposCostos <- c(
    "adquisicion", "administracion", "drogas", "eAdversos",
    "preOnT", "preOffT", "Enf",
    "subsecOnT", "subseOffT", "subseEnf", "EnfTodo", "otros",
    "subseDrugs", "total"
  )

  raCostos <- list()
  yTotal <-  as.list(setNames(rep(0, length(camposCostos)), camposCostos))
  for (año in 1:parametros$tHT) {
    pacCostos <- list()
    total <- as.list(setNames(rep(0, length(camposCostos)), camposCostos))
    for (e in estrategias) {
      pecCostos <- as.list(setNames(rep(0, length(camposCostos)), camposCostos))
      for (i in 1:parametros$tHT) {
        if (i <= año) {
          for (campo in names(pecCostos)) {
            
            pecCostos[[campo]] <- pecCostos[[campo]] +
            rCostos[[i]][[e]][[año - i + 1]][[campo]] 
            total[[campo]] <- total[[campo]] + rCostos[[i]][[e]][[año - i + 1]][[campo]] 
            yTotal[[campo]] <- yTotal[[campo]] + rCostos[[i]][[e]][[año - i + 1]][[campo]] 
            #print(paste("Estrategia", e, "Cohorte", i, "en año", año, "corregido", año - i + 1, campo, rCostos[[i]][[e]][[año - i + 1]][[campo]]))
          }
        }
      }
      pacCostos[[e]] <- pecCostos
    }
    pacCostos$total <- total
    raCostos[[año]] <- pacCostos
  }
  resCostos$PorAño <- raCostos

  xTotal <- list()
  xPromedio <- list()
  for (e in estrategias) {
    zTotal <- as.list(setNames(rep(0, length(camposCostos)), camposCostos))
    zPromedio <- as.list(setNames(rep(0, length(camposCostos)), camposCostos))
    for (i in 1:parametros$tHT){
      for (campo in names(pecCostos)) {
        zTotal[[campo]] <- zTotal[[campo]] +  raCostos[[i]][[e]][[campo]]
      }
    }
    for (campo in names(pecCostos)) {
      zPromedio[[campo]] <- zTotal[[campo]] / parametros$tHT
    }
    xTotal[[e]] <- zTotal
    xPromedio[[e]] <- zPromedio
  }
  raCostos$total <- xTotal
  raCostos$promedio <- xPromedio
  yPromedio <- list()
  for (campo in names(pecCostos)) {
    yPromedio[[campo]] <- yTotal[[campo]] / parametros$tHT
  }
  
  raCostos$total$total <- yTotal
  raCostos$total$promedio <- yPromedio
  resCostos$PorAño <- raCostos

  return(resCostos)

}
distribuirPoblacion <- function(cohortes, parametros, basal) {
  
  msCohorte <- list()

  escenario <- ifelse(basal == 1, "EB", "EP")
  for (año in 1:parametros$tHT)
  {
    distribucion <- list()
    
    distribucion$EVP <- parametros[[paste0("msEVP", escenario, año)]] * cohortes[[año]]
    distribucion$NIV <- parametros[[paste0("msNIV", escenario, año)]] * cohortes[[año]]
    
    QMT <- (1 - parametros[[paste0("msNIV", escenario, año)]] - parametros[[paste0("msEVP", escenario, año)]]) * cohortes[[año]]
    
    distribucion$QMTNR <- QMT * (1 - parametros$pNoProgresa)
    
    QMT <- QMT - distribucion$QMTNR
    
    distribucion$AVE <- QMT *  parametros[[paste0("msAVE", escenario, año)]]
    distribucion$BSC <- QMT - distribucion$AVE
    
    distribucion$total <- distribucion$AVE + distribucion$BSC + distribucion$QMTNR + distribucion$EVP + distribucion$NIV
    msCohorte[[año]] <- distribucion
  }
  
  
  return(msCohorte)
}

estimarCostosSubsecuentes <- function(parametros, costosDrogas) {
  
  costosSubsecuentes <- list()
  
  estrategias <- c("AVE", "NIV", "BSC", "EVP", "QMTNR")
  tratamientosSegundaLinea <- c("Atezolizumab", "Nivolumab", "Pembro", "Durvalumab", "Vinflunine", "Cisplatino", "Carboplatino", "Gemcitabine", "Docetaxel", "Paclitaxel", "EV2", "Erdafitinib")
  
  for (e in estrategias)
  {
    pAcumulado <- 0
    costo <- 0
    duracion <- 0
    for (tsl in tratamientosSegundaLinea)
    {
      pAcumulado <- pAcumulado + parametros[[paste0("pSD_", e, "_", tsl)]]
      duracion <- duracion + parametros[[paste0("pSD_", e, "_", tsl)]] * parametros[[paste0("tSTD_", e, "_", tsl)]]
      costo <- costo + parametros[[paste0("pSD_", e, "_", tsl)]] * (costosDrogas$adquisicion[[tsl]] + costosDrogas$administracion[[tsl]])
      
    }
    duracion <- duracion / pAcumulado
    
    costosSubsecuentes[[e]] <- list(
      costo = costo * parametros[[paste0("pSD_", e)]],
      duracion = duracion
    )
  }
  return(costosSubsecuentes)

}
estimarEfectosAdversos <- function(parametros) {
  
  estrategias <- c("AVE", "NIV", "BSC", "EVP", "QMTNR")
  efectosAdversos <- c("Diarrea", "ITU", "Anemia", "Plqd", "GBd", "Hiperglu", "Rash", "Neutd", "Pnp")
  costosAdversos <- list()
  for (e in estrategias) 
  {
    costoEA <- 0
    for (ea in efectosAdversos) {
      costoEA <- costoEA + parametros[[paste0("cEA_", ea)]] * parametros[[paste0("pEA_", ea, "_", e)]]
      
    }
    

    costosAdversos[[e]] <- costoEA
  }
  
  return(costosAdversos)
  
  
}
estimarTiempos <- function(parametros) {
  
  tiemposTratamientos <- list()
  estrategias <- c("AVE", "NIV", "BSC", "EVP", "QMTNR")
  
  for (e in estrategias) 
  {
    
    duracionInduccion <- parametros[[paste0("tDura", e, "I")]]
    inicioMantenimiento <- parametros[[paste0("tInicioMant", e)]]
    duracionMantenimiento <- parametros[[paste0("tDura", e, "M")]]
    sobrevidaPF <- parametros[[paste0("tPFS", e)]]
    sobrevidaGlobal <- parametros[[paste0("tOS", e)]]
    
    tiemposTratamientos[[e]] <- list(
      dInduccion = duracionInduccion,
      iMantenimiento = inicioMantenimiento,
      dMantenimiento = duracionMantenimiento,
      pfs = sobrevidaPF,
      sobrevida = sobrevidaGlobal
    )
    
    
  }

  return(tiemposTratamientos)
}
estimarCostosDrogas <- function(parametros) {
  
  primeraLinea <- list()
  
  tratamientosPrimeraLinea <- c("Avelumab", "EV", "Pembro", "Nivolumab", "Cisplatino", "Carboplatino", "Gemcitabine")
  
  for (t in tratamientosPrimeraLinea) {
    
    dosis <- switch(parametros[[paste0("nTD_", t)]] + 1,
                    parametros[[paste0("nDos_", t)]],
                    parametros[[paste0("nDos_", t)]] * parametros$nPeso,
                    parametros[[paste0("nDos_", t)]] * parametros$nSuperficie,
                    parametros[[paste0("nDos_", t)]] * (parametros$nFiltrado + 25)
      
    )
    
    dosisCiclo <- parametros[[paste0("nNA_porCiclo_", t)]] * dosis
    
    ciclosMes <- (365.25 / 7 /12) / parametros[[paste0("nCL_", t)]] 
    
    dosisMensual <- dosisCiclo * ciclosMes
    
    costoMG <- parametros[[paste0("c", t)]] / parametros[[paste0("nVS_", t)]]
    
    costoMensual <- dosisMensual * costoMG
    
    primeraLinea[[t]] <- costoMensual
  }

  primeraLinea$QMT <- primeraLinea$Cisplatino * parametros$pCisplatino + primeraLinea$Carboplatino * (1 - parametros$pCisplatino) + primeraLinea$Gemcitabine
  # ----------------- Costos de Adquisición ------------------------------------
  costos_adquisicion <- list()
  costos_administracion <- list()

  costos_adquisicion$induccion <- list()
  costos_adquisicion$mantenimiento <- list()
  
  costos_adquisicion$induccion$AVE <- primeraLinea$QMT
  costos_adquisicion$mantenimiento$AVE <- primeraLinea$Avelumab
  
  costos_adquisicion$induccion$BSC <- primeraLinea$QMT
  costos_adquisicion$mantenimiento$BSC <- 0
  
  costos_adquisicion$induccion$QMTNR <- primeraLinea$QMT
  costos_adquisicion$mantenimiento$QMTNR <- 0
  
  costos_adquisicion$induccion$EVP <- primeraLinea$EV + primeraLinea$Pembro
  costos_adquisicion$mantenimiento$EVP <- primeraLinea$Pembro
  
  costos_adquisicion$induccion$NIV <- primeraLinea$Nivolumab + primeraLinea$QMT
  costos_adquisicion$mantenimiento$NIV <- primeraLinea$Nivolumab
  # ----------------- Costos de Administración ------------------------------------
  costos_administracion$induccion <- list()
  costos_administracion$mantenimiento <- list()
  
  costos_administracion$induccion$AVE <- max(parametros$nNA_porCiclo_Gemcitabine, parametros$nNA_porCiclo_Cisplatino) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Cisplatino
  costos_administracion$mantenimiento$AVE <- parametros$nNA_porCiclo_Avelumab * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Avelumab
  
  costos_administracion$induccion$BSC <- max(parametros$nNA_porCiclo_Gemcitabine, parametros$nNA_porCiclo_Cisplatino) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Cisplatino
  costos_administracion$mantenimiento$BSC <- 0
  
  costos_administracion$induccion$QMTNR <- max(parametros$nNA_porCiclo_Gemcitabine, parametros$nNA_porCiclo_Cisplatino) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Cisplatino
  costos_administracion$mantenimiento$QMTNR <- 0

  costos_administracion$induccion$EVP <- max(parametros$nNA_porCiclo_Pembro, parametros$nNA_porCiclo_EV ) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_EV
  costos_administracion$mantenimiento$EVP <- parametros$nNA_porCiclo_Pembro * parametros$cAdministracion * (365.25 / 7 / 12) / parametros$nCL_Pembro
  
  costos_administracion$induccion$NIV <- max(parametros$nNA_porCiclo_Gemcitabine, parametros$nNA_porCiclo_Cisplatino, parametros$nNA_porCiclo_Nivolumab) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Cisplatino
  costos_administracion$mantenimiento$NIV <- parametros$nNA_porCiclo_Nivolumab * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Nivolumab
  
  
  costosPrimeraLinea <- list(
    adquisicion = costos_adquisicion,
    administracion = costos_administracion
  )
  
  # ---------------------- Segunda Linea --------------------------
  
  tratamientosSegundaLinea <- c("Atezolizumab", "Nivolumab", "Pembro", "Durvalumab", "Vinflunine", "Cisplatino", "Carboplatino", "Gemcitabine", "Docetaxel", "Paclitaxel", "EV2", "Erdafitinib")
  costo_adquisicion2 <- list()
  costo_administracion2 <- list()
  
  for (t in tratamientosSegundaLinea) {
    
    dosis <- switch(parametros[[paste0("nTD_", t)]] + 1,
                    parametros[[paste0("nDos_", t)]],
                    parametros[[paste0("nDos_", t)]] * parametros$nPeso,
                    parametros[[paste0("nDos_", t)]] * parametros$nSuperficie,
                    parametros[[paste0("nDos_", t)]] * (parametros$nFiltrado + 25)
                    
    )
    
    dosisCiclo <- parametros[[paste0("nNA_porCiclo_", t)]] * dosis
    
    ciclosMes <- (365.25 / 7 /12) / parametros[[paste0("nCL_", t)]] 
    
    dosisMensual <- dosisCiclo * ciclosMes
    
    costoMG <- parametros[[paste0("c", t)]] / parametros[[paste0("nVS_", t)]]
    
    costo_adquisicion2[[t]] <- dosisMensual * costoMG
    
    if (!t %in% c("Carboplatino", "Cisplatino", "Erdafitinib")) {
      costo_administracion2[[t]] <- parametros[[paste0("nNA_porCiclo_", t)]] * (365.25 / 7 /12) / parametros[[paste0("nCL_", t)]] * parametros$cAdministracion
    } else {
      costo_administracion2[[t]] <- 0
    }
    
  }
  

  
  costosSegundaLinea <- list(
    adquisicion = costo_adquisicion2,
    administracion = costo_administracion2
  )
  
  
  costosDrogas <- list(
    primeraLinea = costosPrimeraLinea,
    subsecuentes = costosSegundaLinea
  )
  return(costosDrogas)
}
estimarPoblacion <- function(parametros) {
  
  
  cohorte <- parametros$nAfiliados
  poblacion <- list()
  
  for(i in 1:parametros$tHT)
  {
    nIncidentes <- cohorte * sum(vapply(c("H064", "M064", "H65", "M65"), function(x){ 
      parametros[[paste0("nIncidencia", x)]] * 0.00001 * parametros[[paste0("p", x)]]
    }, numeric(1)))
    
    nUroteliales <- nIncidentes * parametros$pCaUrotelialVejiga / parametros$pCaVejigaUrotelial
    
    nAvanzados <- nUroteliales * (parametros$pDistancia + parametros$pRegional + (1 - parametros$pDistancia - parametros$pRegional) * parametros$pLocalizadosProgresan)
    
    nElegibles <- nAvanzados * parametros$pPlatinoElegible
    
    poblacion[[i]] <- nElegibles
    
    cohorte <- cohorte + (cohorte * parametros$pAnualGrowth)
  }
  
  return(poblacion)
}
