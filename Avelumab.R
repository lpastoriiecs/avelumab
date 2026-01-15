
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
formatear_porcentaje <- function(x, decimales = 0) {
  if (is.numeric(x)){
    return(paste0(formatC(x*100, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales), " %"))} else 
  { return(x)}
}
procesarResultados <- function(basal, proyectado) {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Recibe los resultados de cada escenario y prepara los outputs%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

  #Indicadores principales de ejemplo para el visualizador.
  indicadores <- list(deltaExitosos = formatear_epi(1),
                      deltaMuertes = formatear_epi(2),
                      deltaLTFU = formatear_epi(3),
                      deltaLyLost = formatear_epi(4) , 
                      deltaCostoTesteo = formatear_pesos(5),
                      deltaOtrosCostos = formatear_pesos(6)
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
  
  #Resultados Costos
  tabla_costos <- data.frame(
    Categorias = c("Costos de Testeo"),
    Escenario_Actual = c(0), 
    Escenario_Proyectado = c(0),
    Diferencia =  c(0)
  )
  #Formateamos la tabla
  tabla_costos[] <- lapply(tabla_costos, function(col) {
    if (is.numeric(col)) formatear_pesos(col) else col
  })
  #Nombramos Headers
  colnames(tabla_costos) <- c("Categorias", 
                                 "Comparador", 
                                 "Intervención", 
                                 "Diferencia")
  
  
  # #Resultados DALYS
  # tabla_dalys <- data.frame(
  #   Categorias = c("Años de vida perdidos por muerte prematura", "Años de vida perdidos por muerte prematura (d)", "Años de vida Ajustados por Discapacidad por TBC", "Años de vida Ajustados por Discapacidad por vivir con TBC (d)", "Años de Vida Ajustados por Discapacidad", "Años de Vida Ajustados por Discapacidad (d)"),
  #   Escenario_Actual = c(basal$lyLost, basal$dLyLost, basal$disLost, basal$dDisLost, basal$dalys, basal$dDalys),
  #   Escenario_Proyectado = c(proyectado$lyLost, proyectado$dLyLost, proyectado$disLost, proyectado$dDisLost, proyectado$dalys, proyectado$dDalys),
  #   Diferencia =  c(proyectado$lyLost - basal$lyLost, proyectado$dLyLost - basal$dLyLost, proyectado$disLost - basal$disLost, proyectado$dDisLost - basal$dDisLost,  proyectado$dalys - basal$dalys, proyectado$dDalys - basal$dDalys)
  # )
  # #Formateamos la tabla
  #  tabla_dalys[] <- lapply(tabla_dalys, function(col) {
  #    if (is.numeric(col)) formatear_epi(col) else col
  #  })
  # #Nombramos headers
  # colnames(tabla_dalys) <- c("Categorias",
  #                             "Comparador",
  #                             "Intervención",
  #                             "Diferencia")
  
  
  # #Calculamos delta costos para estimar ICERS y ROI
  # costo_total_intervencion <- proyectado$costoTesteo + proyectado$costoProgramatico
  # diferencia_otros_costos <- basal$otrosCostos - proyectado$otrosCostos
  # diferencia_costos <- (costo_total_intervencion - basal$costoTesteo) - diferencia_otros_costos
  # 
  # dDiferencia_otros_costos <- basal$dOtrosCostos - proyectado$dOtrosCostos
  # dDiferencia_costos <- (costo_total_intervencion - basal$costoTesteo) - diferencia_otros_costos
  # 
  # inversion <- costo_total_intervencion - basal$costoTesteo
  # #Modificado 23/12 <<<--
  
    
  #[ROI/ICER] en la tabla ahora mostramos el objeto valor dentro de la lista que representa el ICER y el ROI
  #Preparamos la tabla MAIN 
  tabla_main <- data.frame(
    Categorias = c("1"),
    Valor = c(0),
    Valor_descontado = c(0)
   )
  
  #Modificado 23/12 -->>>
  #Formateamos cada valor de la tabla
  func_format <- c(
    formatear_epi
  )
  tabla_main$Valor <- mapply(
    function(f, v) {
      num <- suppressWarnings(as.numeric(v))
        if (!is.na(num)) {
          f(num)
        } else {
          v
        }
      },
    func_format,
    tabla_main$Valor
  )
  tabla_main$Valor_descontado <- mapply(
    function(f, v) {
      num <- suppressWarnings(as.numeric(v))
      if (!is.na(num)) {
        f(num)
      } else {
        v
      }
    },
    func_format,
    tabla_main$Valor_descontado
  )
  #Renombramos headers
  colnames(tabla_main) <- c("Indicador",
                             "Valor",
                             "Valor Descontado")
  #Devolvemos resultados
  resultado = list(
    indicadores = indicadores,
    tablaCostos = tabla_costos,
    #tablaSanitaria = tabla_sanitaria,
    #tablaDalys = tabla_dalys,
    tablaMain = tabla_main
    #infoICER = ifelse(any(incersInfo != ""), incersInfo[incersInfo != ""][1], ""),
    #infoROI = roi$info
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
  cohorteBasal <- distribuirPoblacion(cohortes, parametros, basal = 1)
  cohorteProyectada <- distribuirPoblacion(cohortes, parametros, basal = 0)
  costosMensualesDrogas <- estimarCostosDrogas(parametros)
  tratamientosDuraciones <- estimarTiempos(parametros)
  eAdversos <- estimarEfectosAdversos(parametros)
  costosMensualesSubsecuentes <- estimarCostosSubsecuentes(parametros, costosMensualesDrogas$subsecuentes)
  
  costosBasal <- estimarCostos(cohorteBasal, costosMensualesDrogas, tratamientosDuraciones, eAdversos, costosMensualesSubsecuentes, parametros)
  costosProyectado <- estimarCostos(cohorteProyectada, costosMensualesDrogas, tratamientosDuraciones, eAdversos, costosMensualesSubsecuentes, parametros)
  

  
  
  
  
  #Inicia correr modelo
  resultadosBasal <- 0  
  resultadosProyectado <- 0  
  
  return(procesarResultados(resultadosBasal, resultadosProyectado))
}
estimarCostos <- function(cohorte, cDrogas, tDuraciones, eAdversos, cSubsecuentes, parametros) {
  
  estrategias <- c("AVE", "NIV", "BSC", "EVP", "QMTNR")
  rCostos <- list()
  
  for (año in 1:parametros$tHT) {
    peCostos <- list()
    for (e in estrategias) {
      Costos <- list()
      
      # Costos de Adquisición Primera Linea
      
      # Costos de Adminsitración Primera Linea
      
      # Costos de Drogas primera linea
      
      # Costos de Efectos Adversos
      
      # Costos de manejo de la enfermedad
      
      # Costos de Tratamientos subsecuentes
      
      # Costos totales
      
      peCostos[[e]] <- Costos
    }
    rCostos[[año]] <- peCostos
  }
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
  
  print(costosSubsecuentes)
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
    sobrevidaGlobal <- parametros[[paste0("tOS", e)]]
    
    tiemposTratamientos[[e]] <- list(
      dInduccion = duracionInduccion,
      iMantenimiento = inicioMantenimiento,
      dMantenimiento = duracionMantenimiento,
      sobrevida = sobrevidaGlobal
    )
    
    
  }
  print(tiemposTratamientos)
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

  costos_adquisicion$Induccion <- list()
  costos_adquisicion$Mantenimiento <- list()
  
  costos_adquisicion$Induccion$Avelumab <- primeraLinea$QMT
  costos_adquisicion$Mantenimiento$Avelumab <- primeraLinea$Avelumab
  
  costos_adquisicion$Induccion$BSC <- primeraLinea$QMT
  costos_adquisicion$Mantenimiento$BSC <- 0
  
  costos_adquisicion$Induccion$QMTNR <- primeraLinea$QMT
  costos_adquisicion$Mantenimiento$QMTNR <- 0
  
  costos_adquisicion$Induccion$EVP <- primeraLinea$EV + primeraLinea$Pembro
  costos_adquisicion$Mantenimiento$EVP <- primeraLinea$Pembro
  
  costos_adquisicion$Induccion$Nivo <- primeraLinea$Nivolumab + primeraLinea$QMT
  costos_adquisicion$Mantenimiento$Nivo <- primeraLinea$Nivolumab
  # ----------------- Costos de Administración ------------------------------------
  costos_administracion$Induccion <- list()
  costos_administracion$Mantenimiento <- list()
  
  costos_administracion$Induccion$Avelumab <- max(parametros$nNA_porCiclo_Gemcitabine, parametros$nNA_porCiclo_Cisplatino) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Cisplatino
  costos_administracion$Mantenimiento$Avelumab <- parametros$nNA_porCiclo_Avelumab * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Avelumab
  
  costos_administracion$Induccion$BSC <- max(parametros$nNA_porCiclo_Gemcitabine, parametros$nNA_porCiclo_Cisplatino) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Cisplatino
  costos_administracion$Mantenimiento$BSC <- 0
  
  costos_administracion$Induccion$QMTNR <- max(parametros$nNA_porCiclo_Gemcitabine, parametros$nNA_porCiclo_Cisplatino) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Cisplatino
  costos_administracion$Mantenimiento$QMTNR <- 0

  costos_administracion$Induccion$EVP <- max(parametros$nNA_porCiclo_Pembro, parametros$nNA_porCiclo_EV ) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_EV
  costos_administracion$Mantenimiento$EVP <- parametros$nNA_porCiclo_Pembro * parametros$cAdministracion * (365.25 / 7 / 12) / parametros$nCL_Pembro
  
  costos_administracion$Induccion$Nivo <- max(parametros$nNA_porCiclo_Gemcitabine, parametros$nNA_porCiclo_Cisplatino, parametros$nNA_porCiclo_Nivolumab) * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Cisplatino
  costos_administracion$Mantenimiento$Nivo <- parametros$nNA_porCiclo_Nivolumab * parametros$cAdministracion * (365.25 / 7 /12) / parametros$nCL_Nivolumab
  
  
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
