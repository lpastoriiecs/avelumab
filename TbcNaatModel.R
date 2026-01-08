
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

INFO_ROI_NOAPLICA <- "El retorno de inversión no fue estimado debido a que la intervención es menos costosa que el comparador."
ICER_C_SUP_IZQ <- "La intervención es menos costosa y menos efectiva, el valor presente representa el RCEI del comparador contra la intervención. Por tanto, la intervención será costo-efectiva si RCEI está por encima del umbral."

MAX_AÑOS_MUERTES <- 5


formatear_pesos <- function(x, decimales = 0) {
  if (is.numeric(x)){
  return(formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales))} else {
    return(x)
  }
}

formatear_pesos2 <- function(x, decimales = 0) {
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
  indicadores <- list(deltaExitosos = formatear_epi(proyectado$tratamientosExitosos - basal$tratamientosExitosos),
                      deltaMuertes = formatear_epi(proyectado$muertes - basal$muertes),
                      deltaLTFU = formatear_epi(proyectado$ltfu - basal$ltfu),
                      deltaLyLost = formatear_epi(proyectado$lyLost - basal$lyLost) , 
                      deltaCostoTesteo = formatear_pesos2(proyectado$costoTesteo - basal$costoTesteo),
                      deltaOtrosCostos = formatear_pesos2(proyectado$otrosCostos - basal$otrosCostos)
  )

  #Eventos sanitarios
  tabla_sanitaria <- data.frame(
    Categorias = c("Tratamientos Exitosos", "Muertes", "LTFU", "Tratamientos Innecesarios", "Años Perdidos", "Disutilidad", "Dalys"),
    Escenario_Actual = c(basal$tratamientosExitosos, basal$muertes, basal$ltfu, basal$tratamientosInnecesarios, basal$lyLost, basal$disLost, basal$dalys),
    Escenario_Proyectado = c(proyectado$tratamientosExitosos, proyectado$muertes, proyectado$ltfu, proyectado$tratamientosInnecesarios, proyectado$lyLost, proyectado$disLost, proyectado$dalys),
    Diferencia =  c(proyectado$tratamientosExitosos - basal$tratamientosExitosos, proyectado$muertes - basal$muertes, proyectado$ltfu - basal$ltfu, proyectado$tratamientosInnecesarios - basal$tratamientosInnecesarios, proyectado$lyLost - basal$lyLost, proyectado$disLost - basal$disLost, proyectado$dalys - basal$dalys)
  )
  #Formateamos la tabla
  tabla_sanitaria[] <- lapply(tabla_sanitaria, function(col) {
    if (is.numeric(col)) formatear_epi(col) else col
  })
  #Nombramos los headers
  colnames(tabla_sanitaria) <- c("Categorias", 
                                 "Comparador", 
                                 "Intervención", 
                                 "Diferencia")  
  
  #Resultados Costos
  tabla_costos <- data.frame(
    Categorias = c("Costos de Testeo", "Otros Costos", "Costos Totales",  "dOtros Costos", "dCostos Totales"),
    Escenario_Actual = c(basal$costoTesteo, basal$otrosCostos, basal$costoTotal, basal$dOtrosCostos, basal$dCostoTotal),
    Escenario_Proyectado = c(proyectado$costoTesteo, proyectado$otrosCostos, proyectado$costoTotal, proyectado$dOtrosCostos, proyectado$dCostoTotal),
    Diferencia =  c(proyectado$costoTesteo - basal$costoTesteo, proyectado$otrosCostos - basal$otrosCostos, proyectado$costoTotal - basal$costoTotal, proyectado$dOtrosCostos - basal$dOtrosCostos, proyectado$dCostoTotal - basal$dCostoTotal)
  )
  #Formateamos la tabla
  tabla_costos[] <- lapply(tabla_costos, function(col) {
    if (is.numeric(col)) formatear_pesos2(col) else col
  })
  #Nombramos Headers
  colnames(tabla_costos) <- c("Categorias", 
                                 "Comparador", 
                                 "Intervención", 
                                 "Diferencia")
  
  
  #Resultados DALYS
  tabla_dalys <- data.frame(
    Categorias = c("Años de vida perdidos por muerte prematura", "Años de vida perdidos por muerte prematura (d)", "Años de vida Ajustados por Discapacidad por TBC", "Años de vida Ajustados por Discapacidad por vivir con TBC (d)", "Años de Vida Ajustados por Discapacidad", "Años de Vida Ajustados por Discapacidad (d)"),
    Escenario_Actual = c(basal$lyLost, basal$dLyLost, basal$disLost, basal$dDisLost, basal$dalys, basal$dDalys),
    Escenario_Proyectado = c(proyectado$lyLost, proyectado$dLyLost, proyectado$disLost, proyectado$dDisLost, proyectado$dalys, proyectado$dDalys),
    Diferencia =  c(proyectado$lyLost - basal$lyLost, proyectado$dLyLost - basal$dLyLost, proyectado$disLost - basal$disLost, proyectado$dDisLost - basal$dDisLost,  proyectado$dalys - basal$dalys, proyectado$dDalys - basal$dDalys)
  )
  #Formateamos la tabla
   tabla_dalys[] <- lapply(tabla_dalys, function(col) {
     if (is.numeric(col)) formatear_epi(col) else col
   })
  #Nombramos headers
  colnames(tabla_dalys) <- c("Categorias",
                              "Comparador",
                              "Intervención",
                              "Diferencia")
  
  
  #Calculamos delta costos para estimar ICERS y ROI
  costo_total_intervencion <- proyectado$costoTesteo + proyectado$costoProgramatico
  diferencia_otros_costos <- basal$otrosCostos - proyectado$otrosCostos
  diferencia_costos <- (costo_total_intervencion - basal$costoTesteo) - diferencia_otros_costos
  
  dDiferencia_otros_costos <- basal$dOtrosCostos - proyectado$dOtrosCostos
  dDiferencia_costos <- (costo_total_intervencion - basal$costoTesteo) - diferencia_otros_costos

  inversion <- costo_total_intervencion - basal$costoTesteo
  #Modificado 23/12 <<<--
  
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
  roi <- estimarRoi(inversion, diferencia_otros_costos)
  dRoi <- estimarRoi(inversion, dDiferencia_otros_costos)
  
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
    
    #Estima los ICERS.
    icer_añosVida <- interpretacionDecision(diferencia_costos, (basal$lyLost - proyectado$lyLost))
    icer_añosVidaDesc <- interpretacionDecision(dDiferencia_costos, (basal$dLyLost - proyectado$dLyLost))
    icer_dalys <- interpretacionDecision(diferencia_costos, basal$dalys - proyectado$dalys)
    icer_dalysDesc <- interpretacionDecision(dDiferencia_costos, basal$dDalys - proyectado$dDalys)
  
    
  #[ROI/ICER] en la tabla ahora mostramos el objeto valor dentro de la lista que representa el ICER y el ROI
  #Preparamos la tabla MAIN 
  tabla_main <- data.frame(
    Categorias = c("Tratamientos Exitosos Incrementales", "Muertes Evitadas", "Perdidas de seguimiento Evitadas", "Tratamientos Innecesarios Evitados", "Años de Vida Salvados", "Años de Vida Ajustados por Discapacidad Evitados", "Costo Total de la Intervención (USD)", "Costos Evitados atribuibles a la intervención (USD)", "Diferencia de costos respecto al escenario basal (USD)", "Razón de costo-efectividad incremental por año de vida salvado (USD)", "Razón de costo-efectividad incremental por año de vida ajustado por discapacidad evitado (USD)", "Retorno de Inversión (%)"),
    Valor = c((proyectado$tratamientosExitosos - basal$tratamientosExitosos), (proyectado$muertes - basal$muertes), (basal$ltfu - proyectado$ltfu), (basal$tratamientosInnecesarios - proyectado$tratamientosInnecesarios), (basal$lyLost - proyectado$lyLost), (basal$dalys - proyectado$dalys), 
              costo_total_intervencion, diferencia_otros_costos, diferencia_costos, icer_añosVida$valor, icer_dalys$valor , roi$valor),
    Valor_descontado = c("-", "-", "-", "-", (basal$dLyLost - proyectado$dLyLost), (basal$dDalys - proyectado$dDalys),
                         "-", dDiferencia_otros_costos, dDiferencia_costos, icer_añosVidaDesc$valor, icer_dalysDesc$valor, dRoi$valor)
   )
  
  #Modificado 23/12 -->>>
  #Formateamos cada valor de la tabla
  func_format <- c(
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_epi,
    formatear_pesos2,
    formatear_pesos2,
    formatear_pesos2,
    formatear_pesos2,
    formatear_pesos2,
    formatear_porcentaje
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
  
  #Modificado 23/12 <<<--
  #[ROI/ICER] devolvemos infoICER cuando alguno de los ICER tiene el problema en cuestión e infoROI
  incersInfo <-  c(icer_dalys$info, icer_dalysDesc$info, icer_añosVida$info, icer_añosVidaDesc$info)
  #Devolvemos resultados
  resultado = list(
    indicadores = indicadores,
    tablaCostos = tabla_costos,
    tablaSanitaria = tabla_sanitaria,
    tablaDalys = tabla_dalys,
    tablaMain = tabla_main,
    infoICER = ifelse(any(incersInfo != ""), incersInfo[incersInfo != ""][1], ""),
    infoROI = roi$info
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

correrFuncion <- function() {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Función dummy, carga valores del excel, seleciona argentina y corre el modelo%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
  
  parametros <- cargar()
  
  resultados <- correrModelo(parametros[['ARGENTINA']])
  
  return(resultados)
}
correrModelo <- function(parametros) {

  
  calcularResultados <- function(outcomesSensible, outcomesSensiblesNAAT, outcomesMDR, outcomesMDRNAAT, pNAAT, parametros) {
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Calcula eventos, dalys y costos por escenario%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #Leandro Pastori - 12/25 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #Ajustamos sospechosos y confirmados por el porcentaje de TBC Pulmonar de los confirmados
    #Estamos asumiendo que la distribución de Pulmonar - No pulmonar es igual en los confirmados que en los sospechosos
    sospechosos <- parametros$nSospechosos * parametros$pTBCPulmonar
    
    confirmados <- parametros$nCasosConfirmados * parametros$pTBCPulmonar
    
    #Estimamos la prevalencia de Tuberculosis en los casos sospechosos reportados.
    pTBC <- confirmados / sospechosos

    ##################Sin NAAT#############################################
    sospechosos_sinNAAT <- sospechosos * (1 - pNAAT)
    
    confirmados_sinNAAT <- confirmados * (1 - pNAAT)
    
    costosTesteoBCP <- sospechosos_sinNAAT * (parametros$nBaciloscopias * parametros$cBCP + parametros$cCultivo) + confirmados_sinNAAT * parametros$cAntibiograma
    

    desenlacesBCP <- estimarDesenlaces(sospechosos = sospechosos_sinNAAT, confirmados = confirmados_sinNAAT, sensibilidad = parametros$pBCPSensibilidad, especificidad = parametros$pBCPEspecificidad, sensibilidadResistencia = 0, especificidadResistencia = 1, pEmpirico = parametros$pTtoEmpiricoBCP, pResistencia = parametros$pResistencia, pPreXDR = parametros$pPreXDR)
    

    costosBCP <- estimarPayoffsCostos(desenlacesBCP, parametros, outcomesSensible$pMuerte, outcomesMDR$pMuerte, tasaDescuento = 0)

    dCostosBCP <- estimarPayoffsCostos(desenlacesBCP, parametros, outcomesSensible$pMuerte, outcomesMDR$pMuerte, tasaDescuento = parametros$rTasaDescuento)
    
    disBCP <- estimarPayoffsDalys(desenlacesBCP, parametros, outcomesSensible$pMuerte, outcomesMDR$pMuerte, tasaDescuento = 0)

    dDisBCP <- estimarPayoffsDalys(desenlacesBCP, parametros, outcomesSensible$pMuerte, outcomesMDR$pMuerte, tasaDescuento = parametros$rTasaDescuento)
    
    muertesBCP <- confirmados_sinNAAT * (((1 - parametros$pResistencia) * outcomesSensible$pMuerte) + (parametros$pResistencia * outcomesMDR$pMuerte))
    
    lyBCP <- muertesBCP * (parametros$tExpectativaVidaEdadTBC - parametros$tEdadMediaTBC)
    
    dalysBCP <- lyBCP + disBCP$Total
    
    dLyBCP <- muertesBCP * descontar(tasa = parametros$rTasaDescuento, años = (parametros$tExpectativaVidaEdadTBC - parametros$tEdadMediaTBC))
    
    dDalysBCP <- dLyBCP + dDisBCP$Total
    
    tratamientosExitososBCP <- confirmados_sinNAAT * ((1 - parametros$pResistencia) * outcomesSensible$pTtoExitoso + parametros$pResistencia * outcomesMDR$pTtoExitoso)
    
    tratamientosInnecesariosBCP <- desenlacesBCP$innecesarios + desenlacesBCP$innecesariosMDR
    
    ltfuBCP <- confirmados_sinNAAT - (tratamientosExitososBCP + muertesBCP)
    
    sospechosos_conNAAT <- sospechosos * pNAAT
    
    confirmados_conNAAT <- confirmados * pNAAT
    
    costosTesteoNAAT <- sospechosos_conNAAT * (parametros$cNAAT + parametros$cCultivo) + confirmados_conNAAT * parametros$cAntibiograma
    
    desenlacesNAAT <- estimarDesenlaces(sospechosos = sospechosos_conNAAT, confirmados = confirmados_conNAAT, sensibilidad = parametros$pNAATSensibilidad, especificidad = parametros$pNAATEspecificidad, sensibilidadResistencia = parametros$pNAATRifSensibilidad, especificidadResistencia = parametros$pNAATRifEspecificidad, pEmpirico = parametros$pTtoEmpiricoNAAT, pResistencia = parametros$pResistencia, pPreXDR = parametros$pPreXDR)
    
    costosNAAT <- estimarPayoffsCostos(desenlacesNAAT, parametros, outcomesSensiblesNAAT$pMuerte, outcomesMDRNAAT$pMuerte, tasaDescuento = 0)

    dCostosNAAT <- estimarPayoffsCostos(desenlacesNAAT, parametros, outcomesSensiblesNAAT$pMuerte, outcomesMDRNAAT$pMuerte, tasaDescuento = parametros$rTasaDescuento)
        
    disNAAT <- estimarPayoffsDalys(desenlacesNAAT, parametros, outcomesSensiblesNAAT$pMuerte, outcomesMDRNAAT$pMuerte, tasaDescuento = 0)
    
    dDisNAAT <- estimarPayoffsDalys(desenlacesNAAT, parametros, outcomesSensiblesNAAT$pMuerte, outcomesMDRNAAT$pMuerte, tasaDescuento = parametros$rTasaDescuento)
    
    muertesNAAT <- confirmados_conNAAT * (((1 - parametros$pResistencia) * outcomesSensiblesNAAT$pMuerte) + (parametros$pResistencia * outcomesMDRNAAT$pMuerte))

    lyNAAT <- muertesNAAT * (parametros$tExpectativaVidaEdadTBC - parametros$tEdadMediaTBC)
    
    dLyNAAT <- muertesNAAT * descontar(tasa = parametros$rTasaDescuento, años = (parametros$tExpectativaVidaEdadTBC - parametros$tEdadMediaTBC))

    dalysNAAT <- lyNAAT + disNAAT$Total
    
    dDalysNAAT <- dLyNAAT + dDisNAAT$Total
    
    tratamientosExitososNAAT <- confirmados_conNAAT * ((1 - parametros$pResistencia) * outcomesSensiblesNAAT$pTtoExitoso + parametros$pResistencia * outcomesMDRNAAT$pTtoExitoso)
    
    ltfuNAAT <- confirmados_conNAAT - (tratamientosExitososNAAT + muertesNAAT)
    
    tratamientosInnecesariosNAAT <- desenlacesNAAT$innecesarios + desenlacesNAAT$innecesariosMDR
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    return(list(
      tratamientosExitosos = tratamientosExitososBCP + tratamientosExitososNAAT,
      tratamientosInnecesarios = tratamientosInnecesariosBCP + tratamientosInnecesariosNAAT,
      muertes = muertesNAAT + muertesBCP,
      ltfu = ltfuBCP + ltfuNAAT,
      lyLost = lyBCP + lyNAAT,
      disLost = disBCP$Total + disNAAT$Total,
      dalys = dalysBCP + dalysNAAT,
      dLyLost = dLyBCP + dLyNAAT,
      dDisLost = dDisBCP$Total + dDisNAAT$Total,
      dDalys = dDalysBCP + dDalysNAAT,
      otrosCostos = costosBCP$Total + costosNAAT$Total,
      dOtrosCostos = dCostosBCP$Total + dCostosNAAT$Total,
      costoTesteo = costosTesteoBCP + costosTesteoNAAT,
      costoTotal = costosTesteoBCP + costosTesteoNAAT + costosBCP$Total + costosNAAT$Total,
      dCostoTotal = costosTesteoBCP + costosTesteoNAAT + dCostosBCP$Total + dCostosNAAT$Total
    ))
      

    
  }

  #Inicia correr modelo
  
  
  
  #Ajustamos outcome según el porcentaje de MDR y los outcome de MDR.
  outcomeSensible <- ajustarOutcomePorMDR(pTtoExitoso = parametros$pTtoExitoso, pMuerte = parametros$pMuerte, pTtoExitosoMDR =  parametros$pTtoExitosoMDR, pMuerteMDR = parametros$pMuerteMDR, pResistencia = parametros$pResistencia)
  #Ajustamos outcomes del pais según el porcentaje de uso basal de NAAT
  outcomeSensibleSinNAAT <- ajustarOutcomePorNAAT(outcomeSensible$pTtoExitoso, outcomeSensible$pMuerte, parametros$rRRMuerteNAAT, parametros$rORExitosoNAAT, pTesteadosNAAT = parametros$pTesteadosNAAT, pTesteadosNAATObj = 0)
  
  #Ajustamos outcomes del pais según el porcentaje de uso proyectado de NAAT
  outcomeSensibleNAAT <- ajustarOutcomePorNAAT(outcomeSensible$pTtoExitoso, outcomeSensible$pMuerte, parametros$rRRMuerteNAAT, parametros$rORExitosoNAAT, pTesteadosNAAT = parametros$pTesteadosNAAT, pTesteadosNAATObj = 1)
  #Ajustamos outcomes MDR del pais según el porcentaje de uso basal de NAAT
  outcomeMDRSinNAAT  <- ajustarOutcomePorNAAT(parametros$pTtoExitosoMDR, parametros$pMuerteMDR, parametros$rRRMuerteNAAT, parametros$rORExitosoNAAT, parametros$pTesteadosNAAT, 0)

  #Ajustamos outcomes MDR del pais según el porcentaje de uso basal de NAAT
  outcomeMDRNAAT  <- ajustarOutcomePorNAAT(parametros$pTtoExitosoMDR, parametros$pMuerteMDR, parametros$rRRMuerteNAAT, parametros$rORExitosoNAAT, parametros$pTesteadosNAAT, pTesteadosNAATObj = 1)
  
  resultadosBasal <- calcularResultados(outcomeSensibleSinNAAT, outcomeSensibleNAAT, outcomeMDRSinNAAT, outcomeMDRNAAT, parametros$pTesteadosNAAT, parametros)
  
  resultadosProyectado <- calcularResultados(outcomeSensibleSinNAAT, outcomeSensibleNAAT, outcomeMDRSinNAAT, outcomeMDRNAAT, parametros$pTesteadosNAATObj, parametros)
  
  resultadosProyectado$costoProgramatico <- parametros$cCostoProgramatico
  
  return(procesarResultados(resultadosBasal, resultadosProyectado))

  
}
estimarPayoffsCostos <- function(desenlaces, parametros, pMuerte, pMuerteMDR, tasaDescuento) {
  

  #Costos
  #innecesarios - Pacientes sin tuberculosis que reciben tratamiento
  #hasta cultivo negativo.
  
  #Costos de tratamiento y seguimiento de los pacientes innecesarios.
  cInnecesarios <- desenlaces$innecesarios * estimarCostoTTO(costoTTO =  parametros$cTratamiento, costoSTO =  parametros$cSeguimientoTBC, meses =  parametros$tCultivoNegativo, inicio = 0, descuento = tasaDescuento)
  
  #innecesariosMDR - Pacientes sin tuberculosis que son catalogados como TBC MDR
  cInnecesariosMDR <- desenlaces$innecesariosMDR * estimarCostoTTO(costoTTO =  parametros$cTratamientoMDR, costoSTO =   parametros$cSeguimientoTBCMDR, meses = parametros$tCultivoNegativo, inicio = 0, descuento = tasaDescuento)
  
  #adecuadosSensibles - Pacientes con tuberculosis que son catalogados como TBC sensibles
  cAdecuadosSensiblesTTO <- desenlaces$adecuadosSensibles * estimarCostoTTO(costoTTO = parametros$cTratamiento, costoSTO = parametros$cSeguimientoTBC, meses = parametros$tTtoSensibles * (1 - pMuerte) + pMuerte * parametros$tMuerte, inicio = 0, descuento = tasaDescuento)
  
  cAdecuadosSensiblesTBC <- desenlaces$adecuadosSensibles * estimarCostoTBC(costoConsulta =  parametros$cConsulta, cantEspConsulta =  parametros$ceConsulta, costoHosp =  parametros$cHospitalizacion, pHosp =  parametros$pHospitalizacion, diasHosp =  parametros$tDiasHospTBC, meses =  parametros$tTtoSensibles * (1 - pMuerte) + pMuerte * parametros$tMuerte, inicio = 0, descuento = tasaDescuento)

  cAdecuadosSensibles <- cAdecuadosSensiblesTTO + cAdecuadosSensiblesTBC
  
  #inadecuadosSensibles - Pacientes con tuberculosis falsamente interpretados como MDR
  
  cInadecuadosSensiblesTTO <- desenlaces$inadecuadosSensibles * (estimarCostoTTO(costoTTO =  parametros$cTratamientoMDR, costoSTO =  parametros$cSeguimientoTBCMDR, meses = parametros$tResultadoResistencia, inicio = 0, descuento = tasaDescuento) +
                                                                estimarCostoTTO(costoTTO =  parametros$cTratamiento, costoSTO =  parametros$cSeguimientoTBC, meses = (parametros$tTtoSensibles - parametros$tResultadoResistencia) * (1 - pMuerte) + pMuerte * parametros$tMuerte, inicio = parametros$tResultadoResistencia, descuento = tasaDescuento))
  

  cInadecuadosSensiblesTBC <- desenlaces$inadecuadosSensibles * estimarCostoTBC( costoConsulta =  parametros$cConsulta, cantEspConsulta =  parametros$ceConsulta, costoHosp =  parametros$cHospitalizacion , pHosp =  parametros$pHospitalizacion ,diasHosp =  parametros$tDiasHospTBC, meses = parametros$tResultadoResistencia + ((parametros$tTtoSensibles - parametros$tResultadoResistencia) * (1 - pMuerte) + pMuerte * parametros$tMuerte), inicio = 0, descuento = tasaDescuento)
  

  cInadecuadosSensibles <- cInadecuadosSensiblesTTO + cInadecuadosSensiblesTBC
  
  #adecuadosResistentes 
  cAdecuadosResistentesTTO <- desenlaces$adecuadosResistentes * estimarCostoTTO(costoTTO =  parametros$cTratamientoMDR, costoSTO =  parametros$cSeguimientoTBCMDR, meses = parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR, inicio = 0, descuento = tasaDescuento)


  cAdecuadosResistentesTBC <- desenlaces$adecuadosResistentes *  estimarCostoTBC(costoConsulta =  parametros$cConsulta, cantEspConsulta =  parametros$ceConsultaMDR, costoHosp = parametros$cHospitalizacion , pHosp =  parametros$pHospitalizacionMDR, diasHosp =  parametros$tDiasHospTBCMDR, meses = parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR, inicio = 0, descuento = tasaDescuento)

  cAdecuadosResistentes <- cAdecuadosResistentesTTO + cAdecuadosResistentesTBC
  #adecuadosResistentesPreXDR Pacientes con TBC MDR PreXDR que se diagnostican como MDR,
  #reciben tratamiento MDR hasta resultado de la resistencia y luego siguen como preXDR.
  cAdecuadosResistentesPreXDRTTO <- desenlaces$adecuadosResistentesPreXDR * (estimarCostoTTO(costoTTO = parametros$cTratamientoMDR, costoSTO = parametros$cSeguimientoTBCMDR, meses = parametros$tResultadoResistencia, inicio = 0, descuento = tasaDescuento) + 
                                                                             estimarCostoTTO(costoTTO = parametros$cTratamientoPreXDR, costoSTO = parametros$cSeguimientoTBCMDR, meses = (parametros$tTtoPreXDR - parametros$tResultadoResistencia) * ( 1- pMuerteMDR) + parametros$tMuerte * pMuerteMDR , inicio = parametros$tResultadoResistencia, descuento = tasaDescuento))
  

  cAdecuadosResistentesPreXDRTBC <- desenlaces$adecuadosResistentesPreXDR * estimarCostoTBC(costoConsulta = parametros$cConsulta, cantEspConsulta = parametros$ceConsultaMDR, costoHosp = parametros$cHospitalizacion, pHosp = parametros$pHospitalizacionMDR, diasHosp = parametros$tDiasHospTBCMDR, meses = parametros$tResultadoResistencia + ((parametros$tTtoPreXDR - parametros$tResultadoResistencia) * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = 0, descuento = tasaDescuento)

  cAdecuadosResistentesPreXDR <- cAdecuadosResistentesPreXDRTTO + cAdecuadosResistentesPreXDRTBC
  #inadecuadosResistentes - Pacientes con TBC diagnosticados como sensible pero que son MDR 
  
  cInadecuadosResistentesTTO <- desenlaces$inadecuadosResistentes * (
    estimarCostoTTO(costoTTO = parametros$cTratamiento, costoSTO = parametros$cSeguimientoTBC, meses = parametros$tResultadoResistencia, inicio = 0, descuento = tasaDescuento) +
    estimarCostoTTO(costoTTO = parametros$cTratamientoMDR, costoSTO = parametros$cSeguimientoTBCMDR, meses = parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR, inicio = parametros$tResultadoResistencia, descuento = tasaDescuento) +
    parametros$pHospMDRCambio * parametros$tDiasHospTBCMDRATB * parametros$cHospitalizacion
  )

  cInadecuadosResistentesTBC <- desenlaces$inadecuadosResistentes * estimarCostoTBC(costoConsulta = parametros$cConsulta, cantEspConsulta = parametros$ceConsultaMDR, pHosp = parametros$pHospitalizacionMDR, costoHosp = parametros$cHospitalizacion, diasHosp = parametros$tDiasHospTBCMDR, meses = parametros$tResultadoResistencia + (parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = 0, descuento = tasaDescuento)

  cInadecuadosResistentes <- cInadecuadosResistentesTTO + cInadecuadosResistentesTBC
  
  #inadecuadosResistentesPreXDR - Pacientes con TBC diagnosticados como sensible pero que son Pre XDR
  cInadecuadosResistentesPreXDRTTO <- desenlaces$inadecuadosResistentesPreXDR * (
    estimarCostoTTO(costoTTO = parametros$cTratamiento, costoSTO = parametros$cSeguimientoTBC, meses = parametros$tResultadoResistencia, inicio = 0, descuento = tasaDescuento) +
      estimarCostoTTO(costoTTO = parametros$cTratamientoPreXDR, costoSTO = parametros$cSeguimientoTBCMDR, meses = parametros$tTtoPreXDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR, inicio = parametros$tResultadoResistencia, descuento = tasaDescuento) +
      parametros$pHospMDRCambio * parametros$tDiasHospTBCMDRATB * parametros$cHospitalizacion
  )
  cInadecuadosResistentesPreXDRTBC <- desenlaces$inadecuadosResistentesPreXDR * estimarCostoTBC(costoConsulta = parametros$cConsulta, cantEspConsulta = parametros$ceConsultaMDR, pHosp = parametros$pHospitalizacionMDR, costoHosp = parametros$cHospitalizacion, diasHosp = parametros$tDiasHospTBCMDR, meses = parametros$tResultadoResistencia + (parametros$tTtoPreXDR * (1-pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = 0, descuento = tasaDescuento)

  cInadecuadosResistentesPreXDR <- cInadecuadosResistentesPreXDRTTO + cInadecuadosResistentesPreXDRTBC
  
  
  #tardiosSensibles - Pacientes que se diagnostican tardiamente con TBC sensible.
  cTardiosSensiblesTTO <- desenlaces$tardiosSensibles * estimarCostoTTO(costoTTO = parametros$cTratamiento, costoSTO = parametros$cSeguimientoTBC, meses = parametros$tTtoSensibles * (1 - pMuerte) + parametros$tMuerte * pMuerte, inicio = parametros$tCultivoPositivo, descuento = tasaDescuento)
  cTardiosSensiblesTBC <- desenlaces$tardiosSensibles * estimarCostoTBC(costoConsulta = parametros$cConsulta, cantEspConsulta = parametros$ceConsulta, costoHosp = parametros$cHospitalizacion, pHosp = parametros$pHospitalizacion, diasHosp = parametros$tDiasHospTBC, meses = (parametros$tTtoSensibles * (1 - pMuerte) + parametros$tMuerte * pMuerte) + parametros$tCultivoPositivo, inicio = 0, descuento = tasaDescuento)


  cTardiosSensibles <- cTardiosSensiblesTTO + cTardiosSensiblesTBC
  #tardiosMDR - Pacientes que se diagnostican tardiamente y luego resultan MDR
  
  cTardiosMDRTTO <- desenlaces$tardiosMDR * (
    estimarCostoTTO(costoTTO = parametros$cTratamiento, costoSTO = parametros$cSeguimientoTBC, meses = parametros$tResultadoResistencia - parametros$tCultivoPositivo, inicio = parametros$tCultivoPositivo, descuento = tasaDescuento) +
    estimarCostoTTO(costoTTO = parametros$cTratamientoMDR, costoSTO = parametros$cSeguimientoTBCMDR, meses = parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR, inicio = parametros$tResultadoResistencia, descuento = tasaDescuento) +
    parametros$pHospMDRCambio * parametros$tDiasHospTBCMDRATB * parametros$cHospitalizacion
  )  
  cTardiosMDRTBC <- desenlaces$tardiosMDR * estimarCostoTBC(costoConsulta = parametros$cConsulta, cantEspConsulta = parametros$ceConsultaMDR, pHosp = parametros$pHospitalizacionMDR, costoHosp = parametros$cHospitalizacion, diasHosp = parametros$tDiasHospTBCMDR, meses = parametros$tResultadoResistencia + (parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = 0, descuento = tasaDescuento)

  cTardiosMDR <- cTardiosMDRTTO + cTardiosMDRTBC
  
  #tardiosPreXDR - Pacientes que se diagnostican tardiamente y luego resultan MDR PreXDR
  cTardiosPreXDRTTO <- desenlaces$tardiosPreXDR * (
    estimarCostoTTO(costoTTO = parametros$cTratamiento, costoSTO = parametros$cSeguimientoTBC, meses = parametros$tResultadoResistencia - parametros$tCultivoPositivo, inicio = parametros$tCultivoPositivo, descuento = tasaDescuento) +
      estimarCostoTTO(costoTTO = parametros$cTratamientoPreXDR, costoSTO = parametros$cSeguimientoTBCMDR, meses = parametros$tTtoPreXDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR, inicio = parametros$tResultadoResistencia, descuento = tasaDescuento) +
      parametros$pHospMDRCambio * parametros$tDiasHospTBCMDRATB * parametros$cHospitalizacion
  )  
  cTardiosPreXDRTBC <- desenlaces$tardiosPreXDR * estimarCostoTBC(costoConsulta = parametros$cConsulta, cantEspConsulta = parametros$ceConsultaMDR, pHosp = parametros$pHospitalizacionMDR, costoHosp = parametros$cHospitalizacion, diasHosp = parametros$tDiasHospTBCMDR, meses = parametros$tResultadoResistencia + (parametros$tTtoPreXDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = 0, descuento = tasaDescuento)

  
  cTardiosPreXDR <- cTardiosPreXDRTTO + cTardiosPreXDRTBC
  
  Total <- cInnecesarios + cInnecesariosMDR + cAdecuadosSensibles + cInadecuadosSensibles + cAdecuadosResistentes + cAdecuadosResistentesPreXDR + cInadecuadosResistentes + cInadecuadosResistentesPreXDR + cTardiosSensibles + cTardiosMDR + cTardiosPreXDR

  return(
    list(
      cInnecesarios = cInnecesarios,
      cInncesariosMDR = cInnecesariosMDR,
      cAdecuadosSensibles = cAdecuadosSensibles,
      cInadecuadosSensibles = cInadecuadosSensibles,
      cAdecuadosResistentes = cAdecuadosResistentes,
      cAdecuadosResistentesPreXDRTBC = cAdecuadosResistentesPreXDR,
      cInadecuadosResistentes = cInadecuadosResistentes,
      cInadecuadosResistentesPreXDR = cInadecuadosResistentesPreXDR,
      cTardiosSensibles = cTardiosSensibles,
      cTardiosMDR = cTardiosMDR,
      cTardiosPreXDR = cTardiosPreXDR,
      Total = Total
    )
  )
}
estimarPayoffsDalys <- function(desenlaces, parametros, pMuerte, pMuerteMDR, tasaDescuento) {
  
  #Costos
  #innecesarios - Pacientes sin tuberculosis que reciben tratamiento
  #hasta cultivo negativo.
  #Costos de tratamiento y seguimiento de los pacientes innecesarios.
  uInnecesarios <- desenlaces$innecesarios * estimarDisutilidad(valor =  parametros$uSANOTratado, meses =  parametros$tCultivoNegativo, inicio = 0, descuento = tasaDescuento)
  
  #innecesariosMDR - Pacientes sin tuberculosis que son catalogados como TBC MDR
  uInnecesariosMDR <- desenlaces$innecesariosMDR * estimarDisutilidad(valor = parametros$uSANOTratado , meses = parametros$tCultivoNegativo, inicio = 0, descuento = tasaDescuento)
  
  #adecuadosSensibles - Pacientes con tuberculosis que son catalogados como TBC sensibles
  uAdecuadosSensibles <- desenlaces$adecuadosSensibles * estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tTtoSensibles * (1 - pMuerte) + parametros$tMuerte * pMuerte), inicio = 0, descuento = tasaDescuento)
  

  #inadecuadosSensibles - Pacientes con tuberculosis falsamente interpretados como MDR
  uInadecuadosSensibles <- desenlaces$inadecuadosSensibles * estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tResultadoResistencia + ((parametros$tTtoSensibles - parametros$tResultadoResistencia) * (1 - pMuerte) + parametros$tMuerte * pMuerte)), inicio = 0, descuento = tasaDescuento)

  #adecuadosResistentes 
  uAdecuadosResistentes <- desenlaces$adecuadosResistentes * estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = 0, descuento = tasaDescuento)

  #adecuadosResistentesPreXDR Pacientes con TBC MDR PreXDR que se diagnostican como MDR,
  #reciben tratamiento MDR hasta resultado de la resistencia y luego siguen como preXDR.
  uAdecuadosResistentesPreXDR <- desenlaces$adecuadosResistentesPreXDR * estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tResultadoResistencia + ((parametros$tTtoPreXDR - parametros$tResultadoResistencia) * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR)), inicio = 0, descuento = tasaDescuento)

  #inadecuadosResistentes - Pacientes con TBC diagnosticados como sensible pero que son MDR 
  uInadecuadosResistentes <- desenlaces$inadecuadosResistentes * (
    estimarDisutilidad(valor = parametros$uTBCNoTratada, meses = parametros$tResultadoResistencia, inicio = 0, descuento = tasaDescuento) + 
    estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = parametros$tResultadoResistencia, descuento = tasaDescuento)
  )

  
  #inadecuadosResistentesPreXDR - Pacientes con TBC diagnosticados como sensible pero que son Pre XDR
  uInadecuadosResistentesPreXDR <- desenlaces$inadecuadosResistentesPreXDR * (
      estimarDisutilidad(valor = parametros$uTBCNoTratada, meses = parametros$tResultadoResistencia, inicio = 0, descuento = tasaDescuento) +
      estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tTtoPreXDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = parametros$tResultadoResistencia, descuento = tasaDescuento)
  )
  
  #tardiosSensibles - Pacientes que se diagnostican tardiamente con TBC sensible.
  uTardiosSensibles <- desenlaces$tardiosSensibles * ( 
                        estimarDisutilidad(valor = parametros$uTBCNoTratada, meses = parametros$tCultivoPositivo, inicio = 0, descuento = tasaDescuento) +
                        estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tTtoSensibles * (1 - pMuerte) + parametros$tMuerte * pMuerte), inicio = parametros$tCultivoPositivo, descuento = tasaDescuento)
  )
  #tardiosMDR - Pacientes que se diagnostican tardiamente y luego resultan MDR
  uTardiosMDR <- desenlaces$tardiosMDR * (
    estimarDisutilidad(valor = parametros$uTBCNoTratada, meses = parametros$tResultadoResistencia, inicio = 0, descuento = tasaDescuento) +
    estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tTtoMDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = parametros$tResultadoResistencia, descuento = tasaDescuento)
  )  
  
  #tardiosPreXDR - Pacientes que se diagnostican tardiamente y luego resultan MDR PreXDR
  uTardiosPreXDR <- desenlaces$tardiosPreXDR * (
      estimarDisutilidad(valor = parametros$uTBCNoTratada, meses = parametros$tResultadoResistencia, inicio = 0, descuento = tasaDescuento) +
      estimarDisutilidad(valor = parametros$uTBCTratada, meses = (parametros$tTtoPreXDR * (1 - pMuerteMDR) + parametros$tMuerte * pMuerteMDR), inicio = parametros$tResultadoResistencia, descuento = tasaDescuento)
  )  
  
  Total <- uInnecesarios + uInnecesariosMDR + uAdecuadosSensibles + uInadecuadosSensibles + uAdecuadosResistentes + uAdecuadosResistentesPreXDR + uInadecuadosResistentes + uInadecuadosResistentesPreXDR + uTardiosSensibles + uTardiosMDR + uTardiosPreXDR
  
  return(
    list(
      uInnecesarios = uInnecesarios,
      uInnecesariosMDR = uInnecesariosMDR,
      uAdecuadosSensibles = uAdecuadosSensibles,
      uInadecuadosSensibles = uInadecuadosSensibles,
      uAdecuadosResistentes = uAdecuadosResistentes,
      uAdecuadosResistentesPreXDR = uAdecuadosResistentesPreXDR,
      uInadecuadosResistentes = uInadecuadosResistentes,
      uInadecuadosResistentesPreXDR = uInadecuadosResistentesPreXDR,
      uTardiosSensibles = uTardiosSensibles,
      uTardiosMDR = uTardiosMDR,
      uTardiosPreXDR = uTardiosPreXDR,
      Total = Total
    )
  )
}

estimarDisutilidad <- function(meses, valor, inicio, descuento) {
  
  if (meses + inicio <= 12 || descuento == 0) {
    res <- valor / 12 * meses
  } else {

    res <- valor / 12 * max(0, 12 - inicio)  + descontarMensual(tasa_anual = descuento, mes_inicio = max(13, inicio), duracion = meses - max(0, 12 - inicio), valor = valor / 12)
  }
  return(
    res
  )
}

estimarCostoTBC <- function(cantEspConsulta, costoConsulta, pHosp, diasHosp, costoHosp, meses, inicio, descuento) {
  
  
  valor <- (costoConsulta * cantEspConsulta) + (costoHosp * pHosp * diasHosp)
  if (meses + inicio <= 12 || descuento == 0)
  {
    res <- valor * meses
  } else {
    
    res <- valor * max(0, 12 - inicio)  + descontarMensual(tasa_anual = descuento, mes_inicio = max(13, inicio), duracion = meses - max(0, 12 - inicio), valor = valor)
    
  }
  return(res)
}
estimarCostoTTO <- function(costoTTO, costoSTO, meses, inicio, descuento) {
  valor <- (costoTTO + costoSTO)
  if (meses + inicio <= 12 || descuento == 0)
  {
    res <- valor * meses
    
  } else {
    
    res <- valor * max(0, 12 - inicio)  + descontarMensual(tasa_anual = descuento, mes_inicio = max(13, inicio), duracion = meses -  max(0, 12 - inicio), valor = valor)
    
  }
  return(res)
}
estimarDesenlaces <- function(sospechosos, confirmados, sensibilidad, especificidad, sensibilidadResistencia, especificidadResistencia = 1, pEmpirico, pResistencia, pPreXDR) {
  
  
  
  verdaderosPositivos <- confirmados * sensibilidad
  
  falsosNegativos <- confirmados * (1 - sensibilidad)
  
  falsosPositivos <- (sospechosos - confirmados) * (1 - especificidad)
  
  verdaderosNegativos <- (sospechosos - confirmados) * especificidad
  
  
  
  falsosNegativos_Empirico <- falsosNegativos * pEmpirico
  
  verdaderosNegativos_Empirico <- verdaderosNegativos * pEmpirico
  
  #Hay 2 grupos que reciben tratamiento al pedo falsosPositivos_sinNAAT y empiricosFP_sinNAAT
  #innecesarios <- verdaderosNegativos_Empirico + falsosPositivos * especificidadResistencia
  innecesarios <- 0
  #Falsos positivos que la resistencia le da positivo, si el test no mide resistencia hay que pasarle especificidad 1.
  innecesariosMDR <- falsosPositivos * (1 - especificidadResistencia)
  innecesariosMDR <- 0
  #Tuberculosis sensible diagnosticada y tuberculosis sensible bajo tratamiento empirico
  adecuadosSensibles <- verdaderosPositivos * (1 - pResistencia) * especificidadResistencia + falsosNegativos_Empirico * (1 - pResistencia)
  
  #Tuberculosis sensible a los que el test de resistencia les da positivo
  inadecuadosSensibles <- verdaderosPositivos * (1 - pResistencia) * (1 - especificidadResistencia)
  
  #Tuberculosis resistente que son detectados por el test
  adecuadosResistentes <- verdaderosPositivos * pResistencia * sensibilidadResistencia * (1 - (pPreXDR / pResistencia))
  
  #Tuberculosis que iniciaron tratamiento para MDR pero son PreXDR
  adecuadosResistentesPreXDR <- verdaderosPositivos * pResistencia * sensibilidadResistencia * pPreXDR / pResistencia
  
  #Son tuberculosis resistentes pero les dio negativa la sensibilidad
  inadecuadosResistentes <- verdaderosPositivos * pResistencia * (1 - sensibilidadResistencia) * (1 - (pPreXDR / pResistencia)) + falsosNegativos_Empirico * (1 - (pPreXDR / pResistencia)) * pResistencia

  #Son tuberculosis resistentes pero les dio negativa la sensibilidad
  inadecuadosResistentesPreXDR <- verdaderosPositivos * pResistencia * (1 - sensibilidadResistencia) * pPreXDR / pResistencia  + falsosNegativos_Empirico * pPreXDR / pResistencia * pResistencia
  
  #un grupo recibe tratamiento tardio, de ellos alguno será adecuado y otros inadecuado.
  tardios <- falsosNegativos - falsosNegativos_Empirico
  
  tardiosSensibles <- tardios * (1 - pResistencia)
  
  tardiosMDR <- tardios * pResistencia * (1 - (pPreXDR / pResistencia))
  
  tardiosPreXDR <- tardios * pPreXDR
  
  
  
  return (
    list(
      innecesarios = innecesarios,
      innecesariosMDR = innecesariosMDR,
      adecuadosSensibles = adecuadosSensibles,
      inadecuadosSensibles = inadecuadosSensibles,
      adecuadosResistentes = adecuadosResistentes,
      adecuadosResistentesPreXDR = adecuadosResistentesPreXDR,
      inadecuadosResistentes = inadecuadosResistentes,
      inadecuadosResistentesPreXDR = inadecuadosResistentesPreXDR,
      tardiosSensibles = tardiosSensibles,
      tardiosMDR = tardiosMDR,
      tardiosPreXDR = tardiosPreXDR
    )
  )
  
  
}
#cargarDatos()
ajustarOutcomePorMDR <- function(pTtoExitoso, pMuerte, pTtoExitosoMDR, pMuerteMDR, pResistencia) {
  res <-
    list(
      pTtoExitoso = (pTtoExitoso - (pTtoExitosoMDR * pResistencia)) / (1 - pResistencia),
      pMuerte = (pMuerte - (pMuerteMDR * pResistencia)) / (1 - pResistencia)
    )
  res$pLTFU = 1 - res$pTtoExitoso - res$pMuerte
  
  return(res)
  
}

ajustarOutcomePorNAAT <- function(pTtoExitoso, pMuerte, rRRMuerteNAAT, rORExitosoNAAT, pTesteadosNAAT, pTesteadosNAATObj) {
  crudo <- 
    list(
      pTtoExitoso = ((pTtoExitoso / (1 - pTtoExitoso)) / (pTesteadosNAAT * rORExitosoNAAT + (1 - pTesteadosNAAT)))  / (1 + ((pTtoExitoso / (1 - pTtoExitoso)) / (pTesteadosNAAT * rORExitosoNAAT + (1 - pTesteadosNAAT)))),
      pMuerte = pMuerte / (pTesteadosNAAT * rRRMuerteNAAT + (1 - pTesteadosNAAT))
    )
  
  res <- (
    list(
      pTtoExitoso = crudo$pTtoExitoso * (1 - pTesteadosNAATObj) +( ((crudo$pTtoExitoso / (1 - crudo$pTtoExitoso)) * rORExitosoNAAT) / (1 + ((crudo$pTtoExitoso / (1 - crudo$pTtoExitoso)) * rORExitosoNAAT))) * pTesteadosNAATObj,
      pMuerte = crudo$pMuerte * (1 - pTesteadosNAATObj) + crudo$pMuerte * rRRMuerteNAAT * pTesteadosNAATObj
    )
  )
  #Si nos pasamos de 1 ajustamos a 1, esto es por si ttoexitoso aumenta mucho y muerte no baja tanto.
  if (res$pTtoExitoso + res$pMuerte > 1) { 
      res$pTtoExitoso <- 1 - res$pMuerte
    }
  res$pLTFU <- 1 - res$pTtoExitoso - res$pMuerte
  return(res)
}


cargar <- function() {
    
  data <- read_excel("lparametros.xlsx", sheet = "parametros")

  parametros_paises <- list()
  lista_paises <- c("ARGENTINA", "BRASIL", "CHILE", "COLOMBIA", "ECUADOR", "MEXICO", "COSTA RICA", "PERU", "URUGUAY", "JAMAICA", "REPUBLICA DOMINICANA")
  
  for (i in lista_paises) {
    datafiltrada <- data[toupper(data$Pais) %in% c(i, "GLOBAL"), ]
    PARAMETROS <- as.list(datafiltrada$Valor)
    names(PARAMETROS) <- datafiltrada$Parametro
    
    parametros_paises[[i]] <- PARAMETROS
  }
  return(parametros_paises)
}
