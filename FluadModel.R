
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)


formatear_pesos <- function(x, decimales = 0) {
  formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales)
}

formatear_pesos2 <- function(x, decimales = 0) {
  paste0("$", formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales))
}
formatear_epi <- function(x, decimales = 0) {
  formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = decimales)
}
procesarResultados <- function(horizonteTemporal, cohorte, poblacionActual, poblacionProyectada) {
  

  promediarLista <- function(lista, campo) {
    mean(sapply(lista[1:horizonteTemporal], function(x) x[[campo]]))
  }
  sumarLista <- function(lista, campo) {
    sum(sapply(lista[1:horizonteTemporal], function(x) x[[campo]]))
  }
  
  resActual <- list()
  resProy <- list()
  
  detailActual <- list()
  detailProy <- list()
  
  saniActual <- list()
  saniProy <- list()
  
  
  fillSummaryTable <- function(table, campos) {
    
    resumenAcumulado <- list()
    resumenPromedio <- list()
    
    for (x in campos)
    {
      resumenAcumulado[[x]] <- sumarLista(table, x)
      resumenPromedio[[x]] <- promediarLista(table, x)
    }
    table[[horizonteTemporal + 1]] <- resumenAcumulado
    table[[horizonteTemporal + 2]] <- resumenPromedio
    return(table)
  }
  
  fillResTable <- function(pob, año) {
    res <- list()
    res$costosVacunacion <- sum(pob$costosVacunacion[, año] + pob$costosAdministracion[, año])
    res$costosSanitarios <- sum(pob$costosConsultas[, año]) + sum(pob$costosHospitalizaciones[, año])
    res$costoTotal <- res$costosVacunacion + res$costosSanitarios
    return(res)
  }

  fillDetailTable <- function(pob, año) {
    res <- list()
    res$costosVacunacion <- sum(pob$costosVacunacion[, año])
    res$costosAdministracion <- sum(pob$costosAdministracion[, año])
    res$costosConsultas <- sum(pob$costosConsultas[, año]) 
    res$costosHospitalizaciones <- sum(pob$costosHospitalizaciones[, año])
    res$costoTotal <- res$costosVacunacion + res$costosAdministracion + res$costosConsultas + res$costosHospitalizaciones
    return(res)
  }  
  fillSaniTable <- function(pob, año) {
    res <- list()
    res$personasVacunadas <- sum(pob$personasVacunadas[, año])
    res$casosInfluenza <- sum(pob$casosInfluenza[, año])
    res$consultas <- sum(pob$consultas[, año]) 
    res$hospitalizaciones <- sum(pob$hospitalizaciones[, año])
    res$muertes <- sum(pob$muertes[, año])
    return(res)
  }  
  
  for (i in 1:horizonteTemporal)
  {
    resA <- fillResTable(poblacionActual, i)
    resP <- fillResTable(poblacionProyectada, i)
    
    resP$IP <- resP$costoTotal - resA$costoTotal
    resP$IPpc <- (resP$IP / resA$costoTotal)

    resP$PMPM <- resP$IP / cohorte / 12
    
    resActual[[i]] <- resA
    resProy[[i]] <- resP
    
    detA <- fillDetailTable(poblacionActual, i)
    detP <- fillDetailTable(poblacionProyectada, i)
    
    detailActual[[i]] <- detA
    detailProy[[i]] <- detP
    
    sanA <- fillSaniTable(poblacionActual, i)
    sanP <- fillSaniTable(poblacionProyectada, i)
    
    saniActual[[i]] <- sanA
    saniProy[[i]] <- sanP
    
  }
  

  resActual <- fillSummaryTable(resActual,  c("costosVacunacion", "costosSanitarios", "costoTotal"))

  resProy <- fillSummaryTable(resProy,  c("costosVacunacion", "costosSanitarios", "costoTotal"))

  resProy[[horizonteTemporal + 1]]$IP <- resProy[[horizonteTemporal + 1]]$costoTotal - resActual[[horizonteTemporal + 1]]$costoTotal
  resProy[[horizonteTemporal + 1]]$IPpc <- (resProy[[horizonteTemporal + 1]]$costoTotal - resActual[[horizonteTemporal + 1]]$costoTotal) / resActual[[horizonteTemporal + 1]]$costoTotal
  resProy[[horizonteTemporal + 1]]$PMPM <- resProy[[horizonteTemporal + 1]]$IP / cohorte / 12 / horizonteTemporal
  
  resProy[[horizonteTemporal + 2]]$IP <- (resProy[[horizonteTemporal + 1]]$costoTotal - resActual[[horizonteTemporal + 1]]$costoTotal) / horizonteTemporal
  resProy[[horizonteTemporal + 2]]$IPpc <- ((resProy[[horizonteTemporal + 1]]$costoTotal - resActual[[horizonteTemporal + 1]]$costoTotal) / resActual[[horizonteTemporal + 1]]$costoTotal)
  resProy[[horizonteTemporal + 2]]$PMPM <- resProy[[horizonteTemporal + 1]]$IP / cohorte / 12 / horizonteTemporal
  
  detailActual <- fillSummaryTable(detailActual, c("costosVacunacion", "costosAdministracion", "costosConsultas", "costosHospitalizaciones", "costoTotal"))
  detailProy <- fillSummaryTable(detailProy, c("costosVacunacion", "costosAdministracion", "costosConsultas", "costosHospitalizaciones", "costoTotal"))
  
  saniActual <- fillSummaryTable(saniActual, c("personasVacunadas", "casosInfluenza", "consultas", "hospitalizaciones", "muertes"))
  saniProy <- fillSummaryTable(saniProy, c("personasVacunadas", "casosInfluenza", "consultas", "hospitalizaciones", "muertes"))
  
  
 
  indicadores <- list(personasVacunadas = formatear_epi(mean(colSums(poblacionProyectada$personasVacunadas))),
                      deltaCostoVacunas = formatear_pesos2(mean(colSums(poblacionProyectada$costosVacunacion)) - mean(colSums(poblacionActual$costosVacunacion)) ),
                      HospEvitadas = formatear_epi(mean(colSums(poblacionActual$hospitalizaciones)) - mean(colSums(poblacionProyectada$hospitalizaciones))) , 
                      costosEvitados = formatear_pesos2((mean(colSums(poblacionActual$costosConsultas)) + mean(colSums(poblacionActual$costosHospitalizaciones))) - (mean(colSums(poblacionProyectada$costosConsultas)) + mean(colSums(poblacionProyectada$costosHospitalizaciones)))),
                      impactoPresupuestario = formatear_pesos2(resProy[[horizonteTemporal + 2]]$IP),
                      impactoPMPM = formatear_pesos2(resProy[[horizonteTemporal + 2]]$PMPM, 2), 
                      horizonteTemporal = horizonteTemporal
  )
  
  tabla_actual_detail <- data.frame(
     Categoria = c("Costos de Vacunación", "Costos de Administración", "Costos de Consultas", "Costos de Hospitalizaciones", "Costo Total"),
     rbind(
       formatear_pesos2(sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costosVacunacion)),
       formatear_pesos2(sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costosAdministracion)),
       formatear_pesos2(sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costosConsultas)),
       formatear_pesos2(sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costosHospitalizaciones)),
       formatear_pesos2(sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costoTotal))
      )
  )
  colnames(tabla_actual_detail)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")
  
  
  tabla_actual <- data.frame(
    Categoria = c("Costos de Vacunación", "Costos Sanitarios", "Costos Totales"),
    rbind(
      formatear_pesos2(sapply(resActual[1:(horizonteTemporal + 2)], function(x) x$costosVacunacion)),
      formatear_pesos2(sapply(resActual[1:(horizonteTemporal + 2)], function(x) x$costosSanitarios)),
      formatear_pesos2(sapply(resActual[1:(horizonteTemporal + 2)], function(x) x$costoTotal))
    )
  )
  colnames(tabla_actual)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")
  tabla_actual_sanitaria <- data.frame(
    Categoria = c("Personas Vacunadas", "Casos de Enfermedad X", "Consultas Ambulatorias", "Hospitalizaciones", "Muertes"),
    rbind(
      formatear_epi(sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$personasVacunadas)),
      formatear_epi(sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$casosInfluenza)),
      formatear_epi(sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$consultas)),
      formatear_epi(sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$hospitalizaciones)),
      formatear_epi(sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$muertes))
    )
  )
  colnames(tabla_actual_sanitaria)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")
  
  tabla_proy_detail <- data.frame(
    Categoria =  c("Costos de Vacunación", "Costos de Administración", "Costos de Consultas", "Costos de Hospitalizaciones", "Costo Total"),
    rbind(
      formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costosVacunacion)),
      formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costosAdministracion)),
      formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costosConsultas)),
      formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costosHospitalizaciones)),
      formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costoTotal))
    )
    
  )
  colnames(tabla_proy_detail)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")
  
  tabla_proy <- data.frame(
    Categoria = c("Costos de Vacunación", "Costos Sanitarios", "Costos Totales"),
    rbind(
      formatear_pesos2(sapply(resProy[1:(horizonteTemporal + 2)], function(x) x$costosVacunacion)),
      formatear_pesos2(sapply(resProy[1:(horizonteTemporal + 2)], function(x) x$costosSanitarios)),
      formatear_pesos2(sapply(resProy[1:(horizonteTemporal + 2)], function(x) x$costoTotal))
    )

  )
  colnames(tabla_proy)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")
  tabla_proy_sanitaria <- data.frame(
    Categoria = c("Personas Vacunadas", "Casos de Enfermedad X", "Consultas Ambulatorias", "Hospitalizaciones", "Muertes"),
    rbind(
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$personasVacunadas)),
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$casosInfluenza)),
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$consultas)),
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$hospitalizaciones)),
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$muertes))
    )
  )
  colnames(tabla_proy_sanitaria)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")
  
  tabla_dif_detail <- data.frame(
    Categoria =   c("Costos de Vacunación", "Costos de Administración", "Costos de Consultas", "Costos de Hospitalizaciones", "Costo Total"),
    rbind(
        formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costosVacunacion) - sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costosVacunacion)),
        formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costosAdministracion) - sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costosAdministracion)),
        formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costosConsultas) - sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costosConsultas)),
        formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costosHospitalizaciones) - sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costosHospitalizaciones)),
        formatear_pesos2(sapply(detailProy[1:(horizonteTemporal + 2)], function(x) x$costoTotal) - sapply(detailActual[1:(horizonteTemporal + 2)], function(x) x$costoTotal))
    )

  )
  colnames(tabla_dif_detail)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")
  tabla_dif <- data.frame(
    Categoria =  c("Costos de Vacunación", "Costos Sanitarios", "Impacto Presupuestario", "Impacto Presupuestario (%)", "Impacto Presupuestario PMPM"),
    rbind(
      formatear_pesos2(sapply(resProy[1:(horizonteTemporal + 2)], function(x) x$costosVacunacion) - sapply(resActual[1:(horizonteTemporal + 2)], function(x) x$costosVacunacion)),
      formatear_pesos2(sapply(resProy[1:(horizonteTemporal + 2)], function(x) x$costosSanitarios) - sapply(resActual[1:(horizonteTemporal + 2)], function(x) x$costosSanitarios)),
      formatear_pesos2(sapply(resProy[1:(horizonteTemporal + 2)], function(x) x$IP)),
      paste0(formatear_pesos(sapply(resProy[1:(horizonteTemporal + 2)], function(x) x$IPpc * 100), decimales = 2) , "%"),
      formatear_pesos2(sapply(resProy[1:(horizonteTemporal + 2)], function(x) x$PMPM), decimales = 2)
    )
    
  )
  colnames(tabla_dif)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")
  
  tabla_dif_sanitaria <- data.frame(
    Categoria =   c("Personas Vacunadas", "Casos de Enfermedad X", "Consultas Ambulatorias", "Hospitalizaciones", "Muertes"),
    rbind(
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$personasVacunadas) - sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$personasVacunadas)),
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$casosInfluenza) - sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$casosInfluenza)),
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$consultas) - sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$consultas)),
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$hospitalizaciones) - sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$hospitalizaciones)),
      formatear_epi(sapply(saniProy[1:(horizonteTemporal + 2)], function(x) x$muertes) - sapply(saniActual[1:(horizonteTemporal + 2)], function(x) x$muertes))
      
    )
    
  )
  colnames(tabla_dif_sanitaria)[-1] <- c(paste0("Año ", 1:horizonteTemporal), "Acumulado", "Promedio")

  tablaRes = list(
      tabla_actual = tabla_actual,
      tabla_proy = tabla_proy,
      tabla_dif = tabla_dif
  )
  tablaDetail = list(
      tabla_actual = tabla_actual_detail,
      tabla_proy = tabla_proy_detail,
      tabla_dif = tabla_dif_detail
  )
  tablaSanitaria = list(
    tabla_actual = tabla_actual_sanitaria,
    tabla_proy = tabla_proy_sanitaria,
    tabla_dif = tabla_dif_sanitaria
  )
   lGraficos = list(
     PMPM = sapply(1:(horizonteTemporal+2), function(i) resProy[[i]]$PMPM) 
  )

  resultado = list(
    indicadores = indicadores,
    tablaRes = tablaRes,
    graficos = lGraficos,
    tablaSanitaria = tablaSanitaria, 
    tablaDetail = tablaDetail
    
  )
  return(resultado)  
  
  
}

obtenerPoblaciones <- function(horizonteTemporal, parametros, parametrosI) {
  print("Ingresa en obtener poblacion")


  poblacionProyectada <- list(
    personasElegibles = matrix(0, nrow = 5, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("<50", "50-59", "60-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    personasVacunadas = matrix(0, nrow = 5, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("<50", "50-59", "60-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    costosVacunacion = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    costosAdministracion = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    casosInfluenza = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    casosSinConsulta = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    consultas = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año", 1:horizonteTemporal))),
    costosConsultas = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año", 1:horizonteTemporal))),
    hospitalizaciones = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    costosHospitalizaciones = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    muertes = matrix(0, nrow = 3, ncol =horizonteTemporal,  dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal)))
  )
  poblacionActual <- list(
    personasElegibles = matrix(0, nrow = 5, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("<50", "50-59", "60-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    personasVacunadas = matrix(0, nrow = 5, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("<50", "50-59", "60-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    costosVacunacion = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    costosAdministracion = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    casosInfluenza = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    casosSinConsulta = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    consultas = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    costosConsultas = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año", 1:horizonteTemporal))),
    hospitalizaciones = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    costosHospitalizaciones = matrix(0, nrow = 3, ncol = horizonteTemporal, dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal))),
    muertes = matrix(0, nrow = 3, ncol = horizonteTemporal,  dimnames = list(grupoEtario = c("50-64", "65-74", "75+"), Año = paste0("Año",1:horizonteTemporal)))
  )

  pEtarios <- list(
    grupo1 = 1 - (parametros[["porcEtario5064"]] + parametros[["porcEtario65"]]),
    grupo2 = parametros[["porcEtario5064"]] * parametrosI[["porcEtario5059"]],
    grupo3 = parametros[["porcEtario5064"]] * (1 - parametrosI[["porcEtario5059"]]),
    grupo4 = parametros[["porcEtario65"]] * parametrosI[["porcEtario6574"]],
    grupo5 = parametros[["porcEtario65"]] * (1 - parametrosI[["porcEtario6574"]])
  )
  

  for (y in 1:horizonteTemporal) {
    
    for(i in 1:5)
    {
      
      poblacionActual$personasElegibles[i, y] = parametros[["cohorte"]] * pEtarios[[paste0("grupo", i)]] * parametrosI[[paste0("porcAR", i)]] * switch(i, 0, parametros[["bPob5064"]], parametros[["bPob5064"]], parametros[["bPob65"]], parametros[["bPob65"]])
      poblacionProyectada$personasElegibles[i, y] = parametros[["cohorte"]] * pEtarios[[paste0("grupo", i)]] * parametrosI[[paste0("porcAR", i)]] * switch(i, 0, parametros[["bPob5064"]], parametros[["bPob5064"]], parametros[["bPob65"]], parametros[["bPob65"]])
      
      poblacionActual$personasVacunadas[i, y] = poblacionActual$personasElegibles[i, y] * switch(i, 0, parametros[["adh5064"]], parametros[["adh5064"]],  parametros[["adh6574"]],  parametros[["adh75"]])
      poblacionProyectada$personasVacunadas[i, y] = poblacionProyectada$personasElegibles[i, y] * switch(i, 0, parametros[["adh5064"]], parametros[["adh5064"]],  parametros[["adh6574"]],  parametros[["adh75"]])
      
      #poblacionActual$personasElegibles[i, y] = parametros[["cohorte"]] * parametrosI[[paste0("porcEtario", i)]] * parametrosI[[paste0("porcAR", i)]] * switch(i, 0, parametros[["bPob5064"]], parametros[["bPob5064"]], parametros[["bPob65"]], parametros[["bPob65"]])
      #poblacionProyectada$personasElegibles[i, y] = parametros[["cohorte"]] * parametrosI[[paste0("porcEtario", i)]] * parametrosI[[paste0("porcAR", i)]] * switch(i, 0, parametros[["bPob5064"]], parametros[["bPob5064"]], parametros[["bPob65"]], parametros[["bPob65"]])
      
      #poblacionActual$personasVacunadas[i, y] = poblacionActual$personasElegibles[i, y] * switch(i, 0, parametros[["adh5064"]], parametros[["adh5064"]],  parametros[["adh6574"]],  parametros[["adh75"]])
      #poblacionProyectada$personasVacunadas[i, y] = poblacionProyectada$personasElegibles[i, y] * switch(i, 0, parametros[["adh5064"]], parametros[["adh5064"]],  parametros[["adh6574"]],  parametros[["adh75"]])
    }
  }

  #Devolvemos datos
  return (list(poblacionActual = poblacionActual
              , poblacionProyectada = poblacionProyectada))
  
  
  
  
  
}
correrModelo <- function(horizonteTemporal, parametros, parametrosI) {
  
  
  setCasos <- function(pob, grupo,  y, etario_suffix, escenario) {
      return(calcularCasos(
      poblacionElegible = sum(pob$personasElegibles[etario_suffix, y]),
      poblacionVacunada = sum(pob$personasVacunadas[etario_suffix, y]),
      incidencia = parametrosI[[paste0("iInf", grupo)]] * (parametros[["mIncidencia"]] / 100),
      msTIV = parametros[[paste0(ifelse(escenario == 1, "a", ""), "msTIVSD", y, grupo)]],
      msQIV = parametros[[paste0(ifelse(escenario == 1, "a", ""), "msQIVSD", y, grupo)]],
      efTIV = parametrosI[[paste0("efTIVSD", grupo)]],
      msQIVHD = parametros[[paste0(ifelse(escenario == 1, "a", ""), "msQIVHD", y, grupo)]],
      rveQIVHD = parametrosI[[paste0("rveQIVHD", grupo)]],
      msaTIV = parametros[[paste0(ifelse(escenario == 1, "a", ""), "msaTIV", y, grupo)]],
      rveaTIV = parametrosI[[paste0("rveaTIV", grupo)]]
    ))
  }
  
  setEventos <- function(pob, grupo, año) {
    #Calculamos las consultas a partir de los casos de influenza.
    pob$consultas[grupo, año] <- pob$casosInfluenza[grupo, año] * ifelse(grupo == "50-64", parametrosI[["pCons5064"]], parametrosI[["pCons65"]])
    
    #Estimamos el costo de las Consultas
    pob$costosConsultas[grupo, año] <- pob$consultas[grupo, año] * parametros[["cConsulta"]]
    
    #Calculamos los casos que no consultan.
    pob$casosSinConsulta[grupo, año] <- pob$casosInfluenza[grupo, año] - pob$consultas[grupo, año]
    #Calculamos las hospitalizaciones a partir de las consultas
    pob$hospitalizaciones[grupo, año] <- pob$consultas[grupo, año] * ifelse(grupo == "50-64", parametrosI[["pHosp5064"]], ifelse(grupo == "65-74", parametrosI[["pHosp6574"]], parametrosI[["pHosp75"]]))
    
    #Estimamos el costo de las hospitalizaciones
    pob$costosHospitalizaciones[grupo, año] <- pob$hospitalizaciones[grupo, año] * parametros[["cHosp"]]
    
    #Calculamos las muertes a partir de las hospitalizaciones
    pob$muertes[grupo, año] <- pob$hospitalizaciones[grupo, año] * ifelse(grupo == "50-64", parametrosI[["pMuerte5064"]], ifelse(grupo == "65-74", parametrosI[["pMuerte6574"]], parametrosI[["pMuerte75"]]))
    
    return(pob)
  }
  setCostosVacunacion <- function(pob, grupo, año, escenario) {
    
    personasVacunadas <- ifelse(grupo == "50-64", pob$personasVacunadas["50-59", año] + pob$personasVacunadas["60-64", año], pob$personasVacunadas[grupo, año])
    msSufijo <- paste0(año, ifelse(grupo == "50-64", grupo, "65"))
    msPrefijo <- paste0(ifelse(escenario == 1, "a", ""), "ms")
    costos <- (
      personasVacunadas * parametros[[paste0(msPrefijo, "TIVSD", msSufijo)]] * parametros[["cTIVSD"]] +
      personasVacunadas * parametros[[paste0(msPrefijo, "QIVSD", msSufijo)]] * parametros[["cQIVSD"]] +
      personasVacunadas * parametros[[paste0(msPrefijo, "QIVHD", msSufijo)]] * parametros[["cQIVHD"]] +
      personasVacunadas * parametros[[paste0(msPrefijo, "aTIV", msSufijo)]] * parametros[["caTIV"]] 
    )
    return(costos)
    
    
  }
  setCostosAdministracion <- function(pob, grupo, año) {
    
    personasVacunadas <- ifelse(grupo == "50-64", pob$personasVacunadas["50-59", año] + pob$personasVacunadas["60-64", año], pob$personasVacunadas[grupo, año])
    costos <- (
      personasVacunadas *  parametros[["cAdm"]]
    )
    return(costos)
    
    
  }
  print("Ingresa a correr Modelo")
  
  resultado <- obtenerPoblaciones(horizonteTemporal, parametros, parametrosI)
  
  poblacionActual <- resultado$poblacionActual
  poblacionProyectada <- resultado$poblacionProyectada
 
  
  for (y in 1:horizonteTemporal)
  {
    
    poblacionActual$costosVacunacion["50-64", y] <- setCostosVacunacion(poblacionActual, "50-64", y, 1)
    poblacionActual$costosVacunacion["65-74", y]  <- setCostosVacunacion(poblacionActual, "65-74", y, 1)
    poblacionActual$costosVacunacion["75+", y]  <- setCostosVacunacion(poblacionActual, "75+", y, 1)
    
    poblacionProyectada$costosVacunacion["50-64", y]   <- setCostosVacunacion(poblacionProyectada, "50-64", y, 2)
    poblacionProyectada$costosVacunacion["65-74", y]  <- setCostosVacunacion(poblacionProyectada, "65-74", y, 2)
    poblacionProyectada$costosVacunacion["75+", y] <- setCostosVacunacion(poblacionProyectada, "75+", y, 2)
    
    poblacionActual$costosAdministracion["50-64", y] <- setCostosAdministracion(poblacionActual, "50-64", y)
    poblacionActual$costosAdministracion["65-74", y]  <- setCostosAdministracion(poblacionActual, "65-74", y)
    poblacionActual$costosAdministracion["75+", y]  <- setCostosAdministracion(poblacionActual, "75+", y)
    
    poblacionProyectada$costosAdministracion["50-64", y]   <- setCostosAdministracion(poblacionProyectada, "50-64", y)
    poblacionProyectada$costosAdministracion["65-74", y]  <- setCostosAdministracion(poblacionProyectada, "65-74", y)
    poblacionProyectada$costosAdministracion["75+", y] <- setCostosAdministracion(poblacionProyectada, "75+", y)
    
    poblacionActual$casosInfluenza["50-64", y] <- setCasos(pob = poblacionActual, grupo = "50-64", y, 2:3, 1)
    poblacionActual$casosInfluenza["65-74", y] <- setCasos(pob = poblacionActual,  grupo = "65", y, 4, 1)
    poblacionActual$casosInfluenza["75+", y] <- setCasos(pob = poblacionActual, grupo = "65", y, 5, 1)
    
    
    poblacionProyectada$casosInfluenza["50-64", y] <- setCasos(pob = poblacionProyectada, grupo = "50-64", y, 2:3, 2)
    poblacionProyectada$casosInfluenza["65-74", y] <- setCasos(pob = poblacionProyectada, grupo = "65", y, 4, 2)
    poblacionProyectada$casosInfluenza["75+", y] <- setCasos(pob = poblacionProyectada, grupo = "65", y, 5, 2)
    
    
    
    poblacionActual <- setEventos(poblacionActual, "50-64", y)
    poblacionActual <- setEventos(poblacionActual, "65-74", y)
    poblacionActual <- setEventos(poblacionActual, "75+", y)
    
    poblacionProyectada <- setEventos(poblacionProyectada, "50-64", y)
    poblacionProyectada <- setEventos(poblacionProyectada, "65-74", y)
    poblacionProyectada <- setEventos(poblacionProyectada, "75+", y)
    
  }
  

  
  return(procesarResultados(horizonteTemporal, parametros[["cohorte"]], poblacionActual, poblacionProyectada))
  
}
calcularCasos <- function(poblacionElegible, poblacionVacunada, incidencia, msTIV, msQIV, efTIV, msQIVHD, rveQIVHD, msaTIV, rveaTIV)
{
  noVacunados <- poblacionElegible - poblacionVacunada
  casos <- noVacunados * incidencia
  casos <- (casos + 
    poblacionVacunada * msTIV * incidencia * (1 - efTIV) +
    poblacionVacunada * msQIV * incidencia * (1 - efTIV) + 
    poblacionVacunada * msQIVHD * incidencia * (1 - efTIV) * (1 - rveQIVHD) + 
    poblacionVacunada * msaTIV * incidencia * (1 - efTIV) * (1 - rveQIVHD) * (1 - rveaTIV)
  )
  
  return(casos)
}
cargarDatos()
print("Datos cargados.")

