library(rvest)
library(lubridate)
APLICACION_ID <- 7
COLOR_PRIMARIO <- "#014EA3"
COLOR_GRAFICO2 <- "#014EA3"
COLOR_GRAFICO1 <- "#f5b85e"
COLOR_SPINNER <- "#66cc33"
FECHA_COSTOS <- "2025-09"
disclaimer_text <-HTML(  "
  <p>Esta herramienta ha sido desarrollada por el Instituto de Efectividad Clínica y Sanitaria (IECS) con financiamiento de MERCK. El modelo de impacto presupuestario que sustenta la herramienta fue adaptado por el equipo de investigación de IECS.</p>
  <p>IECS es una institución académica independiente y sin fines de lucro, afiliada a la Universidad de Buenos Aires, dedicada a la investigación, educación y cooperación técnica con el objetivo de mejorar la eficiencia, la equidad, la calidad y la sustentabilidad de los sistemas y servicios de salud en América Latina. En particular, el Departamento de Evaluación de Tecnologías Sanitarias (ETS) y Economía de la Salud, responsable del desarrollo de esta herramienta, realiza evaluaciones económicas, revisiones sistemáticas, estudios de carga de enfermedad y estudios de calidad de vida, entre otros productos orientados a la toma de decisiones basadas en evidencia.</p>
  <p><b>Términos de uso de la herramienta:</b> Los datos precargados en la herramienta para el AIP fueron recolectados, revisados y validados por el equipo de investigación del IECS en septiembre de 2025.</p>
  <p>Si bien los parámetros de costos se actualizan automáticamente por el índice de precios al consumidor obtenido del INDEC, el IECS no se responsabiliza por los resultados obtenidos con esta actualización y/o resultados obtenidos con parámetros ingresados por el usuario.</p>
  "
)



sectores <<- c("PAMI", "SEGURIDAD SOCIAL", "PRIVADO")

DISCLAIMER_TITLE <- "Avelumab"
PIE_DE_TABLA1 <- "Costos actualizados a %1 por IPC según INDEC."
PIE_DE_TABLA2 <- "Costos estimados a septiembre de 2025."
PIE_DE_TABLA3 <- "Costos actualizados a %1 por AlfaBeta."
PIE_DE_TABLA4 <- "Costos actualizados a %1 por IPC según INDEC y AlfaBeta."


PIE_DE_TABLA_ERROR1 <- "Ha ocurrido un error al obtener IPC."
PIE_DE_TABLA_ERROR2 <- "Ha ocurrido un error al actualizar por AlfaBeta."
PIE_DE_TABLA_ERROR3 <- "Ha ocurrido un error al actualizar por IPC y AlfaBeta."

cargarEtiquetasTooltips <- function(){
    etiquetas_tooltipsdf <- readxl::read_excel("tooltips.xlsx")
    
    
    etiquetas <<- etiquetas_tooltipsdf |> 
      dplyr::select(Id, Etiqueta) |> 
      deframe()
    tooltips <<- etiquetas_tooltipsdf  |> 
    dplyr::select(Id, Texto, Posicion) 
    
    tooltip_list <<- purrr::pmap(tooltips, function(Id, Texto, Posicion) {
      list(text = Texto, posicion = Posicion)
    }) |> setNames(tooltips$Id)
    print("Etiquetas Cargadas")
}
cargar <- function() {
  
  data <- read_excel("lparametros.xlsx", sheet = "parametros")
  

  parametros_sectores <- list()
  parametros_inflacion <- list()
  parametros_tipo <- list()
  parametros_decimales <- list()
  parametros_infWeb <- list()
  parametros_webExtraInfo <- list()

  for (i in sectores) {
    datafiltrada <- data[toupper(data$Sector) %in% c(i, "GLOBAL"), ]

    PARAMETROS <- as.list(datafiltrada$Valor)
    TIPO <- as.list(datafiltrada$Tipo)
    INFLACION <- as.list(datafiltrada$Inflacion)
    DECIMALES <- as.list(datafiltrada$Decimales)
    INFLACION_WEB <- as.list(datafiltrada$InflacionWeb)
    WEB_EXTRAINFO <- as.list(datafiltrada$WebExtraInfo)


    names(PARAMETROS) <- datafiltrada$Parametro
    names(TIPO) <- datafiltrada$Parametro
    names(INFLACION) <- datafiltrada$Parametro
    names(DECIMALES) <- datafiltrada$Parametro
    names(INFLACION_WEB) <- datafiltrada$Parametro
    names(WEB_EXTRAINFO) <- datafiltrada$Parametro

    parametros_inflacion[[i]] <- INFLACION
    parametros_tipo[[i]] <- TIPO
    parametros_decimales[[i]] <- DECIMALES
    parametros_sectores[[i]] <- PARAMETROS
    parametros_infWeb[[i]] <- INFLACION_WEB
    parametros_webExtraInfo[[i]] <- WEB_EXTRAINFO
  }
  return(list(parametros = parametros_sectores, inflacion = parametros_inflacion,
   tipo = parametros_tipo, decimales = parametros_decimales,
   infWeb = parametros_infWeb, WebExtraInfo = parametros_webExtraInfo))
}
cargarDatos <- function() {
  
  res <- cargar()

  vParametros_opciones <<- res$tipo
  vParametros_decimales <<- res$decimales
  vParametros_inflacion <<- res$inflacion
  vParametros_infWeb <<- res$infWeb
  vParametros_wExtraInfo <<- res$WebExtraInfo
  vParametros <<- res$parametros

  print("Datos Cargados")
  
}
obtenerPagina <- function(url) {
  response <- httr::GET(url)
  if (httr::http_status(response)$category == "Success") {
    webdata <- read_html(url)
    tmp <- webdata %>% html_table(fill = TRUE)

  } else {
    tmp <- NULL
  }
  return(tmp)
}
updateFromDB <- function(originalValue,  id, con) {
  # Recibe el valor original del parametro, la web de alfabeta y la fila de la droga
  # Intenta obtener el precio, si la web o el offset está mal y da error devuelve NULL
  # si no devuelve un valor, si ese valor es el mismo que el original considera que no actualizo
  # Si devolvio un valor y no era igual al original considera que actualizo, si hubo error devuelve que hubo error.
  update <- FALSE
  if (id >= 1) {
    res <- tryCatch({
      costo <- db_obtener_costo(id, con)
        if (!is.null(costo)) {
          valor <- costo 
          list(valor, FALSE)
        } else {
          valor <- NULL
          list(valor, TRUE)
        }
      }, error = function(e) {
        list(NULL, TRUE)
      })
      valor <- res[[1]]
      hubo_error <- res[[2]]
      if (!is.null(valor) && valor != originalValue) {
        update <- TRUE
      }
  } else {
    valor <- NULL
    hubo_error <- TRUE
  }
      
      return(list(valor = valor, actualizo = update, error = hubo_error))
}

convertirPSL <- function(precio_lista) {

  precio_lista / 1.7545

}
ajustarDatos <- function(parametros, inflacionModificador, parametroInflacion, inflacionWeb, inflacionExtraInfo) {
  con <- NULL
  respuesta <- parametros  # inicializás copia
  multiplicador <- ifelse(inflacionModificador != 0, inflacionModificador, 1)
  huboIPC <- inflacionModificador != 0
  huboWeb <- FALSE

  error_web <- FALSE

  for (param in names(parametros)) {
    ajusto <- parametroInflacion[[param]]
    if (!is.null(ajusto)) {
      if (ajusto == 1) {
        respuesta[[param]] <- parametros[[param]] * multiplicador
      } else if (ajusto == 2) {
        if (is.null(con))
        {
          con <- db_Connect()
        }
        resWeb <- updateFromDB(parametros[[param]], inflacionExtraInfo[[param]], con)
        if (resWeb$actualizo) {
          respuesta[[param]] <- resWeb$valor
          huboWeb <- TRUE
        } else {
          respuesta[[param]] <- parametros[[param]]
        }
        if (resWeb$error) {
          error_web <- TRUE
        }
      }
    }
  }

  error_inflacion <- if (error_web && inflacionModificador == 0) 3
  else if (error_web) 2 else if(inflacionModificador == 0) 1 else 0

  tipo_ajuste <- if (huboIPC && huboWeb) 3 
  else if (huboWeb) 2 else if (huboIPC) 1 else 0

  return(list(respuesta, tipo_ajuste, error_inflacion))

}


obtenerInflacion <- function(fechaInicio) {
  
  url <- "https://animated-syrniki-03194d.netlify.app/"
  webpage <- read_html(url)
  # Paso 2: Extraer la tabla HTML
  # Asumimos que la tabla tiene la clase 'data-table', ajusta si la clase es diferente
  tabla <- webpage %>% html_node("table") %>% html_table(header = FALSE)
  # Paso 3: Convertir las fechas en formato adecuado y extraer los datos de inflación
  Fechas <- as.character(tabla[1, ])  # Convertir la primera fila a un vector de caracteres
  
  Fechas <- as.Date(Fechas, format="%d/%m/%Y")  # Ajusta el formato si es necesario
  # Paso 6: Si las fechas se convierten correctamente a Date, las formateamos como "YYYY-MM"
  Fechas <- format(Fechas, "%Y-%m")
  # Paso 4: Determinar el mes de inicio
  a <- which(Fechas == fechaInicio)
  print(a)
  # Asumimos que los valores de inflación/multiplicadores están en la columna 29
  inicio <- as.numeric(tabla[2, a])
  print(inicio)
  
  fechaI<- as.Date(Sys.Date(), "%Y-%m")
  print(fechaI)
  #fechaI<-AddMonths(fechaI,-0)
  
  Factual<- format(fechaI,"%Y-%m")
  t<-which(Fechas ==Factual)
  
  while((is.integer(t) && length(t) == 0) == TRUE ){
    fechaI <- fechaI %m-% months(1)
    Factual<- format(fechaI,"%Y-%m")
    t<-which(Fechas == Factual)
  }
  
  numero <- 1
  for(i in a:t) {
    numero <- numero * (1 + ((as.numeric(tabla[2, i])) * 0.01))
  }
  
  
  print(paste("Inflacion numero", numero))
  fechaI <- format(fechaI, "%m-%Y")
  res <-list(numero, fechaI)
  
  return(res)
}