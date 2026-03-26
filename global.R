library(rvest)
library(lubridate)
APLICACION_ID <- 7
COLOR_PRIMARIO <- "#014EA3"
COLOR_GRAFICO2 <- "#014EA3"
COLOR_GRAFICO1 <- "#f5b85e"
COLOR_SPINNER <- "#66cc33"
FECHA_COSTOS <- "2025-05"
disclaimer_text <-HTML(  "
  <p>Esta herramienta ha sido desarrollada por el Instituto de Efectividad Clínica y Sanitaria (IECS)</p>
  "
)
sectores <<- c("PAMI", "SEGURIDAD SOCIAL", "PRIVADO")

DISCLAIMER_TITLE <- "Avelumab"
PIE_DE_TABLA1 <- "Costos actualizados a %1 por IPC según INDEC."
PIE_DE_TABLA2 <- "Costos estimados a mayo de 2025."
PIE_DE_TABLA3 <- "Costos actualizados a %1 por AlfaBeta."
PIE_DE_TABLA4 <- "Costos actualizados a %1 por IPC según INDEC y AlfaBeta."

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

  for (i in sectores) {
    datafiltrada <- data[toupper(data$Sector) %in% c(i, "GLOBAL"), ]
    PARAMETROS <- as.list(datafiltrada$Valor)
    TIPO <- as.list(datafiltrada$Tipo)
    INFLACION <- as.list(datafiltrada$Inflacion)
    DECIMALES <- as.list(datafiltrada$Decimales)
    names(PARAMETROS) <- datafiltrada$Parametro
    names(TIPO) <- datafiltrada$Parametro
    names(INFLACION) <- datafiltrada$Parametro
    names(DECIMALES) <- datafiltrada$Parametro
    
    parametros_inflacion[[i]] <- INFLACION
    parametros_tipo[[i]] <- TIPO
    parametros_decimales[[i]] <- DECIMALES
    parametros_sectores[[i]] <- PARAMETROS
  }
  return(list(parametros = parametros_sectores, inflacion = parametros_inflacion, tipo = parametros_tipo, decimales = parametros_decimales))
}
cargarDatos <- function() {
  
  res <- cargar()
  clas <- read_excel("lparametros.xlsx", sheet = "parametros")

  vParametros_opciones <<- res$tipo
  vParametros_decimales <<- res$decimales
  vParametros_inflacion <<- res$inflacion
  vParametros <<- res$parametros

  print("Datos Cargados")
  
}
updateWeb <- function(nombreParametro) {
  # mockup - a implementar
  return(list(valor = NULL, actualizo = FALSE))
}

ajustarDatos <- function(parametros, inflacionModificador, parametroInflacion) {

  respuesta <- parametros  # inicializás copia
  multiplicador <- ifelse(inflacionModificador != 0, inflacionModificador, 1)
  huboIPC <- inflacionModificador != 0
  huboWeb <- FALSE

  for (param in names(parametros)) {
    ajusto <- parametroInflacion[[param]]
    if (!is.null(ajusto)) {
      if (ajusto == 1) {
        respuesta[[param]] <- parametros[[param]] * multiplicador
      } else if (ajusto == 2) {
        resWeb <- updateWeb(param)
        if (resWeb$actualizo) {
          respuesta[[param]] <- resWeb$valor
          huboWeb <- TRUE
        } else {
          respuesta[[param]] <- parametros[[param]]
        }
      }
    }
  }

  tipoAjuste <- if (huboIPC && huboWeb) 3 else if (huboWeb) 2 else if (huboIPC) 1 else 0

  return(list(respuesta, tipoAjuste))

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