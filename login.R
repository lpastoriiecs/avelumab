library(shiny)
library(sodium)
LOGIN_MODULO <- "login1"
mod_login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "loginRow",
      div(id = ns("loginPanel"), class = "centered-container",
          div(class = "login-panel",
              textInput(ns("user"), "Usuario"),
              passwordInput(ns("password"), "Contraseña"),
              actionButton(ns("cmdLogin"), "Ingresar")
          ),
          div(id = ns("disclaimerPanel"), class = "disclaimer-container",
              h1(class = "disclaimer-title", DISCLAIMER_TITLE),
              div(id = ns("disclaimerText"), class = "disclaimer-text", 
                  disclaimer_text,
                  div(class="scroll-sentinel")
              ),
              actionButton(ns("cmdAcepto"), "Acepto términos de uso", class = "cmdAcepto")
          ),
          # Spinner como overlay encima del loginPanel
          div(id = ns("spinner"), class = "spinner-overlay", style = "display: none;",
              div(class = "spinner")
          )
      )
    )
  )
}

# Función que valida las credenciales
validarLogueo <- function(usuario, password, aplicacion_id) {
  res <- db_evaluar_logueo(NULL, usuario, aplicacion_id)
  
  if (nrow(res) == 0) {
    return(list( ok = FALSE, msg = "Credenciales inválidas"))
  }
  
  if (!sodium::password_verify(res$password, password)) {
    return(list( ok = FALSE, msg = "Credenciales inválidas"))
  }
  
  if (!res$organizacion_tiene_acceso) {
    return(list( ok = FALSE, msg = "Credenciales inválidas"))
  }
  
  if (!res$usuario_tiene_acceso) {
    return(list( ok = FALSE, msg = "Tu usuario no se encuentra habilitado para ingresar."))
  }
  
  return(list(ok = TRUE, msg = ""))
}

# Servidor del módulo
mod_login_server <- function(id, aplicacion_id, app_visible) {
  moduleServer(id, function(input, output, session) {
    user_logged <- reactiveVal(FALSE)  # Definimos el estado reactivo del login
    CREDENCIALES_OK <- reactiveVal(FALSE)
    
    ns <- NS(id)
    # Login
    observeEvent(input$cmdAcepto, { 
      req(CREDENCIALES_OK() == TRUE)
      app_visible(TRUE)
      user_logged(TRUE)
      CREDENCIALES_OK(FALSE)
      shinyjs::runjs(sprintf('$("#%s").prop("disabled", true);', ns("cmdAcepto")))
      shinyjs::runjs(sprintf('$("#%s").hide();', ns("loginPanel")))      
      
    })
    observeEvent(input$cmdLogin, {
      
      shinyjs::runjs(sprintf('$("#%s").prop("disabled", true);', ns("cmdLogin")))
      shinyjs::runjs(sprintf('$("#%s").css("display", "flex");', ns("spinner")))
      valLogin <- validarLogueo(input$user, input$password, aplicacion_id)
      if (valLogin$ok) {
        CREDENCIALES_OK(TRUE)
        shinyjs::runjs(sprintf('$("#%s").css("display", "flex");', ns("disclaimerPanel")))
      } else {
        showNotification(valLogin$msg, type = "error")
      }
      shinyjs::runjs(sprintf('$("#%s").css("display", "none");', ns("spinner")))
      shinyjs::runjs(sprintf('$("#%s").prop("disabled", false);', ns("cmdLogin")))
    })
    # Retornamos el estado reactivo
    return(user_logged)
  })
}