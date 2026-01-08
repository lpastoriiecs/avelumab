#install.packages("DBI")
#install.packages("RPostgres")

library(DBI)
library(RPostgres)

db_Connect <- function() {
  print("DB Conecta")
  dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = "aws-0-sa-east-1.pooler.supabase.com",
    port = 5432,
    user = "postgres.ulgmkxbybkmmxgyroxme",
    password = "I3C5DBpassWord$!",
    sslmode = "require"
  )
  
}

db_Disconnect <- function(con) {
  dbDisconnect(con)
}
db_crear_tabla <- function(con = NULL, query, query_tail) {
  
  propio <- is.null(con)
  if (propio) con <- db_Connect()
  
  fquery <- paste("CREATE TABLE IF NOT EXISTS ", query, ", created_at TIMESTAMP default now()", query_tail, ");", sep = "")
  print(fquery)
  dbExecute(con, fquery)
  
  if (propio) dbDisconnect(con)
}

db_crear_tabla_usuarios <- function(con = NULL) {
  
  db_crear_tabla(
    con,
    "usuarios (id SERIAL PRIMARY KEY, nombre TEXT UNIQUE NOT NULL, email TEXT UNIQUE NOT NULL, password TEXT NOT NULL, organizacion_id INTEGER",
    ", FOREIGN KEY (organizacion_id) REFERENCES organizaciones(id)" 
  )
  
}
db_crear_tabla_organizaciones <- function(con = NULL) {
  
  db_crear_tabla(
    con,
    "organizaciones (id SERIAL PRIMARY KEY, nombre TEXT UNIQUE NOT NULL",
    ""
  )
  
}
db_crear_tabla_aplicaciones <- function(con = NULL) {
  
  db_crear_tabla(
    con,
    "aplicaciones (id SERIAL PRIMARY KEY, nombre TEXT UNIQUE NOT NULL",
    ""
  )
  
}
db_crear_tabla_aplicaciones_organizaciones <- function(con = NULL) {
  
  db_crear_tabla(
    con,
    "aplicaciones_organizaciones (aplicacion_id INTEGER REFERENCES aplicaciones(id), organizacion_id INTEGER REFERENCES organizaciones(id)",
    ", PRIMARY KEY (aplicacion_id, organizacion_id)"
  )
  
}

db_crear_tabla_escenarios <- function(con = NULL) {
  
  db_crear_tabla(
    con,
    "escenarios (id INTEGER PRIMARY KEY, nombre TEXT NOT NULL, aplicacion_id INTEGER NOT NULL, owner_id INTEGER NOT NULL",
    ", FOREIGN KEY (aplicacion_id) REFERENCES aplicaciones(id), FOREIGN KEY (owner_id) REFERENCES usuarios(id)"
  )
}

db_crear_tabla_escenario_usuarios <- function(con = NULL) {
  db_crear_tabla(
    con,
    "escenario_usuarios ( escenario_id INTEGER NOT NULL, usuario_id INTEGER NOT NULL",
    ", PRIMARY KEY (escenario_id, usuario_id), FOREIGN KEY (escenario_id) REFERENCES escenarios(id) ON DELETE CASCADE, FOREIGN KEY (usuario_id) REFERENCES usuarios(id) ON DELETE CASCADE"
  )
}

db_crear_tabla_escenario_parametros <- function(con = NULL) {
  db_crear_tabla(
    con,
    "escenario_parametros (escenario_id INTEGER NOT NULL, parametro_id INTEGER NOT NULL, valor DECIMAL(15,4) NOT NULL",
    ", FOREIGN KEY (escenario_id) REFERENCES escenarios(id) ON DELETE CASCADE"
  )
}

db_obtener_tablas <- function(con = NULL) {
  propio <- is.null(con)
  if (propio) con <- db_Connect()
  res <- dbGetQuery(con, "
    SELECT table_name 
    FROM information_schema.tables 
    WHERE table_schema = 'public' AND table_type = 'BASE TABLE'
  ")  
  if (propio) dbDisconnect(con)
  return(res$table_name)
}

db_insert_values <- function(con = NULL, tabla, fields, values) {
  propio <- is.null(con)
  if (propio) con <- db_Connect()
  query <- paste0("INSERT INTO ", tabla, " (", fields , ") ",
                  "VALUES ('", values , "');")
  dbExecute(con, query)
  
  if (propio) dbDisconnect(con)
}

db_evaluar_logueo <- function(con = NULL, usuario, aplicacion_id) {
  propio <- is.null(con)
  if (propio) con <- db_Connect()
  query <- "
    SELECT
      u.id,
      u.password,
      u.organizacion_id,
      EXISTS (
        SELECT 1 FROM aplicaciones_organizaciones oa
        WHERE oa.organizacion_id = u.organizacion_id
          AND oa.aplicacion_id = $1
      ) AS organizacion_tiene_acceso,
      EXISTS (
        SELECT 1 FROM usuarios_aplicaciones ua
        WHERE ua.usuario_id = u.id
          AND ua.aplicacion_id = $1
      ) AS usuario_tiene_acceso
    FROM usuarios u
    WHERE LOWER(u.nombre) = LOWER($2)
    LIMIT 1
  "
  res <- dbGetQuery(con, query, params = list(aplicacion_id, usuario))

  if (propio) dbDisconnect(con)
  return(res)
}