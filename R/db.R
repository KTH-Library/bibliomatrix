#' Connection to Bibliometrics data source for KTH
#' 
#' This function returns a db connection to one of two possible pre-configured
#' data sources containing Bibliometrics data
#' 
#' @param source_type one of "mssql" or "sqlite" with "sqlite" being default
#' @return database connection
#' @export
con_bib <- function(source_type = c("sqlite", "mssql")) 
{
  type <- match.arg(source_type)
  switch(type,
         mssql = con_bib_mssql(),
         sqlite = con_bib_sqlite()
  )
}

#' Connection to Bibliometrics data source for KTH using MS SQL Server db
#' 
#' This function relies on an .Renviron file with environment variables for 
#' a connection to the MS SQL Server data source. Make sure one exists and 
#' that variables are set for: DBHOST, DBNAME, DBUSER, DBPASS
#' 
#' @import DBI odbc
#' @noRd
con_bib_mssql <- function() 
{
  envvars <- c("DBHOST", "DBNAME", "DBUSER", "DBPASS")
  
  if (any(Sys.getenv(envvars) == "")) {
    message("Do you have an .Renviron file at: ", normalizePath("~/.Renviron"), "?")
    stop("Please use an .Renviron with these envvars set", paste(envvars))
  }
  
  if (Sys.getenv("DBTIMEOUT") == "") {
    timeout <- 60
  } else {
    timeout <- strtoi(Sys.getenv("DBTIMEOUT"))
    is_valid <- !is.na(timeout)
    stopifnot(is_valid)
  }
  
  if(startsWith(Sys.getenv("OS"), "Windows")) {
    # encoding Windows-1252 curiously gives neat UTF-8 output from DB on Windows
    # (while encoding UTF-8 does not)
    dbConnect(
      odbc(),
      driver = "ODBC Driver 17 for SQL Server",
      Port = 1433,
      server = Sys.getenv("DBHOST"),
      database = Sys.getenv("DBNAME"),
      UID = Sys.getenv("DBUSER"),
      PWD = Sys.getenv("DBPASS"),
      timeout = timeout,
      encoding = "Windows-1252")
  } else {
    dbConnect(
      odbc(),
      driver = "ODBC Driver 17 for SQL Server",
      Port = 1433,
      server = Sys.getenv("DBHOST"),
      database = Sys.getenv("DBNAME"),
      UID = Sys.getenv("DBUSER"),
      PWD = Sys.getenv("DBPASS"),
      timeout = timeout)
  }
}

#' Connection to Bibliometrics data source for KTH using SQLite3 db
#' 
#' This function relies on a "bibmon.db" file being present in the relevant application
#' directory for a connection to the SQLite3 data source.
#' 
#' @import DBI RSQLite rappdirs
#' @importFrom rappdirs app_dir
#' @noRd
con_bib_sqlite <- function(create = FALSE, overwrite = FALSE) 
{
  db_path <- db_sqlite_location()
  
  if (!file.exists(db_path) & !create) 
    stop("No sqlite3 db available at ", db_path)
  
  if (file.exists(db_path) & create & !overwrite)
    stop("A file exists at ", db_path, ", use `overwrite` = TRUE to overwrite it.")
  
  if (file.exists(db_path) & create & overwrite) {
    message("Deleting database at ", db_path, ", creating new empty database there.")
    unlink(db_path)
  }
  
  if (!file.exists(dirname(db_path)) & create) {
    message("Creating local dir for sqlit3 db at ", dirname(db_path))
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  }
  
  sqliteflag <- if (create) RSQLite::SQLITE_RWC else RSQLite::SQLITE_RW
  #message("Using SQLit3 connection flag ", sqliteflag)
  DBI::dbConnect(RSQLite::SQLite(), synchronous = "normal", 
    dbname = db_path, flags = sqliteflag)

}

#' Summary with total row counts for a db connection and a set of tables
#' @import purrr dplyr
#' @noRd
db_counts <- function(con, tables) {
  
  # fcn to count nr of rows in a db table
  df_rowcount <- function(x) 
    tbl(con, x) %>% count() %>% collect() %>% 
    rename(n_rows = n) %>%
    mutate(table = x)
  
  # fcn to count nr of cols in a db table
  df_colcount <- function(x) tibble(
    n_cols = tbl(con, x) %>% ncol(),
    table = x
  )
  nr <- purrr::possibly(df_rowcount, otherwise = NULL, quiet = TRUE)
  nc <- purrr::possibly(df_colcount, otherwise = NULL, quiet = TRUE)
  
  # for all enumerated tables, count rows and cols
  n_rows <- map_df(tables, nr)
  n_cols <- map_df(tables, nc)
  
  # compile summary results  
  n_rows %>% 
    left_join(n_cols, by = "table") %>%
    select(table, everything()) %>%
    arrange(desc(n_rows))
  
}

db_tables <- function(con) {
  
  type <- class(con)[1]
  
  source_type <- switch(type, 
    "Microsoft SQL Server" = "mssql", 
    "SQLiteConnection" = "sqlite")
  
  if (!source_type %in% c("mssql", "sqlite"))
    stop("Only mssql and sqlite3 connections are supported.")
  
  # enumerate all tables in the BIBMON db
  enum_tables_mssql <- function() {
    con <- con_bib_mssql()
    tables <- odbc::dbListTables(
      con, catalog_name = "BIBMON", schema_name = "dbo", table_type = "table")
    if (!length(tables)) return(NULL)
    res <- db_counts(con, tables)    
    dbDisconnect(con)
    return(res)
  }
  
  # enumerate all tables in the SQLite db, excluding system tables
  enum_tables_sqlite <- function() {
    con <- con_bib_sqlite()
    mygrep <- function(x, pattern = "^sqlite_") 
      grep(x = x, pattern = pattern, invert = TRUE, value = TRUE)
    tables <- RSQLite::dbListTables(con) %>% mygrep()
    if (length(tables)) res <- db_counts(con, tables) else res <- NULL
    dbDisconnect(con)
    return (res)
  }
  
  switch(source_type,
         mssql = enum_tables_mssql(),
         sqlite = enum_tables_sqlite()
  )  
  
}


db_sync_table <- function(
  table, chunk_size = 1e4,
  con_src = con_bib_mssql(), 
  con_dest = con_bib_sqlite(),
  overwrite = FALSE)
{
  tables_src <- db_tables(con_src)$table
  tables_dest <- db_tables(con_dest)$table
  
  if (!table %in% tables_src)
    stop("Table ", table, " is not available in the source connection.")
  
  if (table %in% tables_dest & !overwrite)
    stop("Table ", table, " is in the destination connection, use `overwrite = TRUE`")
  
  if (table %in% tables_dest & overwrite)
    message("Table ", table, " will be overwritten at the destination connection")
  
  rc_sql <- sprintf("SELECT COUNT(*) as n FROM %s;", table)
  rc <- dbGetQuery(con_src, rc_sql) %>% as_vector()
  p <- progress_estimated(n = ceiling(rc / chunk_size))

  rs_sql <- sprintf("SELECT * FROM %s;", table)
  rs <- dbSendQuery(con_src, rs_sql)

  is_first_iter <- TRUE
  # HACK it seems the connection can auto-disconnect pretty quickly
  if (!RSQLite::dbIsValid(con_dest)) con_dest <- RSQLite::dbConnect(con_dest)
  if (overwrite) DBI::dbRemoveTable(con_dest, table)
  while (!dbHasCompleted(rs)) {
    chunk <- odbc::dbFetch(rs, chunk_size) %>% as_tibble()
    DBI::dbWriteTable(con_dest, table, chunk, append = TRUE)
    p$pause(0.1)$tick()$print()
    is_first_iter <- FALSE
  }
  odbc::dbClearResult(rs)
  
  odbc::dbDisconnect(con_src)
  DBI::dbDisconnect(con_dest)
}

#' Sync the MS SQL Server database BIBMON to a local SQLite3 db
#' 
#' This function syncs db tables from an mssql source db and
#' writes the data into a local SQLite3 db using buffering, with
#' chunk size set to 1e4 items per chunk, in order to avoid out of
#' memory exceptions when moving large tables.
#' 
#' @param tables_included a vector of table names in the source db to be 
#'   included, by default all tables are included except those excluded
#' @param tables_excluded a vector of table names in the source db to be
#'   excluded, by default a number of tables are excluded, specify NULL
#'   to not explicitly exclude any tables
#' @param overwrite_existing a logical to indicate whether destination tables
#'   should be overwritten if they already exist
#' @return invisible result with vector of boolean status flags for synced tables
#' @importFrom purrr map set_names
#' @importFrom odbc dbDisconnect
#' @importFrom DBI dbDisconnect
#' @importFrom stringr str_starts
#' @export
db_sync <- function(
  tables_included, 
  tables_excluded = c("OA_status", "Document", "Bestresaddr_KTH", "LastFailedJobs",
                      "DIVA_School_Dept", "Diva_departments", "Doc_statistics"),
  overwrite_existing = FALSE) 
{
  c1 <- con_bib_mssql()
  
  if (missing(tables_included)) {
    t1 <- c1 %>% db_tables() %>% pull(table)
  } else {
    t1 <- tables_included
  }
  
  tryCatch(
    c2 <- con_bib_sqlite(),
    error = function(e) {
      if (str_starts(e$message, "No sqlite3 db")) {
        message("No sqlite3 db exists, probably first run, so creating one.")
        c2 <- con_bib_sqlite(create = TRUE)
      }
    }
  )
  
  t2_df <- c2 %>% db_tables()
  t2 <- if (is.null(t2_df)) NULL else t2_df %>% pull(table)

  # inclusions  
  if (overwrite_existing)
    # which tables exist in both src and dest dbs?
    delta <- intersect(t1, t2)
  else 
    # which tables are new, ie only exist in src db?
    delta <- setdiff(t1, t2)

  # exclusions
  tables <- setdiff(delta, tables_excluded)
  
  # safe function for syncing tables
  sync_possibly <-  purrr::possibly(
    .f = function(x) db_sync_table(x, overwrite = overwrite_existing),
    otherwise = FALSE)

  # iterate over all tables for side-effects of synching
  message("excluded tables: ", paste(tables_excluded, collapse = ", "))
  message("syncing these tables from source db:\n", 
    if (length(tables)) paste(collapse = ", ", tables) else "none")
    
  res <- purrr::map_lgl(tables, sync_possibly)
  names(res) <- as.character(tables)

  odbc::dbDisconnect(c1)
  DBI::dbDisconnect(c2)
  invisible(res)
}

#' Location of sqlite3 db file for BIBMON data
#' 
#' @export
#' @return character string representing on disk location for db file
#' @import rappdirs
db_sqlite_location <- function() {
  file.path(rappdirs::app_dir("bibmon")$config(), "bibmon.db")
}

#' Connection pool to Bibliometrics data source for KTH
#' 
#' This function returns a db connection to one of two possible pre-configured
#' data sources containing Bibliometrics data
#' 
#' @param source_type one of "sqlite" or "mssql" with "sqlite" being default
#' @return database connection
#' @export
pool_bib <- function(source_type = c("sqlite", "mssql"))
{
  type <- match.arg(source_type)
  switch(type,
         sqlite = pool_bib_sqlite(),
         mssql = pool_bib_mssql()
  )
}

#' Connection pool to Bibliometrics data source for KTH using MS SQL Server db
#' 
#' This function relies on an .Renviron file with environment variables for 
#' a connection to the MS SQL Server data source. Make sure one exists and 
#' that variables are set for: DBHOST, DBNAME, DBUSER, DBPASS
#' 
#' @import DBI odbc pool
#' @noRd
pool_bib_mssql <- function() {
  envvars <- c("DBHOST", "DBNAME", "DBUSER", "DBPASS")
  
  if (any(Sys.getenv(envvars) == "")) {
    message("Do you have an .Renviron file at: ", normalizePath("~/.Renviron"), "?")
    stop("Please use an .Renviron with these envvars set", paste(envvars))
  }
  
  if(startsWith(Sys.getenv("OS"), "Windows")) {
    # encoding Windows-1252 curiously gives neat UTF-8 output from DB on Windows
    # (while encoding UTF-8 does not)
    dbPool(
      odbc(),
      driver = "ODBC Driver 17 for SQL Server",
      Port = 1433,
      server = Sys.getenv("DBHOST"),
      database = Sys.getenv("DBNAME"),
      UID = Sys.getenv("DBUSER"),
      PWD = Sys.getenv("DBPASS"),
      timeout = 30,
      encoding = "Windows-1252")
  } else {
    dbPool(
      odbc(),
      driver = "ODBC Driver 17 for SQL Server",
      Port = 1433,
      server = Sys.getenv("DBHOST"),
      database = Sys.getenv("DBNAME"),
      UID = Sys.getenv("DBUSER"),
      PWD = Sys.getenv("DBPASS"),
      timeout = 30)
  }
}

#' Connection pool to Bibliometrics data source for KTH using SQLite3 db
#' 
#' This function relies on a "bibmon.db" file being present in the relevant application
#' directory for a connection to the SQLite3 data source.
#' 
#' @import DBI RSQLite rappdirs pool
#' @importFrom rappdirs app_dir
#' @noRd
pool_bib_sqlite <- function(create = FALSE, overwrite = FALSE) {
  db_path <- db_sqlite_location()
  
  if (!file.exists(db_path) & !create) 
    stop("No sqlite3 db available at ", db_path)
  
  if (file.exists(db_path) & create & !overwrite)
    stop("A file exists at ", db_path, ", use `overwrite` = TRUE to overwrite it.")
  
  if (file.exists(db_path) & create & overwrite) {
    message("Deleting database at ", db_path, ", creating new empty database there.")
    unlink(db_path)
  }
  
  if (!file.exists(dirname(db_path)) & create) {
    message("Creating local dir for sqlit3 db at ", dirname(db_path))
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  }
  sqliteflag <- if (create) RSQLite::SQLITE_RWC else RSQLite::SQLITE_RW

  dbPool(
    drv = RSQLite::SQLite(),
    dbname = db_path,
    synchronous = "normal",
    flags = sqliteflag
  )
}
