# persist to local storage

library(duckdb)
library(dplyr)

hr <- file.path(rappdirs::app_dir("abm")$config(), "hr")
if (!dir.exists(hr)) dir.create(hr, recursive = TRUE)
db <- file.path(hr, "hr.duck")

con <- dbConnect(duckdb::duckdb(), dbdir = db, read_only = FALSE)

# TODO: add fcn here
data <- readRDS("~/researchers.rds")
if (dbExistsTable(con, "researchers")) db_drop_table(con, "researchers")
con %>% dbWriteTable(name = "researchers", data)

# TODO: add fcn here
divisions <- readRDS("~/divs.rds")
if (dbExistsTable(con, "divisions")) db_drop_table(con, "divisions")
con %>% dbWriteTable(name = "divisions", divisions)

# TODO: add fcn here
deps <- readRDS("~/deps.rds")
if (dbExistsTable(con, "deps")) db_drop_table(con, "deps")
con %>% dbWriteTable(name = "deps", deps)

# inspect results
con %>% tbl("researchers") %>% collect()
con %>% tbl("divisions") %>% collect()
con %>% tbl("deps") %>% collect()
con %>% db_drop_table(table = "desc", force = TRUE)
con %>% dbListTables()

# export data in parquet format
tmpdir <- normalizePath("~/ducky")
if (!dir.exists(tmpdir)) dir.create(tmpdir)

sql_export <- 
  sprintf("EXPORT DATABASE '%s' (FORMAT PARQUET)", tmpdir)

sql_import <- 
  sprintf("IMPORT DATABASE '%s'", tmpdir)

con %>% dbExecute(sql_export)

dir(tmpdir, all.files = TRUE)
con %>% dbExecute(import)

dbDisconnect(con, shutdown = TRUE)

# Various SQL statement supported in duckdb (uses postgres query parser)

# load from csv files
# "CREATE TABLE ontime AS SELECT * FROM read_csv_auto('test.csv');"
# "COPY weather FROM '/home/user/weather.csv';"
# "EXPORT DATABASE 'target_directory' (FORMAT PARQUET);"

# -- list all databases, usually one
# PRAGMA database_list;
# -- list all tables
# PRAGMA show_tables;
# -- get info for a specific table
# PRAGMA table_info('table_name');
# 
# -- set the memory limit
# PRAGMA memory_limit='1GB';
# -- set the amount of threads for parallel query execution
# PRAGMA threads=4;


# example duckdb query using EXTRACT in GROUP BY
# SELECT
# MIN(cnt),
# AVG(cnt),
# MAX(cnt)
# FROM
# (
#   SELECT 
#   COUNT(*) as cnt
#   FROM table
#   GROUP BY  
#   EXTRACT(DOY FROM event_datetime::DATE),
#   EXTRACT(HOUR FROM event_datetime)
# ) stats

#tidync:::hyper_array.tidync
#tidync:::hyper_tbl_cube.tidync
