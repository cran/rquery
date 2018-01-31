## ----setup---------------------------------------------------------------
library("rquery")
db = DBI::dbConnect(RSQLite::SQLite(), 
                    ":memory:")
RSQLite::initExtension(db)

DBI::dbWriteTable(db,
                  'd',
                  data.frame(AUC = 0.6, 
                             R2 = c(0.1, 0.2), 
                             D = NA, z = 2),
                  overwrite = TRUE,
                  temporary = TRUE)
d <- dbi_table(db, 'd')
print(d)

qlook(db, d$table_name)

## ----q1------------------------------------------------------------------
q <- d %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2)) %.>%
  select_columns(., c("AUC", "R2", "c"))

## ----q1p, comment=""-----------------------------------------------------
cat(format(q))

## ----q1q-----------------------------------------------------------------
column_names(q)

tables_used(q)

columns_used(q)

## ----q1s, comment=""-----------------------------------------------------
sql <- to_sql(q, db)
cat(sql)

## ----q1e-----------------------------------------------------------------
DBI::dbGetQuery(db, sql) %.>%
  knitr::kable(.)

## ----defdb---------------------------------------------------------------
winvector_temp_db_handle <- list(db = db)

## ----df1-----------------------------------------------------------------
dL <-  data.frame(AUC = 0.6, 
                  R2 = c(0.1, 0.2), 
                  D = NA, z = 2)

dL %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  knitr::kable(.)

dL %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2))  %.>%
  select_columns(., c("AUC", "R2", "c")) %.>%
  knitr::kable(.)

## ----dfahs---------------------------------------------------------------
q2 <- . := {
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2)) %.>%
  select_columns(., c("AUC", "R2", "c"))
}

dL %.>% 
  q2 %.>%
  knitr::kable(.)

## ----s3redef-------------------------------------------------------------
#' Pipe step operator (wrapr 1.2.0 version, wrapr 1.1.1 uses a function value).
#'
#' @param pipe_left_arg pipe_left_arg argument
#' @param pipe_right_arg substitute(pipe_right_arg) argument
#' @param pipe_environment environment to evaluate in
#' @return result
#'
#'
wrapr_function.relop <- function(pipe_left_arg, pipe_right_arg,
                                 pipe_environment) {
  rquery_apply_to_data_frame(pipe_left_arg, pipe_right_arg,
                             pipe_environment)
}

## ----dm------------------------------------------------------------------
needed_columns <- columns_used(q)
print(needed_columns)

q3 <- table_source(table_name = 'tmp', 
                   columns = needed_columns$d) %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2)) %.>%
  select_columns(., c("AUC", "R2", "c"))

dL %.>% 
  q3 %.>%
  knitr::kable(.)

## ----dmuseq--------------------------------------------------------------
DBI::dbExecute(db, "DROP TABLE d")

dL %.>% 
  q %.>%
  knitr::kable(.)

## ----adhocp1-------------------------------------------------------------
z <- dL %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2))  %.>%
  select_columns(., c("AUC", "R2", "c"))

class(z)

## ----adhocp2, comment=""-------------------------------------------------

cat(format(z))

cat(to_sql(z, db))

## ----adhocqp-------------------------------------------------------------
print(z)

as.data.frame(z)

## ----adhocqk-------------------------------------------------------------
knitr::kable(z)

## ----qpiplelinea---------------------------------------------------------
class(q)

## ----qpiplelineaf, comment=""--------------------------------------------
cat(format(q))

## ----qexq----------------------------------------------------------------
dL %.>% 
  q %.>%
  knitr::kable(.)

## ----cleanup-------------------------------------------------------------
DBI::dbDisconnect(winvector_temp_db_handle$db)
winvector_temp_db_handle <- NULL

