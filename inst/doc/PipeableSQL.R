## ----chpkg---------------------------------------------------------------
run_vignette <- requireNamespace("DBI", quietly = TRUE) && 
  requireNamespace("RSQLite", quietly = TRUE)

## ----setup, eval=run_vignette--------------------------------------------
library("rquery")
library("wrapr")

# example database connection
db <- DBI::dbConnect(RSQLite::SQLite(),
                     ":memory:")
RSQLite::initExtension(db)

dbopts <- rq_connection_tests(db)
print(dbopts)
options(dbopts)

# copy in example data
rq_copy_to(
  db, 'd',
  data.frame(v = c(1, -5, 3)),
  temporary = FALSE,
  overwrite = TRUE)

# produce a hande to existing table
d <- db_td(db, "d")

## ----defcoltwice, eval=run_vignette--------------------------------------
DBI::dbGetQuery(db, "
  SELECT
    *,
    ABS(v) AS absv,
    ABS(v) - v AS delta
  FROM
    d
")

## ----nestsql, eval=run_vignette------------------------------------------
DBI::dbGetQuery(db, "
  SELECT
    *,
    absv - v AS delta
  FROM (
    SELECT
      *,
      ABS(v) AS absv
    FROM
      d
  ) subtab
")

## ----rquerypipe1, eval=run_vignette--------------------------------------
op_tree <- d %.>%
  sql_node(., "absv" := "ABS(v)") %.>%
  sql_node(., "delta" := "absv - v")
execute(db, op_tree)

## ----printsql, comment="", eval=run_vignette-----------------------------
cat(to_sql(op_tree, db))

## ----rquerypipe11, comment="", eval=run_vignette-------------------------
op_tree <- d %.>%
  sql_node(., "absv" := list(list("ABS(", quote(v), ")"))) %.>%
  sql_node(., "delta" := list(list(quote(absv),"-", quote(v))))
cat(to_sql(op_tree, db))

## ----printoptree, eval=run_vignette--------------------------------------
cat(format(op_tree))

## ----opsummaries, eval=run_vignette--------------------------------------
column_names(op_tree)

tables_used(op_tree)

columns_used(op_tree)

## ----addop, eval=run_vignette--------------------------------------------
op_tree <- op_tree %.>%
  sql_node(., "prod" := "absv * delta")

cat(format(op_tree))

## ----addoperror, error=TRUE, eval=run_vignette---------------------------
op_tree <- op_tree %.>%
  sql_node(., "z" := list(list("1 + ", quote(z))))

## ----countna, eval=run_vignette------------------------------------------
# load up example data
d2 <- rq_copy_to(
  db, 'd2',
  data.frame(v1 = c(1, 2, NA, 3),
             v2 = c(NA, "b", NA, "c"),
             v3 = c(NA, NA, 7, 8),
             stringsAsFactors = FALSE))

# look at table
execute(db, d2)

# get list of columns
vars <- column_names(d2)
print(vars)

# build a NA/NULLs per-row counting expression.
# names are "quoted" by wrapping them with as.name().
# constants can be quoted by an additional list wrapping.
expr <- lapply(vars,
               function(vi) {
                 list("+ (CASE WHEN (",
                      as.name(vi),
                      "IS NULL ) THEN 1.0 ELSE 0.0 END)")
               })
expr <- unlist(expr, recursive = FALSE)
expr <- c(list(0.0), expr)
cat(paste(unlist(expr), collapse = " "))

# instantiate the operator node
op_tree_count_null <- d2 %.>%
  sql_node(., "num_missing" := list(expr))
cat(format(op_tree_count_null))

# examine produced SQL
sql <- to_sql(op_tree_count_null, db)
cat(sql)

# execute
execute(db, op_tree_count_null)

## ----countna2, eval=run_vignette-----------------------------------------
# whole process wrapped in convenience node
d2 %.>%
  count_null_cols(., vars, "nnull") %.>%
  execute(db, .)

## ----psql, eval=run_vignette---------------------------------------------
# vector of columns we want to work on
colset <- qc(v1, v2, v3)
# build new names we want as results
colterms <- paste0(colset, "_isNA") := colset
map_to_char(colterms)

# build an apply expression to set of columns query 
s_tree <- d2 %.>%
  sql_expr_set(., colterms, 
               "CASE WHEN . IS NULL THEN 1 ELSE 0 END")
cat(to_sql(s_tree, db))
execute(db, s_tree)

## ----execd, eval=run_vignette--------------------------------------------
old_o <- options(list("rquery.rquery_db_executor" = list(db = db)))

data.frame(v = -2:2) %.>%
  execute(., op_tree)

## ----rwpipe, eval=run_vignette-------------------------------------------
data.frame(v = -2:2) %.>% op_tree

## ----adhocops, eval=run_vignette-----------------------------------------
data.frame(x = 5) %.>% sql_node(., "z" := "sqrt(x)")

## ----cleanup, eval=run_vignette------------------------------------------
options(old_o)
DBI::dbDisconnect(db)

