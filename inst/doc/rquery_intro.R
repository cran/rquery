## ----setup---------------------------------------------------------------
library("rquery")

# example database connection
db <- DBI::dbConnect(RSQLite::SQLite(),
                     ":memory:")
RSQLite::initExtension(db)

# example data
d <- dbi_copy_to(
  db, 'd',
  data.frame(v = c(1, -5, 3)),
  temporary = FALSE,
  overwrite = TRUE)

## ----defcoltwice---------------------------------------------------------
DBI::dbGetQuery(db, "
  SELECT
    *,
    ABS(v) AS absv,
    ABS(v) - v AS delta
  FROM
    d
")

## ----nestsql-------------------------------------------------------------
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

## ----rquerypipe1---------------------------------------------------------
op_tree <- d %.>%
  sql_node(., "absv" := "ABS(v)") %.>%
  sql_node(., "delta" := "absv - v")
execute(db, op_tree)

## ----printsql, comment=""------------------------------------------------
cat(to_sql(op_tree, db))

## ----rquerypipe11, comment=""--------------------------------------------
op_tree <- d %.>%
  sql_node(., "absv" := list(list("ABS(", quote(v), ")"))) %.>%
  sql_node(., "delta" := list(list(quote(absv),"-", quote(v))))
cat(to_sql(op_tree, db))

## ----printoptree---------------------------------------------------------
cat(format(op_tree))

## ----opsummaries---------------------------------------------------------
column_names(op_tree)

tables_used(op_tree)

columns_used(op_tree)

## ----addop---------------------------------------------------------------
op_tree <- op_tree %.>%
  sql_node(., "prod" := "absv * delta")

cat(format(op_tree))

## ----addoperror, error=TRUE----------------------------------------------
op_tree <- op_tree %.>%
  sql_node(., "z" := list(list("1 + ", quote(z))))

## ----countna-------------------------------------------------------------
# load up example data
d2 <- dbi_copy_to(
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

## ----countna2------------------------------------------------------------
# whole process wrapped in convenience node
d2 %.>%
  count_null_cols(., vars, "nnull") %.>%
  execute(db, .)

## ----execd---------------------------------------------------------------
winvector_temp_db_handle <- list(db = db)

data.frame(v = -2:2) %.>%
  execute(., op_tree)

## ----rwpipe--------------------------------------------------------------
data.frame(v = -2:2) %.>% op_tree

## ----adhocops------------------------------------------------------------
data.frame(x = 5) %.>% sql_node(., "z" := "sqrt(x)")

## ----isna----------------------------------------------------------------
d %.>% 
  extend_nse(., was_na := ifelse(is.na(v), 1, 0)) %.>%
  to_sql(., db) %.>%
  cat(.)

## ----logisticex----------------------------------------------------------
scale <- 0.237

dq <- table_source("d3", 
                   columns = qc(subjectID, 
                                surveyCategory, 
                                assessmentTotal)) %.>%
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale)/
               sum(exp(assessmentTotal * scale)),
             count := count(1),
             partitionby = 'subjectID') %.>%
  extend_nse(.,
             rank := rank(),
             partitionby = 'subjectID',
             orderby = c('probability', 
                         'surveyCategory'))  %.>%
  rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
  select_rows_nse(., rank == count) %.>%
  select_columns(., c('subjectID', 
                      'diagnosis', 
                      'probability')) %.>%
  orderby(., 'subjectID')

## ----logprops------------------------------------------------------------
tables_used(dq)

columns_used(dq)

column_names(dq)

## ----rsummaryex----------------------------------------------------------
op_tree %.>%
  rsummary_node(.) %.>%
  execute(db, .)

## ----assignmentpart------------------------------------------------------
ot <- table_source('d4',
                   columns = qc('a', 'b', 'c', 'd')) %.>%
  extend_nse(., 
             x = a + 1,
             y = x + 1,
             u = b + 1,
             v = c + 1,
             w = d + 1)

cat(format(ot))

## ----ifelseblock---------------------------------------------------------
ifet <- table_source("d5",
                     columns = "test") %.>%
  extend_se(.,
            c(qae(x = '',
                  y = ''),
              if_else_block(
                qe(test > 5),
                thenexprs = qae(x = 'a', 
                                y = 'b'),
                elseexprs = qae(x = 'b', 
                                y = 'a')
              )))
cat(format(ifet))

## ------------------------------------------------------------------------
wp <- table_source(table = 'd6',
                   columns = letters[1:5]) %.>%
  extend_nse(., res := a + b)

# full query
cat(to_sql(wp, db))

# longer pipeline
wn <- wp %.>%
  select_columns(., "res")

# notice select at end of the pipeline automatically 
# gets propagated back to the beginning of the
# pipeline
cat(to_sql(wn, db))

## ----cleanup-------------------------------------------------------------
rm(list = "winvector_temp_db_handle")
DBI::dbDisconnect(db)

