## ------------------------------------------------------------------------
library("rquery")
library("wrapr")

show_translation <- function(strings) {
  vapply(strings,
         function(si) {
           format(rquery::tokenize_for_SQL(str2lang(si), colnames = NULL)$parsed_toks)
         }, character(1))
}

mapping_table <- data.frame(
  example = c('!x', 'is.na(x)', 'ifelse(a, b, c)', 'a^b', 'a%%b', 
               'a==b', 'a&&b', 'a&b', 'a||b', 'a|b', 
              'pmin(a, b)', 'pmax(a, b)'),
  stringsAsFactors = FALSE)
mapping_table$translation <- show_translation(mapping_table$example)
knitr::kable(mapping_table)

## ------------------------------------------------------------------------
raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
RSQLite::initExtension(raw_connection)
db <- rquery_db_info(
  connection = raw_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(raw_connection))

# RSQLite has a non-standard modulo operator
db$expr_map[["MOD"]] <- list(pre_sql_token("("),
                           3,
                           pre_sql_token("%"),
                           5,
                           pre_sql_token(")"))

fn_name_map <- db$connection_options[[paste0("rquery.", rq_connection_name(db), ".", "fn_name_map")]]
fn_name_map

## ------------------------------------------------------------------------
d_local <- build_frame(
   "subjectID", "surveyCategory"     , "assessmentTotal", "irrelevantCol1", "irrelevantCol2" |
   1L         , "withdrawal behavior", 5                , "irrel1"        , "irrel2"         |
   1L         , "positive re-framing", 2                , "irrel1"        , "irrel2"         |
   3L         , "withdrawal behavior", 3                , "irrel1"        , "irrel2"         |
   3L         , "positive re-framing", 4                , "irrel1"        , "irrel2"         )
table_handle <- rq_copy_to(db, 'd',
            d_local,
            temporary = TRUE, 
            overwrite = TRUE)
print(table_handle)

ops <- table_handle %.>% 
  project(., 
          avg_total := avg(pmax(0, assessmentTotal)),
          groupby = "subjectID")

cat(to_sql(ops, db))

ops %.>%
  execute(db, .) %.>%
  knitr::kable(.)

## ------------------------------------------------------------------------
rquery::rq_function_mappings(db) %.>%
  knitr::kable(.)

## ------------------------------------------------------------------------
table_handle %.>% extend(., z := subjectID %% 3) -> ops
cat(to_sql(ops, db))
execute(db, ops)

## ------------------------------------------------------------------------
DBI::dbDisconnect(raw_connection)

