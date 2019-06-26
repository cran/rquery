## ------------------------------------------------------------------------
library("rquery")
library("wrapr")

show_translation <- function(strings) {
  vapply(strings,
         function(si) {
           format(rquery::tokenize_for_SQL(parse(text = si, keep.source = FALSE)[[1]], colnames = NULL)$parsed_toks)
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
have_RSQLite <- requireNamespace("RSQLite", quietly = TRUE)

## ---- eval=have_RSQLite--------------------------------------------------
raw_RSQLite_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
RSQLite::initExtension(raw_RSQLite_connection)
db <- rquery_db_info(
  connection = raw_RSQLite_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(raw_RSQLite_connection))


fn_name_map <- db$connection_options[[paste0("rquery.", rq_connection_name(db), ".", "fn_name_map")]]
fn_name_map

## ---- eval=have_RSQLite--------------------------------------------------
d_local <- build_frame(
   "subjectID", "surveyCategory"     , "assessmentTotal", "irrelevantCol1", "irrelevantCol2" |
   1L         , "withdrawal behavior", 5                , "irrel1"        , "irrel2"         |
   1L         , "positive re-framing", 2                , "irrel1"        , "irrel2"         |
   3L         , "withdrawal behavior", 3                , "irrel1"        , "irrel2"         |
   3L         , "positive re-framing", 2                , "irrel1"        , "irrel2"         |
   3L         , "other"              , 1                , "irrel1"        , "irrel2"         )
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

## ---- eval=have_RSQLite--------------------------------------------------
rquery::rq_function_mappings(db) %.>%
  knitr::kable(.)

## ---- eval=have_RSQLite--------------------------------------------------
ops <- table_handle %.>% 
  project(., groupby = "subjectID",
          n := 5, 
          count := n(),
          mean := mean(assessmentTotal)) %.>% 
  extend(., was_n := n)
                 
cat(to_sql(ops, db))

ops %.>%
  execute(db, .) %.>%
  knitr::kable(.)

## ---- eval=have_RSQLite--------------------------------------------------
ops <- table_handle %.>% 
  extend(., z := 1 + subjectID %% 3) %.>%
  select_columns(., c("subjectID", "z"))
                 
cat(to_sql(ops, db))

ops %.>%
  execute(db, .) %.>%
  knitr::kable(.)

## ---- eval=have_RSQLite--------------------------------------------------
DBI::dbDisconnect(raw_RSQLite_connection)

