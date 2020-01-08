## -----------------------------------------------------------------------------
library(wrapr)
library(rquery)
have_rqdatatable <- requireNamespace('rqdatatable', quietly = TRUE)

example_data <- wrapr::build_frame(
   "mpg"  , "cyl", "disp",  "car"               |
     21   , 6    , 160   ,  "Mazda RX4"         |
     21   , 6    , 160   ,  "Mazda RX4 Wag"     |
     22.8 , 4    , 108   ,  "Datsun 710"        |
     21.4 , 6    , 258   ,  "Hornet 4 Drive"    |
     18.7 , 8    , 360   ,  "Hornet Sportabout" |
     18.1 , 6    , 225   ,  "Valiant"           )

knitr::kable(example_data)

## -----------------------------------------------------------------------------
vars <- setdiff(colnames(example_data), 'car')

print(vars)

## -----------------------------------------------------------------------------
expressions <- vars := paste0(vars, ' - mean(', vars, ')')

print(expressions)

## -----------------------------------------------------------------------------
ops <- local_td(example_data) %.>%
  extend_se(., expressions)

cat(format(ops))

## -----------------------------------------------------------------------------
if(have_rqdatatable) {
  example_data %.>%
    ops %.>%
    knitr::kable(.)
}

## -----------------------------------------------------------------------------
ops <- local_td(example_data) %.>%
  extend_se(., vars := paste0(vars, ' - mean(', vars, ')'))

cat(format(ops))

## -----------------------------------------------------------------------------
combos <- t(combn(vars, 2))
interactions <- 
  paste0(combos[, 1], '_', combos[, 2]) := 
  paste0(combos[, 1], ' * ', combos[, 2])

print(interactions)

## -----------------------------------------------------------------------------
ops <- local_td(example_data) %.>%
  extend_se(., interactions)

cat(format(ops))

## -----------------------------------------------------------------------------
if(have_rqdatatable) {
  example_data %.>%
    ops %.>%
    knitr::kable(.)
}

## -----------------------------------------------------------------------------
have_db <- requireNamespace("DBI", quietly = TRUE) && 
  requireNamespace("RSQLite", quietly = TRUE)

if(have_db) {
  raw_connection <- DBI::dbConnect(RSQLite::SQLite(), 
                                   ":memory:")
  RSQLite::initExtension(raw_connection)
  db <- rquery_db_info(
    connection = raw_connection,
    is_dbi = TRUE,
    connection_options = rq_connection_tests(raw_connection))
  
  rq_copy_to(db, 'example_data',
             example_data,
             temporary = TRUE, 
             overwrite = TRUE)
  
  sql <- to_sql(ops, db)
  
  cat(format(sql))
}

## -----------------------------------------------------------------------------
if(have_db) {
  res_table <- materialize(db, ops)
  DBI::dbReadTable(raw_connection, res_table$table_name) %.>%
    knitr::kable(.)
}

## -----------------------------------------------------------------------------
if(have_db) {
  DBI::dbDisconnect(raw_connection)
}

