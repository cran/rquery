## ------------------------------------------------------------------------
col_string <- "x"
col_name <- as.name(col_string)

## ------------------------------------------------------------------------
str(col_string)

## ------------------------------------------------------------------------
str(col_name)

## ------------------------------------------------------------------------
library("rquery")

d <- data.frame(x = c('a', 'b'),
                stringsAsFactors = FALSE)

d_rep <- local_td(d)

db_info <- rquery_db_info(identifier_quote_char = "__IDENTIFIER__",
                          string_quote_char = "__STRING_CONSTANT__")

## ------------------------------------------------------------------------
# direct use, comparing to a string constant
# probaly not the query we intend as the
# result is going to be empty independent
# of the data.
cat(to_sql(
  d_rep %.>% select_rows(., is.na('x')),
  db_info))

## ------------------------------------------------------------------------
# direct use, comparing to a column
cat(to_sql(
  d_rep %.>% select_rows(., is.na(x)),
  db_info))

## ------------------------------------------------------------------------
cat(to_sql(
  d_rep %.>% select_rows(., x == 'a'),
  db_info))

## ------------------------------------------------------------------------
# Let substitution treats all substitutions as source-text
# so strings and names are as if the user had typed them
# in and behave as names (becoming the name of a column).
let(c(COL_STRING = col_string),
    cat(to_sql(d_rep %.>% select_rows(., is.na(COL_STRING)),
               db_info)))

## ------------------------------------------------------------------------
# Let substitution treats all substitutions as source-text
# so strings and names are as if the user had typed them
# in and behave as names (becoming the name of a column).
let(c(COL_NAME = col_name),
    cat(to_sql(d_rep %.>% select_rows(., is.na(COL_NAME)),
               db_info)))

## ------------------------------------------------------------------------
value_we_want <- "a"

let(c(COL_NAME = col_name),
    cat(to_sql(d_rep %.>% select_rows(., COL_NAME == value_we_want),
               db_info)))

## ------------------------------------------------------------------------
# bquote substitution on string type: col_string 
# is taken to represent a string constant, not
# the name of a column.
cat(to_sql(d_rep %.>% select_rows(., is.na(.(col_string))),
           db_info))

## ------------------------------------------------------------------------
# bquote substitution on name type: col_name 
# is taken to represent a column name.
cat(to_sql(d_rep %.>% select_rows(., is.na(.(col_name))),
           db_info))

## ------------------------------------------------------------------------
# bquote substitution on string type: col_string 
# is taken to represent a string constant, not
# the name of a column.
cat(to_sql(d_rep %.>% select_rows_se(., qe(is.na(.(col_string)))),
           db_info))

## ------------------------------------------------------------------------
# bquote substitution on name type: col_name 
# is taken to represent a column name.
cat(to_sql(d_rep %.>% select_rows_se(., qe(is.na(.(col_name)))),
           db_info))

