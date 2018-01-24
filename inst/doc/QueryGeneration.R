## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ex, warning=FALSE, message=FALSE, include=FALSE---------------------
library("rquery")
use_spark = FALSE

# this db does not have window fns
my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

d <- dbi_copy_to(my_db, 'd',
                 data.frame(
                   subjectID = c(1,                   
                                 1,
                                 2,                   
                                 2),
                   surveyCategory = c(
                     'withdrawal behavior',
                     'positive re-framing',
                     'withdrawal behavior',
                     'positive re-framing'
                   ),
                   assessmentTotal = c(5,                 
                                       2,
                                       3,                  
                                       4),
                   irrelevantCol1 = "irrel1",
                   irrelevantCol2 = "irrel2",
                   stringsAsFactors = FALSE),
                 temporary = TRUE, 
                 overwrite = !use_spark)

## ----calc----------------------------------------------------------------
scale <- 0.237

dq <- d %.>%
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale)/
               sum(exp(assessmentTotal * scale)),
             count := count(1),
             partitionby = 'subjectID') %.>%
  extend_nse(.,
             rank := rank(),
             partitionby = 'subjectID',
             orderby = c('probability', 'surveyCategory'))  %.>%
  rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
  select_rows_nse(., rank == count) %.>%
  select_columns(., c('subjectID', 
                      'diagnosis', 
                      'probability')) %.>%
  orderby(., 'subjectID')

class(my_db)

sql <- to_sql(dq, db = my_db, source_limit = 1000)

## ----res, echo=FALSE, comment = ' '--------------------------------------
cat(sql)

## ----cleanup, include=FALSE----------------------------------------------
if(use_spark) {
  sparklyr::spark_disconnect(my_db)
} else {
  DBI::dbDisconnect(my_db)
}

