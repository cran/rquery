
#' Make a natural_join node.
#'
#' Natural join is a join by identity on all common columns specified in the \code{by}
#' argument.
#' Any common columns not specified in the \code{by} argument
#' are coalesced into a single column preferring the first or "a" table.
#'
#' @param a source to select from.
#' @param b source to select from.
#' @param ... force later arguments to bind by name
#' @param by character, set of columns to match.  If by is a named character vector the right table will have columns renamed.
#' @param jointype type of join ('INNER', 'LEFT', 'RIGHT', 'FULL').
#' @param env environment to look to.
#' @return natural_join node.
#'
#' @examples
#'
#' if(requireNamespace("DBI", quietly = TRUE) &&
#'    requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'
#'   d1 <- rq_copy_to(
#'     my_db, 'd1',
#'     build_frame(
#'       "key", "val", "val1" |
#'         "a"  , 1  ,  10    |
#'         "b"  , 2  ,  11    |
#'         "c"  , 3  ,  12    ))
#'   d2 <- rq_copy_to(
#'     my_db, 'd2',
#'     build_frame(
#'       "key", "val", "val2" |
#'         "a"  , 5  ,  13    |
#'         "b"  , 6  ,  14    |
#'         "d"  , 7  ,  15    ))
#'
#'   # key matching join
#'   optree <- natural_join(d1, d2,
#'                          jointype = "LEFT", by = 'key')
#'   execute(my_db, optree) %.>%
#'     print(.)
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
natural_join <- function(a, b,
                         ...,
                         by,
                         jointype = 'INNER',
                         env = parent.frame()) {
  force(env)
  UseMethod("natural_join", a)
}


# check left/right table consistency (name implies table)
tables_are_consistent <- function(a, b = NULL) {
  tabs <- find_all_tables(a)
  if(!is.null(b)) {
    tabs <- c(tabs, find_all_tables(b))
  }
  if(length(tabs)<=1) {
    return(TRUE)
  }
  seen <- list()
  for(ti in tabs) {
    prev <- seen[[ti$table_name]]
    if(is.null(prev)) {
      seen[ti$table_name] <- list(ti)
    } else {
      if(!isTRUE(all.equal(column_names(ti), column_names(prev)))) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#' @export
natural_join.relop <- function(a, b,
                               ...,
                               by,
                               jointype = 'INNER',
                               env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::natural_join.relop")
  if(!("relop" %in% class(a))) {
    stop("rquery::natural_join.relop a must be of class relop")
  }
  if(!("relop" %in% class(b))) {
    stop("rquery::natural_join.relop b must also be of class relop")
  }
  if(!tables_are_consistent(a, b)) {
    stop("rquery::natural_join.relop all tables with matching names must be identical")
  }
  if((length(by)>0) && (length(names(by))>0)) { # see if we want to rename right table colums
    rght <- as.character(by) # strip names
    by <- names(by) # new naming
    if(length(by)!=length(rght)) {
      stop("rquery::natural_join.relop when `by` is a named vector the number of names must equal the number of values")
    }
    deltas <- by!=rght
    if(sum(deltas)>0) {
      cmap <- rght[deltas]
      names(cmap) <- by[deltas]
      b <- rquery::rename_columns(b, cmap, env = env)
    }
  }
  usesa <- column_names(a)
  usesb <- column_names(b)
  common <- intersect(usesa, usesb)
  if(length(by)>0) {
    bads <- setdiff(by, common)
    if(length(bads)>0) {
      stop(paste("rquery::natural_join.relop all tables must have all join keys, the following keys are not in some tables:",
                 paste(bads, collapse = ", ")))
    }
  }
  r <- list(source = list(a, b),
            table_name = NULL,
            parsed = NULL,
            by = by,
            jointype = jointype)
  r <- relop_decorate("relop_natural_join", r)
  r
}

#' @export
natural_join.data.frame <- function(a, b,
                                    ...,
                                    by,
                                    jointype = 'INNER',
                                    env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::natural_join.data.frame")
  if(!is.data.frame(b)) {
    stop("rquery::natural_join.data.frame b must also be a data.frame")
  }
  nmgen <- mk_tmp_name_source("rquery_tmp")
  tmp_namea <- nmgen()
  dnodea <- mk_td(tmp_namea, colnames(a))
  tmp_nameb <- nmgen()
  dnodeb <- mk_td(tmp_nameb, colnames(b))
  enode <- natural_join(dnodea, dnodeb,
                        jointype = jointype,
                        by = by,
                        env = env)
  source <- list(a = a, b = b)
  names(source) <- c(tmp_namea, tmp_nameb)
  rquery_apply_to_data_frame(source, enode, env = env)
}


#' @export
format_node.relop_natural_join <- function(node) {
  paste0("natural_join(.1, .2,",
         "  j= ",
         node$jointype,
         ", by= ",
         paste(node$by, collapse = ", "),
         ")",
         "\n")
}


#' @export
format.relop_natural_join <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "format.relop_natural_join")
  a <- trimws(format(x$source[[1]]), which = "right")
  b <- trimws(format(x$source[[2]]), which = "right")
  b <- gsub("\n", "\n  ", b, fixed = TRUE)
  paste0(a,
         " %.>%\n ",
         "natural_join(.,\n",
         "  ", b, ",\n",
         "  jointype = \"",
         x$jointype,
         "\", by = ",
         wrapr::map_to_char(x$by),
         ")",
         "\n")
}



calc_used_relop_natural_join <- function (x, ...,
                                          using = NULL) {
  cols <- column_names(x)
  if(length(using)>0) {
    missing <- setdiff(using, cols)
    if(length(missing)>0) {
      stop(paste("rquery::calc_used_relop_natural_join unkown columns",
                 paste(missing, collapse = ", ")))

    }
    cols <- using
  }
  cols <- unique(c(cols, x$by))
  cols
}

#' @export
columns_used.relop_natural_join <- function (x, ...,
                                             using = NULL) {
  using <- calc_used_relop_natural_join(x,
                                        using=using)
  c1 <- intersect(using, column_names(x$source[[1]]))
  s1 <- columns_used(x$source[[1]],
                     using = c1)
  c2 <- intersect(using, column_names(x$source[[2]]))
  s2 <- columns_used(x$source[[2]],
                     using = c2)
  merge_columns_used(s1, s2)
}


#' @export
to_sql.relop_natural_join <- function (x,
                                       db,
                                       ...,
                                       limit = NULL,
                                       source_limit = NULL,
                                       indent_level = 0,
                                       tnum = mk_tmp_name_source('tsql'),
                                       append_cr = TRUE,
                                       using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::to_sql.relop_natural_join")
  dispatch_to_sql_method(
    method_name = "to_sql.relop_natural_join",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}


to_sql_relop_natural_join <- function(
  x,
  db,
  ...,
  limit = NULL,
  source_limit = NULL,
  indent_level = 0,
  tnum = mk_tmp_name_source('tsql'),
  append_cr = TRUE,
  using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::to_sql_relop_natural_join")
  using <- unique(c(calc_used_relop_natural_join(x,
                                                 using=using),
                    x$by))
  cs1 <- column_names(x$source[[1]])
  cs2 <- column_names(x$source[[2]])
  if(length(setdiff(using, c(cs1, cs2)))>0) {
    stop(paste("to_sql.relop_natural_join input table(s) missing columns ",
               paste(setdiff(using, c(cs1, cs2)), collapse = ", ")))
  }
  c1 <- intersect(using, cs1)
  subsqla_list <- to_sql(x$source[[1]],
                         db = db,
                         source_limit = source_limit,
                         indent_level = indent_level + 1,
                         tnum = tnum,
                         append_cr = FALSE,
                         using = c1)
  subsqla <- subsqla_list[[length(subsqla_list)]]
  c2 <- intersect(using, cs2)
  subsqlb_list <- to_sql(x$source[[2]],
                         db = db,
                         source_limit = source_limit,
                         indent_level = indent_level + 1,
                         tnum = tnum,
                         append_cr = FALSE,
                         using = c2)
  subsqlb <- subsqlb_list[[length(subsqlb_list)]]
  taba <- tnum()
  tabaq <- quote_identifier(db, taba)
  tabb <- tnum()
  tabbq <- quote_identifier(db, tabb)
  bexpr <- NULL
  aterms <- setdiff(c1, x$by)
  bterms <- setdiff(c2, x$by)
  overlap <- c(x$by, intersect(aterms, bterms))
  prefix <- paste(rep(' ', indent_level), collapse = '')
  osql <- vapply(overlap,
                 function(ci) {
                   ciq <- quote_identifier(db, ci)
                   paste0("COALESCE(",
                          tabaq, ".", ciq,
                          ", ",
                          tabbq, ".", ciq,
                          ") AS ", ciq)
                 }, character(1))
  asql <- vapply(setdiff(aterms, overlap),
                 function(ci) {
                   ciq <- quote_identifier(db, ci)
                   paste0(tabaq, ".", ciq,
                          " AS ", ciq)
                 }, character(1))
  bsql <- vapply(setdiff(bterms, overlap),
                 function(ci) {
                   ciq <- quote_identifier(db, ci)
                   paste0(tabbq, ".", ciq,
                          " AS ", ciq)
                 }, character(1))
  texpr <- paste(c(osql, asql, bsql), collapse = paste0(",\n ", prefix))
  q <- paste0(prefix, "SELECT\n",
              " ", prefix, texpr, "\n",
              prefix, "FROM (\n",
              subsqla, "\n",
              prefix, ") ",
              tabaq, "\n",
              prefix, x$jointype,
              " JOIN (\n",
              subsqlb, "\n",
              prefix, ") ",
              tabbq)
  if(length(x$by)>0) {
    bt <- vapply(x$by,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
    mt <- paste(paste(paste(tabaq, bt, sep='.'),
                      paste(tabbq, bt, sep='.'), sep = ' = '),
                collapse = ' AND ')
    q <- paste0(q, "\n",
                prefix, "ON\n",
                prefix, " ", mt)
  }
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsqla_list[-length(subsqla_list)],
    subsqlb_list[-length(subsqlb_list)],
    q)
}

