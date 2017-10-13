#' Reshape data
#'
#' Reshape data from long to wide format
#'
#' @param dt long format data.table
#' @param var_row variable used as row in the final result
#' @param var_col variable used as column in the final result
#' @param var_val variable used as value to be computed in the final result
#'
#' @return wide format data.table
#'
#' @examples
#' require(data.table)
#'
#' lf <- data.table(Row = rep(c("a", "b"), each = 3), Col = rep(1:3, 2), Val = rep(1:2))
#' lf
#' wf <- MyDCast(lf, "Row", "Col", "Val")
#' wf
#'
#' @export
MyDCast <- function(dt, var_row, var_col, var_val) {
  # MyDCast <- function(dt, var_row, var_col, var_val, fn, ...) {
  # res <- data.table::dcast(dt,
  #                          formula = as.formula(paste(var_row, var_col,
  #                                                     sep = "~")),
  #                          value.var = var_val, fun.aggregate = fn, ...)
  # bug in passing fn function as argument
  # cf. https://github.com/Rdatatable/data.table/issues/1369
  # always sum up while aggregating
  res <- data.table::dcast(dt,
                           formula = as.formula(paste(var_row, var_col,
                                                      sep = "~")),
                           value.var = var_val, fun.aggregate = sum)
  old_names <- names(res)[2:ncol(res)]
  new_names <- paste0(var_val, "_", old_names)
  setnames(res, old_names, new_names)
  res
}

#' Reshape data
#'
#' Reshape data from long to wide format
#'
#' @param dt long format data.table
#' @param var_row variable used as row in the final result
#' @param var_col variable used as column in the final result
#' @param var_val variable used as value to be computed in the final result
#'
#' @return wide format data.table
#'
#' @examples
#' require(data.table)
#' lf <- data.table(Row = rep(c("a", "b"), each = 3), Col = rep(1:3, 2), Val = rep(1:2))
#' lf
#' wf <- MyDCast(lf, "Row", "Col", "Val")
#' wf
#'
#' @export
MyWeightedMean <- function(dt, var_row, var_col, var_val,
                           coeffs, method) {
  # MyWeightedMean <- function(dt, var_row, var_col, var_val,
  #                            fn, coeffs, method, ...) {
  # res <- MyDCast(dt, var_row, var_col, var_val, fn, ...)
  # bug in passing fn function as argument
  # cf. MyDCast
  res <- MyDCast(dt, var_row, var_col, var_val)
  
  years <- setdiff(colnames(res), var_row)
  years <- sapply(years, function(e) {
    splt <- strsplit(e, split = "_", fixed = TRUE)[[1]]
    splt <- splt[length(splt)]
  })
  
  res[is.na(res)] <- 0
  res <- res[coeffs, nomatch = 0]
  
  if (method == 1) {
    e_1 <- paste0("(", paste0(paste0(var_val, "_", years), " * ",
                              paste0("coeff_", years), collapse = " + "), ")")
  } else {
    e_1 <- paste0("(", paste0(paste0(var_val, "_", years),
                              collapse = " + "), ")")
  }
  e_2 <- paste0("(", paste0(paste0("coeff_", years),
                            collapse = " + "), ")")
  e <- paste0(paste0(var_val, "_moy := ifelse(", e_2, " == 0, 0, ", e_1,
                     " / ", e_2, ")"))
  res[, eval(parse(text = e))]
}
# TODO optimize this function
#' @export
MyQuantileGroupedData <- function(v, quant) {
  l <- floor(length(v) / quant)
  quantiles <- data.table(num = 1:quant,
                          inf = numeric(quant), sup = numeric(quant),
                          sum = numeric(quant), nb = numeric(quant))
  for (i in 1:quant) {
    quantiles[i, inf := v[(i-1) * l + 1]]
    if (i != quant) {
      sum_ <- sum(v[((i-1) * l + 1):(i * l)])
      nb_ <- length(v[((i-1) * l + 1):(i * l)])
      quantiles[i, sup := v[i * l]]
    } else {
      sum_ <- sum(v[((i-1) * l + 1):length(v)])
      nb_ <- length(v[((i-1) * l + 1):length(v)])
      quantiles[i, sup := v[length(v)]]
    }
    quantiles[i, sum := sum_]
    quantiles[i, nb := nb_]
    quantiles[i, moy := ifelse(nb == 0, 0, sum / nb)]
  }
  quantiles
}
#' @export
MyQuantileGroupedData_2 <- function(dt, quant) {
  l <- floor(nrow(dt) / quant)
  quantiles <- data.table(num = 1:quant,
                          inf = numeric(quant), sup = numeric(quant),
                          sum_a = numeric(quant), nb_a = numeric(quant),
                          sum_f = numeric(quant), nb_f = numeric(quant))
  for (i in 1:quant) {
    quantiles[i, inf := as.numeric(dt[(i-1) * l + 1, 1])]
    if (i != quant) {
      dt_tmp <- dt[((i-1) * l + 1):(i * l), ]
      sum_a_ <- dt_tmp[source == "accor", sum(ca_moy)]
      nb_a_ <- nrow(dt_tmp[source == "accor", ])
      sum_f_ <- dt_tmp[source == "frhi", sum(ca_moy)]
      nb_f_ <- nrow(dt_tmp[source == "frhi", ])
      quantiles[i, sup := as.numeric(dt[i * l, 1])]
    } else {
      dt_tmp <- dt[((i-1) * l + 1):nrow(dt), ]
      sum_a_ <- dt_tmp[source == "accor", sum(ca_moy)]
      nb_a_ <- nrow(dt_tmp[source == "accor", ])
      sum_f_ <- dt_tmp[source == "frhi", sum(ca_moy)]
      nb_f_ <- nrow(dt_tmp[source == "frhi", ])
      quantiles[i, sup := as.numeric(dt[nrow(dt), 1])]
    }
    quantiles[i, sum_a := sum_a_]
    quantiles[i, nb_a := nb_a_]
    quantiles[i, sum_f := sum_f_]
    quantiles[i, nb_f := nb_f_]
    quantiles[i, sum := sum_a_ + sum_f_]
    quantiles[i, nb := nb_a_ + nb_f_]
    quantiles[i, moy := ifelse(nb == 0, 0, sum / nb)]
  }
  quantiles
}
#' @export
MyVarNameXtract <- function(def, table) {
  names(def[[table]])
}
#' @export
MyDataTypeXtract <- function(def, table) {
  sapply(def[[table]], function(e) {e[["dtype"]]})
}
# The attribute "dmulti" and "dsep" indicate if the column contains a list of values
# and their separator respectively.
MyDataMultiplicityXtract <- function(def, table) {
  list(dmulti = sapply(def[[table]], function(e) {e[["dmulti"]]}),
       dsep = sapply(def[[table]], function(e) {e[["dsep"]]}))
}
#' Convert data type for data.table columns
#'
#' Convert data.table columns from \code{character} to \code{numeric} or \code{Date}
#'
#' @param def data definitions
#' @param table table name
#' @param node node name whose format is used to modify table variable
#' @param dec decimal separator, default to "."
#' @param verbose whether to print debug messages, default to FALSE
#'
#' @return nothing, function used for its side-effects
#'
#' @examples
#' require(data.table)
#'
#' @export
MyDataTypeConvert <- function(def, table, node = NULL, dec = ".", verbose = FALSE) {
  if (verbose) {
    print(Sys.time())
    print(paste0("Converting data types for table '", table, "'..."))
  }
  if (is.null(node)) {
    node = table
  }
  dtypes <- MyDataTypeXtract(def, node)
  dmulti_dsep <- MyDataMultiplicityXtract(def, node)
  dmulti = dmulti_dsep$dmulti
  dsep = dmulti_dsep$dsep
  invisible(
    lapply(names(dtypes), function(n) {
      
      # Since it is "dangerous" to cast non-character variables,
      # we leave them alone
      pre.if = paste0("if(class(", table, "$", n, ")==\"character\") {")
      post.if = "}"
      
      # For date and datetime, consider two cases: list and non-list columns
      # with corresponding datetime format
      if (tolower(dtypes[[n]]) == "date") {
        dformat <- def[[node]][[n]][["dformat"]]
        if (verbose) {
          print(paste0("...converting '", n, "' to type '", dtypes[[n]], "', format = ",
                       dformat))
        }
        if ((!is.null(dmulti[[n]])) && (dmulti[[n]] == "1")) {
          dsep <- def[[node]][[n]][["dsep"]]
          exp <- paste0(table, "$", n,
                        " <- sapply(as.list(strsplit(", table, "$", n, ", \"", dsep, "\")),",
                        "function(x){as.Date(x", ", format = \"", dformat, "\")", "})")
        } else {
          exp <- paste0(table, "$", n, " <- as.Date(", table, "$", n,
                        ", format = '", dformat, "')")
        }
        exp = paste0(pre.if, exp, post.if)
        eval.parent(parse(text = exp), n = 3)
        
      } else if (tolower(dtypes[[n]]) == "datetime") {
        dformat <- def[[node]][[n]][["dformat"]]
        if (verbose) {
          print(paste0("...converting '", n, "' to type '", dtypes[[n]], "', format = ",
                       dformat))
        }
        if ((!is.null(dmulti[[n]])) && (dmulti[[n]] == "1")) {
          dsep <- def[[node]][[n]][["dsep"]]
          exp <- paste0(table, "$", n,
                        " <- sapply(as.list(strsplit(", table, "$", n, ", \"", dsep, "\")),",
                        "function(x){as.POSIXct(x", ", format = \"", dformat, "\")", "})")
        } else {
          exp <- paste0(table, "$", n, " <- as.POSIXct(", table, "$", n,
                        ", format = '", dformat, "')")
        }
        exp = paste0(pre.if, exp, post.if)
        eval.parent(parse(text = exp), n = 3)
        
      } else if (tolower(dtypes[[n]]) == "numeric") {
        # For numeric type, no need to consider data format, only convert directly
        if (verbose) {
          print(paste0("...converting '", n, "' to type '", dtypes[[n]], "', decimal = ",
                       dec))
        }
        if (dec != ".") {
          exp <- paste0(table, "$", n, " <- gsub('", dec, "', '.', ",
                        table, "$", n, ", fixed = TRUE)")
          eval.parent(parse(text = exp), n = 3)
        }
        if ((!is.null(dmulti[[n]])) && (dmulti[[n]] == "1")) {
          dsep <- def[[node]][[n]][["dsep"]]
          exp <- paste0(table, "$", n,
                        " <- sapply(as.list(strsplit(", table, "$", n, ", \"", dsep, "\")),",
                        "function(x){as.numeric(x)})")
        } else {
          exp <- paste0(table, "$", n, " <- as.numeric(", table, "$", n, ")")
        }
        exp = paste0(pre.if, exp, post.if)
        eval.parent(parse(text = exp), n = 3)
        
      } else {
        # For other types (mainly characters), we consider only the list case
        if (verbose) {
          print(paste0("...converting '", n, "' to type '", dtypes[[n]]))
        }
        if ((!is.null(dmulti[[n]])) && (dmulti[[n]] == "1")) {
          dsep <- def[[node]][[n]][["dsep"]]
          exp <- paste0(table, "$", n, " <- as.list(strsplit(", table, "$", n, ", \"", dsep, "\"))")
          exp = paste0(pre.if, exp, post.if)
          eval.parent(parse(text = exp), n = 3)
        }
      }
    })
  )
}
#' Convert between date formats
#'
#' Convert data.table date columns between different formats as specified in dformat object
#'
#' @param def data definitions
#' @param table table name
#' @param dformat name of the date formats object
#' @param verbose whether to print debug messages, default to FALSE
#'
#' @return nothing, function used for its side-effects
#'
#' @examples
#' require(data.table)
#'
#' @export
# TODO variables to be defined in the vertone package
MyDateConvert <- function(def, table, dformat, verbose = FALSE) {
  if (verbose) {
    print(Sys.time())
    print(paste0("Converting date formats for table '", table, "'..."))
  }
  dtypes <- MyDataTypeXtract(def, table)
  invisible(
    lapply(names(dtypes), function(n) {
      if (dtypes[[n]] == "Date") {
        if (verbose) {
          print(paste0("...converting '", n, "'"))
        }
        exp <- paste0(table, "$", n, " <- stringr::str_replace_all(",
                      table, "$", n, ", unlist(", dformat, "))")
        eval.parent(parse(text = exp), n = 3)
      }
    })
  )
}
#' Convert between from 1-digit day & month format to 2-digit equivalents
#'
#' Convert data.table date columns from 1-digit to 2-digit day and month parts
#'
#' @param def data definitions
#' @param table table name
#' @param sep day-month-year separator
#' @param verbose whether to print debug messages, default to FALSE
#'
#' @return nothing, function used for its side-effects
#'
#' @examples
#' require(data.table)
#'
#' @export
MyMonthDayConvert <- function(def, table, sep, verbose = FALSE) {
  if (verbose) {
    print(Sys.time)
    print(paste0("Converting day & month formats for table '", table, "'..."))
  }
  dtypes <- MyDataTypeXtract(def, table)
  invisible(
    lapply(names(dtypes), function(n) {
      if (dtypes[[n]] == "Date") {
        if (verbose) {
          print(paste0("...converting '", n, "'"))
        }
        exp <- paste0(table, "$", n, " <- stringr::str_replace(",
                      table, "$", n, ", '(^[:digit:]{1}", sep, ")', '0\\\\1')")
        eval.parent(parse(text = exp), n = 3)
        exp <- paste0(table, "$", n, " <- stringr::str_replace(",
                      table, "$", n, ", '", sep, "([:digit:]{1})", sep,
                      "', '", sep, "0\\\\1", sep, "')")
        eval.parent(parse(text = exp), n = 3)
      }
    })
  )
}
#' Get a list of months and years between two times
#' The first time must be before the second time
#'
#' @param month1 month of the first time
#' @param year1 year of the first time
#' @param month2 month of the second time
#' @param year2 year of the second time
#'
#' @return a list of months and years (in character)
#'
#' @export
GetMonthsYearsBetween <- function(month1, year1, month2, year2) {
  if ((year1 > year2) | ((year1 == year2) & (month1 >= month2))) {
    return (list())
  }
  if (year1==year2) {
    months <- c(month1:month2)
    years <- rep(year1, month2-month1+1)
  } else {
    months <- c(month1:12, rep(1:12, times=max(0, year2-year1-1)), 1:month2)
    years <- rep(year1, 12-month1+1)
    if (year1+1 <= year2-1) {
      for (year in (year1+1):(year2-1))
        years <- c(years, rep(year, 12))
    }
    years <- c(years, rep(year2, month2))
  }
  return (list(as.character(months),
               as.character(years)))
}
#' Return a cartesian product of two tables
#'
#' @param X the first table
#' @param Y the second table
#'
#' @return a cartesian table of X and Y
#'
#' @export
CartesianProduct <- function(X, Y) {
  k <- NULL
  X <- X[, c(k = 1, .SD)]
  setkey(X, k)
  Y <- Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian = TRUE][, `:=`(k, NULL)]
}
#' Return a Date variable that represents the first or last day
#' of n months after that. The input is current month, current year
#' and the number of added months
#'
#' @param month the current month
#' @param year the current year
#' @param num.added.months the number of added months
#' @param return.first.date if TRUE, return the first date. Otherwise return the last date
#'
#' @return a Date variable num.added.months months after that
#'
#' @export
AddMonthsToMonthYear <- function(month, year, num.added.months,
                                 return.first.date = TRUE) {
  num.added.year <- num.added.months %/% 12
  year <- year + num.added.year
  num.added.months <- num.added.months %% 12
  if (month + num.added.months > 12) {
    year <- year + 1
  }
  month <- ((month + num.added.months) %% 12)
  if (month == 0) month <- 12
  returned.date <- (as.Date(paste("01", as.character(month), as.character(year),
                                  sep = "-"),
                            format = "%d-%m-%Y"))
  if (!return.first.date) {
    returned.date <- (as.Date(paste(as.character(days_in_month(returned.date)),
                                    as.character(month),
                                    as.character(year),
                                    sep = "-"),
                              format = "%d-%m-%Y"))
  }
  return (returned.date)
}
#' Return a Date variable that represents the first or last day
#' of n months after that. The input is Date variable
#' and the number of added months
#'
#' @param date the current date
#' @param num.added.months the number of added months
#' @param return.first.date if TRUE, return the first date. Otherwise return the last date
#'
#' @return a Date variable num.added.months months after that
#'
#' @export
AddMonthsToDate <- function(date, num.added.months,
                            return.first.date = TRUE) {
  return (AddMonthsToMonthYear(month(date),
                               year(date),
                               num.added.months,
                               return.first.date))
}
