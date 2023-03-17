#' Available functions and formats for numeric variables
#'
#' @return Tibble with the available numeric format keys
#'
#' @example numericFormat()
#'
#' @export
#'
numericFormat <- function() {
  return(format("numeric"))
}

#' Available functions and formats for date variables
#'
#' @return Tibble with the available date format keys
#'
#' @example dateFormat()
#'
#' @export
#'
dateFormat <- function() {
  return(format("date"))
}

#' Available functions and formats for categorical variables
#'
#' @return Tibble with the available categorical format keys
#'
#' @example categoricalFormat()
#'
#' @export
#'
categoricalFormat <- function() {
  return(format("categorical"))
}

#' Available functions and formats for binary variables
#'
#' @return Tibble with the available binary format keys
#'
#' @example binaryFormat()
#'
#' @export
#'
binaryFormat <- function() {
  return(format("binary"))
}

#' Classify the variables between 5 types: "numeric", "categorical", "binary",
#' "date", or NA.
#'
#' @param x Tibble with different columns.
#'
#' @return Tibble with the variables classification
#'
#' @export
#'
variableTypes <- function(x) {
  checkmate::assertTibble(x)
  x <- dplyr::tibble(
    variable = colnames(x),
    variable_type = lapply(x, pillar::type_sum) %>% unlist()
  ) %>%
    dplyr::mutate(classification = assertClassification(.data$variable_type, .env$x))
  return(x)
}

#' @noRd
assertClassification <- function(x, tib) {
  lapply(x, function(x) {
    if (x == "lgl") {
      return("binary")
    } else if (x %in% c("chr", "fct", "ord")) {
        return("categorical")
    } else if (x %in% c("date", "dttm")) {
      return("date")
    } else if (x == "drtn") {
      return("numeric")
    } else if (x %in% c("int", "dbl", "int64")) {
      lab <- unique(tib[[x]])
      if (length(lab) <= 2 && all(lab %in% c(0, 1))) {
        return("binary")
      } else {
        return("numeric")
      }
    } else {
      return("NA")
    }
  }) %>%
    unlist()
}

#' @noRd
format <- function(x) {
  x <- formats %>%
    dplyr::filter(.data$type == .env$x) %>%
    dplyr::select(-"type")
  if (sum(is.na(x$info)) == nrow(x)) {
    x <- x %>% dplyr::select(-"info")
  }
  if (sum(is.na(x$are_NA_considered)) == nrow(x)) {
    x <- x %>% dplyr::select(-"are_NA_considered")
  }
  if (sum(is.na(x$warnings)) == nrow(x)) {
    x <- x %>% dplyr::select(-"warnings")
  }
  return(x)
}

#' @noRd
cleanTypes <- function(x, vt) {
  for (k in 1:nrow(vt)) {
    if (vt$classification[k] %in% c("binary", "numeric")) {
      x[[vt$variable[k]]] <- as.numeric(x[[vt$variable[k]]])
    } else if (vt$classification[k] == "categorical") {
      x[[vt$variable[k]]] <- as.character(x[[vt$variable[k]]])
    } else if (vt$classification[k] == "date") {
      x[[vt$variable[k]]] <- as.Date(x[[vt$variable[k]]])
    }
  }
  return(x)
}
