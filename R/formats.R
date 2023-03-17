#' Available functions and formats for numeric variables
#'
#' @return Tibble with the available numeric format keys
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
#' @export
#'
dateFormat <- function() {
  return(format("date"))
}

#' Available functions and formats for categorical variables
#'
#' @return Tibble with the available categorical format keys
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
#' @export
#'
binaryFormat <- function() {
  return(format("binary"))
}

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

