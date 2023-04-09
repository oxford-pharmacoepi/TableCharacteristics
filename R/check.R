#' @noRd
assertGroupVariable <- function(x, group, reference) {
  checkmate::assertCharacter(
    group,
    len = 1, any.missing = FALSE, null.ok = TRUE
  )
  if (!is.null(group)) {
    checkmate::assertTRUE(group %in% colnames(x))
    if (is.null(reference)) {
      return(FALSE)
    } else {
      groups <- x %>%
        dplyr::select(dplyr::all_of(group)) %>%
        dplyr::distinct() %>%
        dplyr::pull()
      checkmate::assertTRUE(length(reference) == 1)
      checkmate::assertTRUE(referenceGroup %in% groups)
      if (length(groups) > 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  } else {
    checkmate::assertNull(referenceGroup)
    return(FALSE)
  }
}

#' @noRd
computeVariables <- function(x,
                             numericVariables,
                             numericFormat,
                             dateVariables,
                             dateFormat,
                             categoricalVariables,
                             categoricalFormat,
                             binaryVariables,
                             binaryFormat,
                             otherVariables,
                             otherFormat) {
  checkCharacter(numericFormat, 1, TRUE)
  checkCharacter(dateFormat, 1, TRUE)
  checkCharacter(categoricalFormat, 1, TRUE)
  checkCharacter(binaryFormat, 1, TRUE)
  checkList(otherFormat, "character", 1, TRUE)
  checkmate::assertList(otherVariables, types = "character")
  checkmate::assertTRUE(length(otherVariables) == length(otherFormat))
  if (length(numericVariables) == 1 && is.na(numericVariables)) {
    numericVariables <- automaticVariables(x, "numeric", groupVariable)
  }
  if (length(dateVariables) == 1 && is.na(dateVariables)) {
    dateVariables <- automaticVariables(x, "date", groupVariable)
  }
  if (length(categoricalVariables) == 1 && is.na(categoricalVariables)) {
    categoricalVariables <- automaticVariables(x, "categorical", groupVariable)
  }
  if (length(binaryVariables) == 1 && is.na(binaryVariables)) {
    binaryVariables <- automaticVariables(x, "binary", groupVariable)
  }
  variable <- NULL
  ## numeric variables
  variable <- variable %>%
    dplyr::union_all(addFormats(
      x, numericVariables, numericFormat, "numericFormat", "numeric"
    ))
  ## date variables
  variable <- variable %>%
    dplyr::union_all( addFormats(
      x, dateVariables, dateFormat, "dateFormat", "date"
    ))
  ## categorical variables
  variable <- variable %>%
    dplyr::union_all(addFormats(
      x, categoricalVariables, categoricalFormat, "categoricalFormat",
      "categorical"
    ))
  ## binary variables
  variable <- variable %>%
    dplyr::union_all(addFormats(
      x, binaryVariables, binaryFormat, "binaryFormat", "binary"
    ))
  ## other variables
  for (k in seq_along(otherFormat)) {
    t <- variableTypes(x) %>%
      dplyr::filter(.data$variable %in% .env$otherVariables[[k]]) %>%
      dplyr::pull("variable_classification") %>%
      unique() %>%
      compatibleType()
    if (t == "incompatible") {
      cli::cli_abort(paste0(
        paste0(otherVariables[[k]], collapse = ", "), " are not compatible"
      ))
    } else {
      variable <- variable %>%
        dplyr::union_all(addFormats(
          x, otherVariables[[k]], otherFormat[k],
          paste0("otherFormat[", k, "]"), t
        ))
    }
  }
  return(variable)
}

#' @noRd
checkContains <- function(variables, x) {
  columns <- colnames(x)
  notContained <- variables[!(variables %in% x)]
  if (length(notContained) > 0) {
    errorMessage <- paste0(
      "Variables: ",
      paste0(notContained, collapse = ", "),
      " are not contained in the table"
    )
    cli::cli_abort(errorMessage)
  }
}

#' @noRd
checkVariablesCompatibility <- function(variables, x, name) {
  uniqueClassification <- variableTypes(x) %>%
    dplyr::pull("variable_classification") %>%
    unique()
  if (any(is.na(uniqueClassification))) {
    cli::cli_abort(glue::glue(
      "{name} contains variables that can not be classified. Use variableTypes()
      to see the different classification of your columns."
    ))
  }
  if (length(uniqueClassification) > 1 &&
      !all(uniqueClassification %in% c("numeric", "binary"))) {
    cli::cli_abort(
      "{name} contains variables that are not compatible. Use variableTypes() to
      see the different classification of your columns."
    )
  }
}

#' @noRd
checkCharacter <- function(x, len, nullOk) {
  if (isTRUE(nullOk) && is.null(x)) {
    return(NULL)
  }
  if (!is.character(x)) {
    cli::cli_abort(glue::glue("{deparse(substitute(x))} is not character."))
  }
  if (length(x) != len) {
    cli::cli_abort(glue::glue(
      "{deparse(substitute(x))} has length {length(x)} (expected = {len})"
    ))
  }
}

#' @noRd
checkList <- function(x, type, elementsLength, nullOk) {
  if (isTRUE(nullOk) && is.null(x)) {
    return(NULL)
  }
  if (!is.list(x)) {
    cli::cli_abort(glue::glue(
      "Expected {deparse(substitute(x))} to be a {type} list of length
      {elementsLength} elements"
    ))
  }
  expected <- lapply(x, function(x) {any(class(x) == type)}) %>%
    unlist() %>%
    any()
  if (!expected) {
    cli::cli_abort(glue::glue(
      "Expected {deparse(substitute(x))} to be a {type} list of length
      {elementsLength} elements"
    ))
  }
  expected <- lapply(x, function(x) {length(x) == elementsLength}) %>%
    unlist() %>%
    any()
  if (!expected) {
    cli::cli_abort(glue::glue(
      "Expected {deparse(substitute(x))} to be a {type} list of length
      {elementsLength} elements"
    ))
  }
}

#' @noRd
addFormats <- function(x, columns, formatColumns, type) {
  if (!is.null(columns)) {
    checkContains(columns, x)
    checkVariablesCompatibility(columns, x)
    formatColumns <- strsplit(formatColumns, "\n")[[1]]
    for (k in seq_along(formatColumns)) {
      functions <- assertFormat(formatColumns[k], type)
      if (length(functions) == 0) {
        cli::cli_abort(glue::glue(
          "No function detected in line {k} of
          {deparse(substitute(formatColumns))}: {formatColumns[k]}"
        ))
      } else {
        return(tidyr::expand_grid(
          variable = columns,
          fun = functions,
          variable_classification = type,
          format = formatColumns[k]
        ))
      }
    }
  } else {
    if (!is.null(formatColumns)) {
      cli::cli_abort(glue::glue(
        "Not NULL format ({deparse(substitute(formatColumns))}) for NULL list of
        variables ({deparse(substitute(columns))})"
      ))
    }
    return(NULL)
  }
}

#' @noRd
automaticVariables <- function(x, type, not) {
  variableTypes(x) %>%
    dplyr::filter(!(.data$variable %in% .env$not)) %>%
    dplyr::filter(.data$variable_classification == .env$type) %>%
    dplyr::pull("variable")
}

#' @noRd
checkGroupAndOrder <- function(x, variable, groupNames, order) {

}
