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
      x, numericVariables, numericFormat, "numeric"
    ))
  ## date variables
  variable <- variable %>%
    dplyr::union_all( addFormats(
      x, dateVariables, dateFormat, "date"
    ))
  ## categorical variables
  variable <- variable %>%
    dplyr::union_all(addFormats(
      x, categoricalVariables, categoricalFormat, "categorical"
    ))
  ## binary variables
  variable <- variable %>%
    dplyr::union_all(addFormats(
      x, binaryVariables, binaryFormat, "binary"
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
          x, otherVariables[[k]], otherFormat[k], t
        ))
    }
  }
  ## check some variables are present
  if (nrow(variable) == 0) {
    cli::cli_abort("Please select at least one variable to summarise")
  }
  return(variable)
}

#' @noRd
checkContains <- function(variables, x) {
  columns <- colnames(x)
  notContained <- variables[!(variables %in% columns)]
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
    dplyr::filter(.data$variable %in% .env$variables) %>%
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
  if (length(x) > 0) {
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
}

#' @noRd
addFormats <- function(x, columns, formatColumns, type) {
  if (!is.null(columns)) {
    checkContains(columns, x)
    checkVariablesCompatibility(columns, x, deparse(substitute(columns)))
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
          format = formatColumns[k],
          format_line = k
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
  if (!is.list(groupNames) && !is.null(groupNames)) {
    cli::cli_abort("Expected list or NULL in groupNames")
  }
  variable <- dplyr::select(variable, "variable") %>%
    dplyr::distinct()
  variable <- variable %>%
    dplyr::mutate(group_name = as.character(NA))
  if (!is.null(groupNames) && length(groupNames) > 0) {
    if (!all(lengths(groupNames) == 1)) {
      cli::cli_abort("All elements of groupName must be length 1")
    }
    namesGroups <- names(groupNames)
    if (is.null(namesGroups) || "" %in% namesGroups) {
      cli::cli_abort("groupNames should be a named list")
    }
    values <- unlist(groupNames)
    for (k in seq_along(namesGroups)) {
      variable <- variable %>%
        dplyr::mutate(group_name = dplyr::if_else(
          is.na(.data$group_name) & grepl(values[k], .data$variable),
          .env$namesGroups[k],
          .data$group_name
        ))
    }
  }
  if (is.null(order)) {
    order <- colnames(x)
    order <- order[order %in% variable$variable]
  }
  variable <- variable %>%
    dplyr::mutate(order_variable = dplyr::if_else(
      is.na(.data$group_name),
      .data$variable,
      .data$group_name
    )) %>%
    dplyr::left_join(
      dplyr::tibble(order_variable = order) %>%
        dplyr::mutate(order_id = dplyr::row_number()),
      by = "order_variable"
    ) %>%
    dplyr::mutate(order_id = dplyr::if_else(
      is.na(.data$order_id), Inf, .data$order_id
    ))
}

#' @noRd
checkPrintFormat <- function(bigMark, decimalMark, significativeDecimals) {
  if (!is.character(bigMark) && length(bigMark) != 1) {
    cli::cli_abort("bigMark should be a character of length 1")
  }
  if (!is.character(decimalMark) && length(decimalMark) != 1) {
    cli::cli_abort("decimalMark should be a character of length 1")
  }
  if (!is.numeric(significativeDecimals) &&
      length(significativeDecimals) != 1 &&
      significativeDecimals <= 0 &&
      significativeDecimals != round(significativeDecimals)) {
    cli::cli_abort("significativeDecimals must be an integer bigger than 0")
  }
}

#' @noRd
checkTable <- function(x) {
  if (!("data.frame" %in% class(x))) {
    cli::cli_abort("x is not a valid table")
  }
  x <- dplyr::as_tibble(x)
  if (nrow(x) <= 0 || ncol(x) <=0) {
    cli::cli_abort("x can not be empty")
  }
  return(x)
}
