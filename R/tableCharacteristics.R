#' Function to compute a nice table one that summarises the characteristics of
#' by groups. Groups can be compared between them adding absolute standardized
#' mean differences statistics.
#'
#' @param x Table that contains the characteristics to be summarised.
#' @param groupVariable Variable of x that contains the grouping. If it is NULL
#' No groupping is used.
#' @param referenceGroup Group of reference to compute the smd. If it is NULL no
#' reference group is picked.
#' @param numericVariables Variables included in the numeric group. If NA they
#' are detected automatically see variableTypes(). If NULL this group is not
#' applied.
#' @param numericFormat Format for the numericVariables. See numericFormats()
#' for the available options. More information in xxx vignette.
#' @param dateVariables Variables included in the date group. If NA they are
#' detected automatically see variableTypes(). If NULL this group is not used.
#' @param dateFormat Format for the numericVariables. See dateFormats()
#' for the available options. More information in xxx vignette.
#' @param categoricalVariables Variables included in the categorical group. If
#' NA they are detected automatically see variableTypes(). If NULL this group is
#' not used.
#' @param categoricalFormat Format for the categoricalVariables. See
#' categoricalFormats() for the available options. More information in xxx
#' vignette.
#' @param binaryVariables Variables included in the binary group. If NA they are
#' detected automatically see variableTypes(). If NULL this group is not used.
#' @param binaryFormat Format for the binaryVariables. See binaryFormats() for
#' the available options. More information in xxx vignette.
#' @param otherVariables List of variables included in the different groups. All
#' variables should share same type.
#' @param otherFormat List of format for each element in otherVariables. See the
#' different avaialble formats in: numericFormats(), dateFormats(),
#' categoricalFormats() and binaryFormats().
#' @param groupNames List that contain the name and start pattern for the
#' grouped variables.
#' @param order Order of the variables. If NULL variables appear as the column
#' order. If NA they are sorted alphabetically.
#' @param bigMark Thousands separator.
#' @param decimalMark Decimal separator.
#' @param significativeDecimals Number of significant decimals for numeric (non
#' integer data)
#'
#' @return Tibble with the characteristics of the different groups
#'
#' @export
#'
#' @examples
#'

tableCharacteristics <- function (
    x,
    groupVariable = NULL,
    referenceGroup = NULL,
    numericVariables = NA,
    numericFormat = "median [q25 - q75]",
    dateVariables = NA,
    dateFormat = "median [min - max]",
    categoricalVariables = NA,
    categoricalFormat = "count (%)",
    binaryVariables = NA,
    binaryFormat = "count (%)",
    otherVariables = list(),
    otherFormat = list(),
    groupNames = list(),
    order = NULL,
    bigMark = ",",
    decimalMark = ".",
    significativeDecimals = 2
) {
  # initial checks ----
  ## check x is tibble
  checkmate::assertTibble(x, min.rows = 1, min.cols = 1)
  ## assert that groupVariable is character
  checkmate::assertCharacter(
    groupVariable, len = 1, any.missing = FALSE, null.ok = TRUE
  )
  ## if groupVariable exist check that is a column of x and referenceGroup is
  ## present
  if (!is.null(groupVariable)) {
    checkmate::assertTRUE(groupVariable %in% colnames(x))
    checkmate::assertTRUE(length(referenceGroup) == 1)
    groups <- x %>%
      dplyr::select(dplyr::all_of(groupVariable)) %>%
      dplyr::distinct() %>%
      dplyr::pull()
    checkmate::assertTRUE(referenceGroup %in% groups)
  } else {
    checkmate::assertNull(referenceGroup)
  }
  ## detect all variables type
  variableType <- variableTypes(x)
  ## start a tibble with all the variables and functions that should be applied
  variables <- dplyr::tibble(
    variable = character(),
    variable_classification = character(),
    format = character(),
    fun = character()
  )
  ## check other variables
  checkmate::assertList(otherVariables, types = "character")
  checkmate::assertList(otherFormat, types = "character")
  checkmate::assertTRUE(length(otherVariables) == length(otherFormat))
  for (k in seq_along(otherFormat)) {
    checkmate::assertTRUE(all(otherVariables[[k]] %in% colnames(x)))
    t <- compatibleType(
      variableType %>%
        dplyr::filter(.data$variable %in% .env$otherVariables[[k]]) %>%
        dplyr::pull("variable_classification") %>%
        unique()
    )
    if (t == "incompatible") {
      stop(paste0(
        paste0(otherVariables[[k]], collapse = ", "), "are not compatible"
      ))
    }
    functions <- assertFormat(otherFormat[[k]], t)
    if (length(functions) == 0) {
      stop(paste0("No function detected in otherFormat[[", k, "]]"))
    } else {
      variables <- variables %>%
        dplyr::union_all(tidyr::expand_grid(
          variable = otherVariables[[k]],
          variable_classification = t,
          format = otherFormat[[k]],
          fun = functions
        ))
    }
  }
  ## if variables groups are NA detect automatically
  if (is.na(numericVariables)) {
    numericVariables <- variableType %>%
      dplyr::filter(!(.data$variable %in% variables$variable)) %>%
      dplyr::filter(.data$classification == "numeric") %>%
      dplyr::pull("variable")
  }
  if (is.na(dateVariables)) {
    dateVariables <- variableType %>%
      dplyr::filter(!(.data$variable %in% variables$variable)) %>%
      dplyr::filter(.data$classification == "date") %>%
      dplyr::pull("variable")
  }
  if (is.na(categoricalVariables)) {
    categoricalVariables <- variableType %>%
      dplyr::filter(!(.data$variable %in% variables$variable)) %>%
      dplyr::filter(.data$classification == "categorical") %>%
      dplyr::pull("variable")
  }
  if (is.na(binaryVariables)) {
    binaryVariables <- variableType %>%
      dplyr::filter(!(.data$variable %in% variables$variable)) %>%
      dplyr::filter(.data$classification == "binary") %>%
      dplyr::pull("variable")
  }
  ## numeric variables
  if (!is.null(numericVariables)) {
    checkmate::assertTRUE(all(numericVariables %in% colnames(x)))
    checkmate::assertTRUE(all(
      variableType %>%
        dplyr::filter(.data$variable %in% .env$numericVariables) %>%
        dplyr::pull("variable_classification") %in%
        c("numeric", "binary")
    ))
    functions <- assertFormat(numericFormat, "numeric")
    if (length(functions) == 0) {
      stop("No function detected in numericFormat")
    } else {
      variables <- variables %>%
        dplyr::union_all(tidyr::expand_grid(
          variable = numericVariables,
          fun = functions,
          variable_classification = "numeric",
          format = numericFormat
        ))
    }
  } else {
    checkmate::assertNull(numericFormat)
  }
  ## date variables
  if (!is.null(dateVariables)) {
    checkmate::assertTRUE(all(dateVariables %in% colnames(x)))
    checkmate::assertTRUE(all(
      variableType %>%
        dplyr::filter(.data$variable %in% .env$dateVariables) %>%
        dplyr::pull("variable_classification") == "date"
    ))
    functions <- assertFormat(dateFormat, "date")
    if (length(functions) == 0) {
      stop("No function detected in dateFormat")
    } else {
      variables <- variables %>%
        dplyr::union_all(tidyr::expand_grid(
          variable = dateVariables,
          fun = functions,
          variable_classification = "date",
          format = dateFormat
        ))
    }
  } else {
    checkmate::assertNull(dateFormat)
  }
  ## categorical variables
  if (!is.null(categoricalVariables)) {
    checkmate::assertTRUE(all(categoricalVariables %in% colnames(x)))
    checkmate::assertTRUE(all(
      variableType %>%
        dplyr::filter(.data$variable %in% .env$categoricalVariables) %>%
        dplyr::pull("variable_classification") == "categorical"
    ))
    functions <- assertFormat(categoricalFormat, "categorical")
    if (length(functions) == 0) {
      stop("No function detected in categoricalFormat")
    } else {
      variables <- variables %>%
        dplyr::union_all(tidyr::expand_grid(
          variable = categoricalVariables,
          fun = functions,
          variable_classification = "categorical",
          format = categoricalFormat
        ))
    }
  } else {
    checkmate::assertNull(categoricalFormat)
  }
  ## binary variables
  if (!is.null(binaryVariables)) {
    checkmate::assertTRUE(all(binaryVariables %in% colnames(x)))
    checkmate::assertTRUE(all(
      variableType %>%
        dplyr::filter(.data$variable %in% .env$binaryVariables) %>%
        dplyr::pull("variable_classification") == "binary"
    ))
    functions <- assertFormat(binaryFormat, "binary")
    if (length(functions) == 0) {
      stop("No function detected in binaryFormat")
    } else {
      variables <- variables %>%
        dplyr::union_all(tidyr::expand_grid(
          variable = binaryVariables,
          fun = functions,
          variable_classification = "binary",
          format = binaryFormat
        ))
    }
  } else {
    checkmate::assertNull(binaryFormat)
  }
  ## check group names
  checkmate::assertList(
    groupNames, types = "character",any.missing = FALSE, unique = TRUE
  )
  if (length(groupNames) > 0) {
    checkmate::assertCharacter(
      names(groupNames), any.missing = FALSE, min.chars = 1
    )
    for (k in seq_along(groupNames)) {
      checkmate::assertCharacter(groupNames[[k]], len = 1, any.missing = FALSE)
      inGroup <- variables$variable[grepl(groupNames[[k]], variables$variable)]
      if (length(inGroup) == 0) {
        warning(paste0("No variable found for group ", names(groupNames)[k]))
        group <- dplyr::tibble(variable = character(), group = character())
      } else {
        group <- dplyr::tibble(
          variable = inGroup,
          group = names(groupNames)[k]
        )
      }
      if (k == 1) {
        groups <- group
      } else {
        groups <- groups %>%
          dplyr::union_all(group)
      }
    }
    variables <- variables %>%
      dplyr::left_join(groups, by = "variable")
  } else {
    variables <- variables %>%
      dplyr::mutate(group = as.character(NA))
  }
  checkmate::assertCharacter(
    order, any.missing = FALSE, null.ok = TRUE, unique = TRUE
  )
  variables <- variables %>%
    dplyr::mutate(order = dplyr::if_else(
      is.na(.data$group), .data$variable, .data$group
    ))
  if (!is.null(order)) {
    if (is.na(order)) {
      order <- sort(unique(variables$order))
    } else {
      checkmate::assertTRUE(all(variables$order %in% order))
      checkmate::assertTRUE(all(order %in% variables$order))
    }
  } else {
    order <-dplyr::tibble(
      variable = colnames(x)
    ) %>%
      dplyr::inner_join(variables, by = "variable", multiple = "all") %>%
      dplyr::select("order") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }
  order <- dplyr::tibble(order = order) %>%
    dplyr::mutate(id = dplyr::row_number())
  checkmate::assertTRUE(nrow(variables) > 0)

  x <- cleanTypes(x, variables)

  result <- summaryValues(
    x,
    variables %>%
      dplyr::select("variable", "variable_classification", "fun") %>%
      dplyr::distinct(),
    groupVariable
  )

}

# variable function
variableTibble <- function(
    x,
    variablesOrder,
    variables,
    formats
) {
  order <- dplyr::tibble(variable = order, order = dplyr::row_number())
  variable_settings <- NULL
  for (k in 1:length(variables)) {
    variable_type <- variableType(x, variables[[k]])
    variable_tibble_k <- dplyr::tibble(
      variable = variables[[k]],
      variable_type = variable_type
    )
    variable_settings <- variable_settings %>%
      dplyr::union_all(
        variable_tibble_k %>%
          dplyr::left_join(
            dplyr::tibble(variable_type = variable_type) %>%
              dplyr::mutate(
                smd_type = dplyr::if_else(
                  .data$variable_type %in% c("numeric", "date"),
                  "numeric",
                  "categorical"
                ),
                format = strsplit(format[[k]], "\n")[[1]],
                variable_group = k
              ) %>%
              dplyr::mutate(
                functions = getFunctions(.data$format, .data$variable_type),
                format = subFormat(.data$format, .data$variable_type),
                number_lines = length(.data$format),
                line_number = dplyr::row_number()
              ),
            by = "variable_type"
          )
      )
  }
  variable_settings <- variable_settings %>%
    dplyr::mutate(
      smd_type = dplyr::if_else(.data$number_lines > 1, NA, .data$smd_type)
    ) %>%
    dplyr::union_all(
      variable_settings %>%
        dplyr::filter(.data$number_lines > 1 & .data$line_number == 1) %>%
        mutate(
          line_number = 0,
          functions = NA,
          format = NA,
        )
    ) %>%
    dplyr::inner_join(variable_settings, by = "variable") %>%
    dplyr::arrange(.data$order, .data$line_number) %>%
    dplyr::mutate(
      variable_new_name = paste0("var", .data$order),
      column_name = paste0("column", .data$order),
      row_name = dplyr::if_else(
        .data$number_lines == 1,
        paste0(.data$variable, " ", .data$format),
        dplyr::if_else(.data$line_number == 0, .data$variable, .data$format)
      ),
      content = dplyr::if_else(
        is.na(.data$format),
        NA,
        gsub("#VAR#", .data$variable_new_name, .data$format)
      )
    )

  #variable variable_type functions row_name variable_id column_name value smd
  #age "age mean (sd)" var1 column_1 'paste0(.data$var1_mean, " (", .data$var1_sd, ")")' TRUE

}

#' @noRd
summaryValues <- function(x, variables, group) {
  if (!is.null(group)) {
    x <- x %>% dplyr::group_by(dplyr::all_of(group))
  }
  result <- dplyr::tibble(
    variable = character(),
    variable_classification = character(),
    fun = character(),
    value = character(),
    groupping = character()
  )
  # numeric variables
  variablesNumeric <- variables %>%
    dplyr::filter(.data$variable_classification == "numeric")
  if (nrow(variablesNumeric) > 0) {
    functions <- variablesNumeric %>%
      dplyr::pull("fun") %>%
      unique()
    for (k in seq_along(functions)) {
      variablesFunction <- variablesNumeric %>%
        dplyr::filter(.data$fun == .env$functions[k]) %>%
        dplyr::pull("variable")
      result.k <- x %>%
        dplyr::summarise(dplyr::across(
          .cols = dplyr::all_of(.env$variablesFunction),
          .fns = getFunctions(functions[k]),
          .names = "{.col}"
        ))
      if (is.null(group)) {
        result.k <- dplyr::mutate(result.k, groupping = NA)
      } else {
        result.k <- dplyr::rename("groupping" = dplyr::all_of(group))
      }
      result.k <- result.k %>%
        dplyr::mutate(dplyr::across(
          !"groupping",
          ~ base::format(
            .x,
            big.mark = bigMark,
            decimal.mark = decimalMark,
            nsmall = ifelse(.x %% 1 == 0, 0, significativeDecimals)
          )
        )) %>%
        tidyr::pivot_longer(!"groupping", names_to = "variable") %>%
        dplyr::mutate(
          fun = .env$functions[k], variable_classification = "numeric"
        )
      result <- dplyr::union_all(result, result.k)
    }
  }
  # date variables
  variablesDate <- variables %>%
    dplyr::filter(.data$variable_classification == "date")
  if (nrow(variablesDate) > 0) {
    functions <- variablesDate %>%
      dplyr::pull("fun") %>%
      unique()
    for (k in seq_along(functions)) {
      variablesFunction <- variablesDate %>%
        dplyr::filter(.data$fun == .env$functions[k]) %>%
        dplyr::pull("variable")
      result.k <- x %>%
        dplyr::summarise(dplyr::across(
          .cols = dplyr::all_of(.env$variablesFunction),
          .fns = getFunctions(functions[k]),
          .names = "{.col}"
        ))
      if (is.null(group)) {
        result.k <- dplyr::mutate(result.k, groupping = NA)
      } else {
        result.k <- dplyr::rename("groupping" = dplyr::all_of(group))
      }
      if (dateFromats()$result[dateFormats()$format_key == functions[k]] == "date") {
        result.k <- result.k %>%
          dplyr::mutate(dplyr::across(
            !"groupping",
            ~ as.character(as.Date(round(.x), origin = "1970-01-01"))
          ))
      } else {
        result.k <- result.k %>%
          dplyr::mutate(dplyr::across(
            !"groupping",
            ~ base::format(
              .x,
              big.mark = bigMark,
              decimal.mark = decimalMark,
              nsmall = ifelse(.x %% 1 == 0, 0, significativeDecimals)
            )
          ))
      }
      result <- dplyr::union_all(
        result,
        result.k %>%
          tidyr::pivot_longer(!"groupping", names_to = "variable") %>%
          dplyr::mutate(
            fun = .env$functions[k], variable_classification = "date"
          )
      )
    }
  }
  # binary variables
  variablesBinary <- variables %>%
    dplyr::filter(.data$variable_classification == "binary")
  if (nrow(variablesBinary) > 0) {
    variablesFunction <- variablesBinary %>%
      dplyr::pull("variable") %>%
      unique()
    result.k <- x %>%
      dplyr::summarise(dplyr::across(
        .cols = dplyr::all_of(.env$variablesFunction),
        .fns = list("sum" = function(x) {sum(x)}),
        .names = "{.col}"
      ))
    if (is.null(group)) {
      result.k <- dplyr::mutate(result.k, groupping = NA)
    } else {
      result.k <- dplyr::rename("groupping" = dplyr::all_of(group))
    }
    if (dateFromats()$result[dateFormats()$format_key == functions[k]] == "date") {
      result.k <- result.k %>%
        dplyr::mutate(dplyr::across(
          !"groupping",
          ~ as.character(as.Date(round(.x), origin = "1970-01-01"))
        ))
    } else {
      result.k <- result.k %>%
        dplyr::mutate(dplyr::across(
          !"groupping",
          ~ base::format(
            .x,
            big.mark = bigMark,
            decimal.mark = decimalMark,
            nsmall = ifelse(.x %% 1 == 0, 0, significativeDecimals)
          )
        ))
    }
    result <- dplyr::union_all(
      result,
      result.k %>%
        tidyr::pivot_longer(!"groupping", names_to = "variable") %>%
        dplyr::mutate(
          fun = .env$functions[k], variable_classification = "date"
        )
    )
  }
}
}

#' @noRd
getFunctions <- function(f) {
  estimates_func <- list(
    "min" = function(x) {
      base::min(x, na.rm = TRUE)
    },
    "max" = function(x) {
      base::max(x, na.rm = TRUE)
    },
    "mean" = function(x) {
      base::mean(x, na.rm = TRUE)
    },
    "median" = function(x) {
      stats::median(x, na.rm = TRUE)
    },
    "sum" = function(x) {
      base::sum(x, na.rm = TRUE)
    },
    "iqr" = function(x) {
      stats::IQR(x, na.rm = TRUE)
    },
    "range" = function(x) {
      base::diff(base::range(x, na.rm = TRUE))
    },
    "sd" = function(x) {
      stats::sd(x, na.rm = TRUE)
    },
    "q01" = function(x) {
      stats::quantile(x, 0.01, na.rm = TRUE)
    },
    "q02" = function(x) {
      stats::quantile(x, 0.02, na.rm = TRUE)
    },
    "q03" = function(x) {
      stats::quantile(x, 0.03, na.rm = TRUE)
    },
    "q04" = function(x) {
      stats::quantile(x, 0.04, na.rm = TRUE)
    },
    "q05" = function(x) {
      stats::quantile(x, 0.05, na.rm = TRUE)
    },
    "q06" = function(x) {
      stats::quantile(x, 0.06, na.rm = TRUE)
    },
    "q07" = function(x) {
      stats::quantile(x, 0.07, na.rm = TRUE)
    },
    "q08" = function(x) {
      stats::quantile(x, 0.08, na.rm = TRUE)
    },
    "q09" = function(x) {
      stats::quantile(x, 0.09, na.rm = TRUE)
    },
    "q10" = function(x) {
      stats::quantile(x, 0.1, na.rm = TRUE)
    },
    "q11" = function(x) {
      stats::quantile(x, 0.11, na.rm = TRUE)
    },
    "q12" = function(x) {
      stats::quantile(x, 0.12, na.rm = TRUE)
    },
    "q13" = function(x) {
      stats::quantile(x, 0.13, na.rm = TRUE)
    },
    "q14" = function(x) {
      stats::quantile(x, 0.14, na.rm = TRUE)
    },
    "q15" = function(x) {
      stats::quantile(x, 0.15, na.rm = TRUE)
    },
    "q16" = function(x) {
      stats::quantile(x, 0.16, na.rm = TRUE)
    },
    "q17" = function(x) {
      stats::quantile(x, 0.17, na.rm = TRUE)
    },
    "q18" = function(x) {
      stats::quantile(x, 0.18, na.rm = TRUE)
    },
    "q19" = function(x) {
      stats::quantile(x, 0.19, na.rm = TRUE)
    },
    "q20" = function(x) {
      stats::quantile(x, 0.2, na.rm = TRUE)
    },
    "q21" = function(x) {
      stats::quantile(x, 0.21, na.rm = TRUE)
    },
    "q22" = function(x) {
      stats::quantile(x, 0.22, na.rm = TRUE)
    },
    "q23" = function(x) {
      stats::quantile(x, 0.23, na.rm = TRUE)
    },
    "q24" = function(x) {
      stats::quantile(x, 0.24, na.rm = TRUE)
    },
    "q25" = function(x) {
      stats::quantile(x, 0.25, na.rm = TRUE)
    },
    "q26" = function(x) {
      stats::quantile(x, 0.26, na.rm = TRUE)
    },
    "q27" = function(x) {
      stats::quantile(x, 0.27, na.rm = TRUE)
    },
    "q28" = function(x) {
      stats::quantile(x, 0.28, na.rm = TRUE)
    },
    "q29" = function(x) {
      stats::quantile(x, 0.29, na.rm = TRUE)
    },
    "q30" = function(x) {
      stats::quantile(x, 0.3, na.rm = TRUE)
    },
    "q31" = function(x) {
      stats::quantile(x, 0.31, na.rm = TRUE)
    },
    "q32" = function(x) {
      stats::quantile(x, 0.32, na.rm = TRUE)
    },
    "q33" = function(x) {
      stats::quantile(x, 0.33, na.rm = TRUE)
    },
    "q34" = function(x) {
      stats::quantile(x, 0.34, na.rm = TRUE)
    },
    "q35" = function(x) {
      stats::quantile(x, 0.35, na.rm = TRUE)
    },
    "q36" = function(x) {
      stats::quantile(x, 0.36, na.rm = TRUE)
    },
    "q37" = function(x) {
      stats::quantile(x, 0.37, na.rm = TRUE)
    },
    "q38" = function(x) {
      stats::quantile(x, 0.38, na.rm = TRUE)
    },
    "q39" = function(x) {
      stats::quantile(x, 0.39, na.rm = TRUE)
    },
    "q40" = function(x) {
      stats::quantile(x, 0.4, na.rm = TRUE)
    },
    "q41" = function(x) {
      stats::quantile(x, 0.41, na.rm = TRUE)
    },
    "q42" = function(x) {
      stats::quantile(x, 0.42, na.rm = TRUE)
    },
    "q43" = function(x) {
      stats::quantile(x, 0.43, na.rm = TRUE)
    },
    "q44" = function(x) {
      stats::quantile(x, 0.44, na.rm = TRUE)
    },
    "q45" = function(x) {
      stats::quantile(x, 0.45, na.rm = TRUE)
    },
    "q46" = function(x) {
      stats::quantile(x, 0.46, na.rm = TRUE)
    },
    "q47" = function(x) {
      stats::quantile(x, 0.47, na.rm = TRUE)
    },
    "q48" = function(x) {
      stats::quantile(x, 0.48, na.rm = TRUE)
    },
    "q49" = function(x) {
      stats::quantile(x, 0.49, na.rm = TRUE)
    },
    "q51" = function(x) {
      stats::quantile(x, 0.51, na.rm = TRUE)
    },
    "q52" = function(x) {
      stats::quantile(x, 0.52, na.rm = TRUE)
    },
    "q53" = function(x) {
      stats::quantile(x, 0.53, na.rm = TRUE)
    },
    "q54" = function(x) {
      stats::quantile(x, 0.54, na.rm = TRUE)
    },
    "q55" = function(x) {
      stats::quantile(x, 0.55, na.rm = TRUE)
    },
    "q56" = function(x) {
      stats::quantile(x, 0.56, na.rm = TRUE)
    },
    "q57" = function(x) {
      stats::quantile(x, 0.57, na.rm = TRUE)
    },
    "q58" = function(x) {
      stats::quantile(x, 0.58, na.rm = TRUE)
    },
    "q59" = function(x) {
      stats::quantile(x, 0.59, na.rm = TRUE)
    },
    "q60" = function(x) {
      stats::quantile(x, 0.6, na.rm = TRUE)
    },
    "q61" = function(x) {
      stats::quantile(x, 0.61, na.rm = TRUE)
    },
    "q62" = function(x) {
      stats::quantile(x, 0.62, na.rm = TRUE)
    },
    "q63" = function(x) {
      stats::quantile(x, 0.63, na.rm = TRUE)
    },
    "q64" = function(x) {
      stats::quantile(x, 0.64, na.rm = TRUE)
    },
    "q65" = function(x) {
      stats::quantile(x, 0.65, na.rm = TRUE)
    },
    "q66" = function(x) {
      stats::quantile(x, 0.66, na.rm = TRUE)
    },
    "q67" = function(x) {
      stats::quantile(x, 0.67, na.rm = TRUE)
    },
    "q68" = function(x) {
      stats::quantile(x, 0.68, na.rm = TRUE)
    },
    "q69" = function(x) {
      stats::quantile(x, 0.69, na.rm = TRUE)
    },
    "q70" = function(x) {
      stats::quantile(x, 0.7, na.rm = TRUE)
    },
    "q71" = function(x) {
      stats::quantile(x, 0.71, na.rm = TRUE)
    },
    "q72" = function(x) {
      stats::quantile(x, 0.72, na.rm = TRUE)
    },
    "q73" = function(x) {
      stats::quantile(x, 0.73, na.rm = TRUE)
    },
    "q74" = function(x) {
      stats::quantile(x, 0.74, na.rm = TRUE)
    },
    "q75" = function(x) {
      stats::quantile(x, 0.75, na.rm = TRUE)
    },
    "q76" = function(x) {
      stats::quantile(x, 0.76, na.rm = TRUE)
    },
    "q77" = function(x) {
      stats::quantile(x, 0.77, na.rm = TRUE)
    },
    "q78" = function(x) {
      stats::quantile(x, 0.78, na.rm = TRUE)
    },
    "q79" = function(x) {
      stats::quantile(x, 0.79, na.rm = TRUE)
    },
    "q80" = function(x) {
      stats::quantile(x, 0.8, na.rm = TRUE)
    },
    "q81" = function(x) {
      stats::quantile(x, 0.81, na.rm = TRUE)
    },
    "q82" = function(x) {
      stats::quantile(x, 0.82, na.rm = TRUE)
    },
    "q83" = function(x) {
      stats::quantile(x, 0.83, na.rm = TRUE)
    },
    "q84" = function(x) {
      stats::quantile(x, 0.84, na.rm = TRUE)
    },
    "q85" = function(x) {
      stats::quantile(x, 0.85, na.rm = TRUE)
    },
    "q86" = function(x) {
      stats::quantile(x, 0.86, na.rm = TRUE)
    },
    "q87" = function(x) {
      stats::quantile(x, 0.87, na.rm = TRUE)
    },
    "q88" = function(x) {
      stats::quantile(x, 0.88, na.rm = TRUE)
    },
    "q89" = function(x) {
      stats::quantile(x, 0.89, na.rm = TRUE)
    },
    "q90" = function(x) {
      stats::quantile(x, 0.9, na.rm = TRUE)
    },
    "q91" = function(x) {
      stats::quantile(x, 0.91, na.rm = TRUE)
    },
    "q92" = function(x) {
      stats::quantile(x, 0.92, na.rm = TRUE)
    },
    "q93" = function(x) {
      stats::quantile(x, 0.93, na.rm = TRUE)
    },
    "q94" = function(x) {
      stats::quantile(x, 0.94, na.rm = TRUE)
    },
    "q95" = function(x) {
      stats::quantile(x, 0.95, na.rm = TRUE)
    },
    "q96" = function(x) {
      stats::quantile(x, 0.96, na.rm = TRUE)
    },
    "q97" = function(x) {
      stats::quantile(x, 0.97, na.rm = TRUE)
    },
    "q98" = function(x) {
      stats::quantile(x, 0.98, na.rm = TRUE)
    },
    "q99" = function(x) {
      stats::quantile(x, 0.99, na.rm = TRUE)
    }
  )
  return(estimates_func[f])
}

#' @noRd
correctFormat <- function(x, bigMark, decimalMark, significativeDecimals) {
  if (is.numeric(x)) {
    format(
      x,
      big.mark = bigMark,
      decimal.mark = decimalMark,
      nsmall = ifelse(x %% 1 == 0, 0, 2)
    )
  }
}
#' referenceGroup <- lapply(referenceGroup)
#' # check other variables --> list of characters
#' # check that grouping + variables is present
#' if (is.na(numericVariables)) {
#'   numericVariables <- findNumericVariables(x)
#' }
#' variables <- c(
#'   grouping, numericVariables, dateVariables, categoricalVariables,
#'   binaryVariables, unlist(otherVariables)
#' )
#' variablesPresence <- variables %in% colnames(x)
#' if (!all(variablesPresence)) {
#'   errorVariables <- paste0(variables[!variablesPresence], collapse = ", ")
#'   stop(paste0("Variables: ", errorVariables, " are not present in x."))
#' }
#' if (is.null(order)) {
#'   order <- colnames(x)
#'   order <- order[order %in% variables]
#' }
#'
#' values <- summaryValues(x, variables)
#'
#' if (!is.null(numericVariables)) {
#'   functions <- findFunctions(numericFormat)
#'   numericSummary <- x %>%
#'     summarise(functions, variables, ungroup)
#'   # group, variable, function, result
#' }
#'
#' #' add percentages and formats to each number
#'
#' formats <- ""
#' ln <- 1
#' for (variable in order) {
#'   for (format in formats[[variable]]) {
#'     format <- gsub("var", paste0(".data$", variable), format)
#'     result <- result %>%
#'       mutate(paset0("line_", ln) := paste0(
#'       ))
#'     ln <- ln + 1
#'   }
#' }

