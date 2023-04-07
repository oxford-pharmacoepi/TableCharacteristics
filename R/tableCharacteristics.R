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
  checkmate::assertTibble(x, min.rows = 1, min.cols = 1)
  smd <- assertGroupVariable(x, groupVariable, referenceGroup)
  variables <- computeVariables(
    x, numericVariables, numericFormat, dateVariables, dateFormat,
    categoricalVariables, categoricalFormat, binaryVariables,
    binaryFormat, otherVariables, otherFormat
  )

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
    if (length(order) == 1 && is.na(order)) {
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
    groupVariable,
    bigMark,
    decimalMark,
    significativeDecimals
  )

  variables <- variables %>%
    dplyr::inner_join(
      result,
      by = c("variable", "variable_classification", "fun"),
      multiple = "all"
    )

variablesOld <- variables



  if (isTRUE(smd)) {
    asmdResults <- computeASMD(
      x, binaryVariables, numericVariables, categoricalVariables, dateVariables,
      groupVariable, referenceGroup
    )
  }

}

#' @noRd
summaryValues <- function(x, variables, groupVariable, bigMark, decimalMark, significativeDecimals) {
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
    result <- dplyr::union_all(
      result,
      getNumericValues(
        x, variablesNumeric, groupVariable, bigMark, decimalMark,
        significativeDecimals
      )
    )
  }
  # date variables
  variablesDate <- variables %>%
    dplyr::filter(.data$variable_classification == "date")
  if (nrow(variablesDate) > 0) {
    result <- dplyr::union_all(
      result,
      getDateValues(
        x, variablesDate, groupVariable, bigMark, decimalMark,
        significativeDecimals
      )
    )
  }
  # binary variables
  variablesBinary <- variables %>%
    dplyr::filter(.data$variable_classification == "binary")
  if (nrow(variablesBinary) > 0) {
    result <- dplyr::union_all(
      result,
      getBinaryValues(
        x, variablesBinary, groupVariable, bigMark, decimalMark,
        significativeDecimals
      )
    )
  }
  # categorical variables
  variablesCategorical <- variables %>%
    dplyr::filter(.data$variable_classification == "categorical")
  if (nrow(variablesCategorical) > 0) {
    result <- dplyr::union_all(
      result,
      getCategoricalValues(
        x, variablesCategorical, groupVariable, bigMark, decimalMark,
        significativeDecimals
      )
    )
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
niceNum <- function(x, bigMark, decimalMark, significativeDecimals) {
  if (all(x %% 1 == 0)) {
    significativeDecimals <- 0
  }
  base::format(
    round(x, significativeDecimals),
    big.mark = bigMark,
    decimal.mark = decimalMark,
    nsmall = significativeDecimals
  )
}

#' @noRd
getNumericValues <- function(x, variablesNumeric, groupVariable, bigMark, decimalMark, significativeDecimals) {
  functions <- variablesNumeric %>%
    dplyr::pull("fun") %>%
    unique()
  result <- dplyr::tibble(
    variable = character(),
    variable_classification = character(),
    fun = character(),
    value = character(),
    groupping = character()
  )
  for (k in seq_along(functions)) {
    variablesFunction <- variablesNumeric %>%
      dplyr::filter(.data$fun == .env$functions[k]) %>%
      dplyr::pull("variable")
    if (!is.null(groupVariable)) {
      result.k <- x %>%
        dplyr::group_by(.data[[groupVariable]])
    } else {
      result.k <- x
    }
    result.k <- result.k %>%
      dplyr::summarise(dplyr::across(
        .cols = dplyr::all_of(variablesFunction),
        .fns = getFunctions(functions[k]),
        .names = "{.col}"
      )) %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(variablesFunction),
        ~ niceNum(., bigMark, decimalMark, significativeDecimals)
      )) %>%
      tidyr::pivot_longer(
        dplyr::all_of(variablesFunction), names_to = "variable"
      ) %>%
      dplyr::mutate(
        fun = .env$functions[k], variable_classification = "numeric"
      )
    if (is.null(groupVariable)) {
      result.k <- dplyr::mutate(result.k, "groupping" = NA)
    } else {
      result.k <- result.k %>%
        dplyr::rename("groupping" = dplyr::all_of(groupVariable))
    }
    result <- dplyr::union_all(result, result.k)
  }
  return(result)
}

#' @noRd
getDateValues <- function(x, variablesDate, groupVariable, bigMark, decimalMark, significativeDecimals) {
  functions <- variablesDate %>%
    dplyr::pull("fun") %>%
    unique()
  result <- dplyr::tibble(
    variable = character(),
    variable_classification = character(),
    fun = character(),
    value = character(),
    groupping = character()
  )
  for (k in seq_along(functions)) {
    variablesFunction <- variablesDate %>%
      dplyr::filter(.data$fun == .env$functions[k]) %>%
      dplyr::pull("variable")
    if (!is.null(groupVariable)) {
      result.k <- x %>%
        dplyr::group_by(.data[[groupVariable]])
    } else {
      result.k <- x
    }
    result.k <- result.k %>%
      dplyr::summarise(dplyr::across(
        .cols = dplyr::all_of(variablesFunction),
        .fns = getFunctions(functions[k]),
        .names = "{.col}"
      ))
    if (dateFormats()$result[dateFormats()$format_key == functions[k]] == "date") {
      result.k <- result.k %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(variablesFunction),
          ~ as.character(as.Date(round(.x), origin = "1970-01-01"))
        ))
    } else {
      result.k <- result.k %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(variablesFunction),
          ~ niceNum(.x, bigMark, decimalMark, significativeDecimals)
        ))
    }
    result.k <- result.k %>%
      tidyr::pivot_longer(dplyr::all_of(variablesFunction), names_to = "variable") %>%
      dplyr::mutate(
        fun = .env$functions[k], variable_classification = "date"
      )
    if (is.null(groupVariable)) {
      result.k <- dplyr::mutate(result.k, "groupping" = NA)
    } else {
      result.k <- result.k %>%
        dplyr::rename("groupping" = dplyr::all_of(groupVariable))
    }
    result <- dplyr::union_all(result, result.k)
  }
  return(result)
}

#' @noRd
getBinaryValues <- function(x, variablesBinary, groupVariable, bigMark, decimalMark, significativeDecimals) {
  variablesFunction <- variablesBinary %>%
    dplyr::pull("variable") %>%
    unique()
  if (!is.null(groupVariable)) {
    result <- x %>%
      dplyr::group_by(.data[[groupVariable]])
  } else {
    result <- x
  }
  result <- result %>%
    dplyr::mutate(denominator = 1) %>%
    dplyr::summarise(dplyr::across(
      .cols = dplyr::all_of(c(variablesFunction, "denominator")),
      .fns = list("sum" = function(x) {sum(x)}),
      .names = "{.col}"
    )) %>%
    tidyr::pivot_longer(
      dplyr::all_of(variablesFunction),
      names_to = "variable",
      values_to = "count"
    ) %>%
    dplyr::mutate("%" = 100 * .data$count / .data$denominator) %>%
    dplyr::mutate(
      "%" = base::paste0(
        niceNum(.data[["%"]], bigMark, decimalMark, significativeDecimals),
        "%"
      ),
      count = niceNum(.data$count, bigMark, decimalMark, significativeDecimals)
    ) %>%
    dplyr::select(-"denominator") %>%
    tidyr::pivot_longer(c("count", "%"), names_to = "fun") %>%
    dplyr::inner_join(
      variablesBinary %>%
        dplyr::select("variable", "variable_classification", "fun"),
      by = c("variable", "fun")
    )
  if (is.null(groupVariable)) {
    result <- dplyr::mutate(result, "groupping" = NA)
  } else {
    result <- result %>%
      dplyr::rename("groupping" = dplyr::all_of(groupVariable))
  }
  return(result)
}

#' @noRd
getCategoricalValues <- function(x, variablesCategorical, groupVariable, bigMark, decimalMark, significativeDecimals) {
  functions <- variablesCategorical %>%
    dplyr::pull("fun") %>%
    unique()
  result <- dplyr::tibble(
    variable = character(),
    variable_classification = character(),
    fun = character(),
    value = character(),
    groupping = character()
  )
  if ("count" %in% functions || "%" %in% functions) {
    variablesFunction <- variablesCategorical %>%
      dplyr::filter(.data$fun %in% c("count", "%")) %>%
      dplyr::pull("variable") %>%
      unique()
    for (k in seq_along(variablesFunction)) {
      categories <- unique(x[[variablesFunction[k]]])
      if (is.null(groupVariable)) {
        categories <- dplyr::tibble(category = categories)
        toJoin <- "category"
        result.k <- x %>%
          dplyr::rename("category" = dplyr::all_of(variablesFunction[k])) %>%
          dplyr::group_by(.data$category)
      } else {
        groups <- x %>%
          dplyr::pull(dplyr::all_of(groupVariable)) %>%
          unique()
        categories <- tidyr::expand_grid(
          !!groupVariable := groups,
          category = categories
        )
        toJoin <- c(groupVariable, "category")
        result.k <- x %>%
          dplyr::rename("category" = dplyr::all_of(variablesFunction[k])) %>%
          dplyr::group_by(.data[[groupVariable]], .data$category)
      }
      result.k <- result.k %>%
        dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop") %>%
        dplyr::right_join(categories, by = dplyr::all_of(toJoin)) %>%
        dplyr::mutate(count = dplyr::if_else(
          is.na(.data$count), 0, .data$count
        ))
      if (!is.null(groupVariable)) {
        result.k <- dplyr::group_by(result.k, .data[[groupVariable]])
      }
      result.k <- result.k %>%
        dplyr::mutate(denominator = as.numeric(sum(.data$count))) %>%
        dplyr::mutate("%" = 100 * .data$count / .data$denominator) %>%
        dplyr::mutate(
          "%" = base::paste0(
            niceNum(
              .data[["%"]], bigMark, decimalMark, significativeDecimals
            ),
            "%"
          ),
          count = niceNum(
            .data$count, bigMark, decimalMark, significativeDecimals
          )
        ) %>%
        dplyr::mutate(
          count = paste0(.data$count, ": ", .data$category),
          "%" = paste0(.data[["%"]], ": ", .data$category)
        ) %>%
        dplyr::select(-c("denominator", "category")) %>%
        tidyr::pivot_longer(c("count", "%"), names_to = "fun") %>%
        dplyr::inner_join(
          variablesCategorical %>%
            dplyr::filter(.data$variable == .env$variablesFunction[k]) %>%
            dplyr::filter(.data$fun %in% c("count", "%")) %>%
            dplyr::select("variable", "variable_classification", "fun"),
          by = "fun"
        )
      if (is.null(groupVariable)) {
        result.k <- dplyr::mutate(result.k, "groupping" = NA)
      } else {
        result.k <- result.k %>%
          dplyr::rename("groupping" = dplyr::all_of(groupVariable))
      }
      result <- dplyr::union_all(result, result.k)
    }
  }
  if ("distinct" %in% functions) {
    variablesFunction <- variablesCategorical %>%
      dplyr::filter(.data$fun == "distinct") %>%
      dplyr::pull("variable")
    if (!is.null(groupVariable)) {
      result.k <- x %>%
        dplyr::group_by(.data[[groupVariable]])
    } else {
      result.k <- x
    }
    result.k <- result.k %>%
      dplyr::summarise(dplyr::across(
        dplyr::all_of(variablesFunction),
        list("distinct" = function(x){dplyr::n_distinct(x)}),
        .names = "{.col}"
      )) %>%
      tidyr::pivot_longer(
        dplyr::all_of(variablesFunction), names_to = "variable"
      ) %>%
      dplyr::mutate(value = format(
        round(.data$value), big.mark = bigMark
      )) %>%
      dplyr::mutate(
        fun = "distinct", variable_classification = "categorical"
      )
    if (is.null(groupVariable)) {
      result.k <- dplyr::mutate(result.k, "groupping" = NA)
    } else {
      result.k <- result.k %>%
        dplyr::rename("groupping" = dplyr::all_of(groupVariable))
    }
    result <- dplyr::union_all(result, result.k)
  }
  variablesCategorical <- variablesCategorical %>%
    dplyr::filter(!(.data$fun %in% c("count", "%", "distinct")))
  variablesFunction <- unique(variablesCategorical$variable)
  for (k in seq_along(variablesFunction)) {
    functions <- variablesCategorical %>%
      dplyr::filter(.data$variable == .env$variablesFunction[k]) %>%
      dplyr::pull("fun")
    categories <- x %>%
      dplyr::pull(dplyr::all_of(variablesFunction[k])) %>%
      unique()
    if (is.null(groupVariable)) {
      categories <- dplyr::tibble(category = categories)
      toJoin <- "category"
      result.k <- x %>%
        dplyr::select(
          dplyr::all_of(groupVariable),
          "category" = dplyr::all_of(variablesFunction[k])
        ) %>%
        dplyr::group_by(.data$category)
    } else {
      groups <- x %>%
        dplyr::pull(dplyr::all_of(groupVariable)) %>%
        unique()
      categories <- tidyr::expand_grid(
        !!groupVariable := groups,
        category = categories
      )
      toJoin <- c(groupVariable, "category")
      result.k <- x %>%
        dplyr::select(
          dplyr::all_of(groupVariable),
          "category" = dplyr::all_of(variablesFunction[k])
        ) %>%
        dplyr::group_by(.data[[groupVariable]], .data$category)
    }
    result.k <- result.k %>%
      dplyr::summarise(count_per_category = dplyr::n(), .groups = "drop") %>%
      dplyr::right_join(categories, by = dplyr::all_of(toJoin)) %>%
      dplyr::mutate(count_per_category = dplyr::if_else(
        is.na(.data$count_per_category), 0, .data$count_per_category
      ))
    if (!is.null(groupVariable)) {
      result.k <- dplyr::group_by(result.k, .data[[groupVariable]])
    }
    result.k <- result.k %>%
      dplyr::summarise(dplyr::across(
        .cols = "count_per_category",
        .fns = getFunctions(functions),
        .names = "{.fn}"
      )) %>%
      tidyr::pivot_longer(dplyr::all_of(functions), names_to = "fun") %>%
      dplyr::mutate(
        variable = .env$variablesFunction[k],
        variable_classification = "categorical"
      ) %>%
      dplyr::mutate(value = niceNum(
        .data$value, bigMark, decimalMark, significativeDecimals
      ))
    if (is.null(groupVariable)) {
      result.k <- dplyr::mutate(result.k, "groupping" = NA)
    } else {
      result.k <- result.k %>%
        dplyr::rename("groupping" = dplyr::all_of(groupVariable))
    }
    result <- dplyr::union_all(result, result.k)
  }
  return(result)
}
