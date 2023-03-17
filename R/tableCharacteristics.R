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
#' @param decimal Decimal separator.
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
    refernceGroup = NULL,
    numericVariables = NA,
    numericFormat = "median [q25 - q75]",
    dateVariables = NA,
    dateFromat = "median [min - max]",
    categoricalVariables = NA,
    categoricalFormat = "count (%)",
    binaryVariables = NA,
    binaryFormat = "count (%)",
    otherVariables = list(),
    otherFormat = list(),
    groupNames = list(),
    order = NULL,
    bigMark = ",",
    decimal = ".",
    significativeDecimals = 2
) {

  # check other variables --> list of characters
  # check that grouping + variables is present
  if (is.na(numericVariables)) {
    numericVariables <- findNumericVariables(x)
  }
  variables <- c(
    grouping, numericVariables, dateVariables, categoricalVariables,
    binaryVariables, unlist(otherVariables)
  )
  variablesPresence <- variables %in% colnames(x)
  if (!all(variablesPresence)) {
    errorVariables <- paste0(variables[!variablesPresence], collapse = ", ")
    stop(paste0("Variables: ", errorVariables, " are not present in x."))
  }
  if (is.null(order)) {
    order <- colnames(x)
    order <- order[order %in% variables]
  }

  colnames(x) <- newNames
  if (is.null(groupVariable)) {
    x <- x %>%
      mutate(group = NA)
  }

  result <- NULL

  #' do for all
  if (!is.null(numericVariables)) {
    functions <- findFunctions(numericFormat)
    numericSummary <- x %>%
      summarise(functions, variables, ungroup)
    # group, variable, function, result
  }

  #' add percentages and formats to each number

  formats <- ""
  ln <- 1
  for (variable in order) {
    for (format in formats[[variable]]) {
      format <- gsub("var", paste0(".data$", variable), format)
      result <- result %>%
        mutate(paset0("line_", ln) := paste0(
        ))
      ln <- ln + 1
    }
  }

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

getFunctions <- function(format, varType) {

}

subFormat <- function(format, varType) {

}
