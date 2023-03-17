# tableCharacteristics package

## operators: 
mean, median, sd, qXX, count, distinct, min, max, %

### tableCharacteristics(
  x,
  grouping = NULL,
  refernceGroup = NULL,
  numericVariables = NA,
  numericFormat = "median [q25 - q75]"
  dateVariables = NA,
  dateFromat = "median [min - max]"
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
  numbersignificativeDecimals = 2
)
output: variable, group1, group2, asmd group2, group3, asmd group 3
variable, comparator, exposure1, asmd_exposure1, exposure2, asmd_exposure2
NA, N = xxxxx, N = xxxxx, NA, N = xxxx, NA 
age median [q25 - q75], 

### summaryCharacteristics(
  x,
  numericVariables = NA,
  numericFormat = c("mean", "sd", "median", "q25", "q75", "min", "max"),
  dateVariables = NA,
  dateFromat = c("mean", "sd", "median", "q25", "q75", "min", "max"),
  categoricalVariables = NA,
  categoricalFormat = c("count", "distinct"),
  binaryVariables = NA,
  binaryFormat = c("count"),
  otherVariables = list(),
  otherFormat - list(),
  minimumCellCount = 5
)
output: grouping, variable, type, estimate, group

### asmd(
  x,
  grouping,
  referenceGroup,
  numericVariables = NA,
  categoricalVariables = NA
)
output: variables, referenceGroup, comparedGroup, value

### tableCharcteristicsFromSummary(
  summary,
  asmd = NULL,
  numericVariables = NA,
  numericFormat = "median [q25 - q75]"
  dateVariables = NA,
  dateFromat = "median [min - max]"
  categoricalVariables = NA,
  categoricalFormat = "count (%)",
  binaryVariables = NA,
  binaryFormat = "count (%)",
  otherVariables = list(),
  otherFormat = list(),
  bigMark = ",",
  decimal = ".",
  numbersignificativeDecimals = 2
)
output: tableCharacteristics

### INTERNAL:
asmdCategorical()
asmdNumeric()

### tableCharacteristics <- function (
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
  order = NULL,
  bigMark = ",",
  decimal = ".",
  numbersignificativeDecimals = 2
) {
  #' check other variables --> list of characters
  #' check that grouping + variables is present
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

### variable function
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

  #' variable variable_type functions row_name variable_id column_name value smd
  #' age "age mean (sd)" var1 column_1 'paste0(.data$var1_mean, " (", .data$var1_sd, ")")' TRUE
  
}

### getFunctions <- function(format, varType) {
  
}

### subFormat <- function(format, varType) {
  
}
