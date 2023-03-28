x <- variables %>%
  dplyr::mutate(get_category = dplyr::if_else(
    .data$variable_classification == "categorical" &
      .data$fun %in% c("count", "%"),
    1,
    0
  ))
x <- x %>%
  dplyr::filter(.data$get_category == 1) %>%
  tidyr::separate("value", c("value", "category"), sep = ": ", extra = "merge") %>%
  dplyr::union_all(
    x %>%
      dplyr::filter(.data$get_category == 0) %>%
      dplyr::mutate(category = NA)
  ) %>%
  dplyr::select(-"get_category")

resultsFormat <- variables %>%
  #dplyr::filter(.data$variable_classification != "categorical") %>%
  dplyr::select("variable_classification", "format") %>%
  dplyr::distinct() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(result = getEvalString(
    .data$format, .data$variable_classification
  )) %>%
  dplyr::ungroup()

xx <- x %>%
  tidyr::pivot_wider(names_from = "fun", values_from = "value") %>%
  dplyr::inner_join(
    resultsFormat, by = c("variable_classification", "format")
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(result = eval(parse(text = .data$result))) %>%
  dplyr::select("groupping", "order", "group", "variable", "variable_classification", "format", "category", "result")

x <- xx %>%
  dplyr::select("variable", "variable_classification", "format", "order", "group", "category") %>%
  dplyr::distinct()

x <- x %>%
  dplyr::inner_join(
    x %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarise(
        number_formats_group = dplyr::n_distinct(.data$format),
        .groups = "drop"
      ),
    by = "group"
  ) %>%
  dplyr::mutate(number_formats_group = dplyr::if_else(
    is.na(.data$group), 0, .data$number_formats_group
  )) %>%
  dplyr::inner_join(
    x %>%
      dplyr::group_by(.data$variable) %>%
      dplyr::summarise(
        number_lines_variable = dplyr::n_distinct(.data$format, .data$category),
        number_formats_variable = dplyr::n_distinct(.data$format),
        .groups = "drop"
      ),
    by = "variable"
  ) %>%
  dplyr::group_by(.data$order) %>%
  dplyr::mutate(num_line = dplyr::row_number()) %>%
  dplyr::ungroup()

x %>%
  dplyr::mutate(label = dplyr::if_else(
    .data$number_formats_group != 1 &
      .data$number_formats_variable != 1,
    paste0(.data$variable, " ", .data$format),
    dplyr::if_else(
      .data$variable_classification == "categorical",
      .data$category,
      .data$variable
    )
  )) %>%
  dplyr::left_join(
    xx,
    by = c(
      "order", "group", "variable", "variable_classification", "format",
      "category"
    )
  ) %>%
  dplyr::select("groupping", "label", "result", "num_line", "variable") %>%
  dplyr::union_all(
    x %>%
      dplyr::filter(!is.na(group)) %>%
      dplyr::select("group") %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        x %>%
          dplyr::filter(.data$number_formats_group == 1) %>%
          dplyr::select("group", "format") %>%
          dplyr::distinct(),
        by = "group"
      ) %>%
      dplyr::mutate(
        label = dplyr::if_else(
          !is.na(.data$format),
          paste0(.data$group, " ", .data$format),
          .data$group
        ),
        groupping = as.character(NA),
        result = as.character(NA),
        num_line = -1,
        variable = as.character(NA)
      ) %>%
      dplyr::select(-c("format", "group"))
  ) %>%
  dplyr::union_all(
    x %>%
      dplyr::filter(.data$number_lines_variable > 1) %>%
      dplyr::select("variable") %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        x %>%
          dplyr::filter(.data$number_formats_variable == 1) %>%
          dplyr::select("variable", "format") %>%
          dplyr::distinct(),
        by = "variable"
      ) %>%
      dplyr::mutate(
        label = dplyr::if_else(
          !is.na(.data$format),
          paste0(.data$variable, " ", .data$format),
          .data$variable
        ),
        groupping = as.character(NA),
        result = as.character(NA),
        num_line = 0,
        variable = as.character(NA)
      ) %>%
      dplyr::select(-"format")
  )
