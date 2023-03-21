#' @noRd
asmdBinary <- function(x, variables = NULL, groupName = "group", weight = NULL) {
  if (is.null(variables)) {
    variables <- colnames(x)[!(colnames(x) %in% c(groupName, weight))]
  }
  if (is.null(weight)) {
    x <- x %>% mutate(weight = 1)
  } else {
    x <- x %>% rename("weight" = dplyr::all_of(weight))
  }
  x <- x %>% rename("group" = dplyr::all_of(groupName))
  for (variable in variables) {
    lab <- unique(x[[variable]])
    if (!all(lab %in% c(0, 1))) {
      x <- dplyr::mutate(x, !!variable := dplyr::if_else(.data[[variable]] == .env$lab[1], 0, 1))
    }
  }
  lab <- unique(x$group)
  x %>%
    dplyr::mutate(group = dplyr::if_else(.data$group == .env$lab[1], 1, 2)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(variables),
      list(
        mean = function(x) {Hmisc::wtd.mean(x, .data$weight)},
        var = function(x) {Hmisc::wtd.var(x, .data$weight)}
      ),
      .names = "{.col} {.fn}"
    )) %>%
    tidyr::pivot_longer(!"group", names_to = "variable.func") %>%
    tidyr::separate("variable.func", c("variable", "func"), " ") %>%
    tidyr::pivot_wider(names_from = c("func", "group"), values_from = "value") %>%
    dplyr::mutate(asmd = abs(.data$mean_1-.data$mean_2)/sqrt((.data$var_1+.data$var_2)/2)) %>%
    dplyr::select("variable", "asmd") %>%
    mutate(asmd_type = "binary")

}

#' @noRd
asmdContinuous <- function(x, variables = NULL, groupName = "group", weight = NULL) {
  if (is.null(variables)) {
    variables <- colnames(x)[!(colnames(x) %in% c(groupName, weight))]
  }
  if (is.null(weight)) {
    x <- x %>% mutate(weight = 1)
  } else {
    x <- x %>% rename("weight" = dplyr::all_of(weight))
  }
  x <- x %>% rename("group" = dplyr::all_of(groupName))
  lab <- unique(x$group)
  x %>%
    dplyr::mutate(group = dplyr::if_else(.data$group == .env$lab[1], 1, 2)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(variables),
      list(
        mean = function(x) {Hmisc::wtd.mean(x, .data$weight)},
        var = function(x) {Hmisc::wtd.var(x, .data$weight)}
      ),
      .names = "{.col} {.fn}"
    )) %>%
    tidyr::pivot_longer(!"group", names_to = "variable.func") %>%
    tidyr::separate("variable.func", c("variable", "func"), " ") %>%
    tidyr::pivot_wider(names_from = c("func", "group"), values_from = "value") %>%
    dplyr::mutate(asmd = abs(.data$mean_1-.data$mean_2)/sqrt(.data$var_1+.data$var_2)) %>%
    dplyr::select("variable", "asmd") %>%
    mutate(asmd_type = "continuous")

}

#' @noRd
asmdCategorical <- function(x, variables = NULL, groupName = "group", weight = NULL) {
  if (is.null(variables)) {
    variables <- colnames(x)[!(colnames(x) %in% c(groupName, weight))]
  }
  if (is.null(weight)) {
    x <- x %>% dplyr::mutate(weight = 1)
  } else {
    x <- x %>% dplyr::rename("weight" = dplyr::all_of(weight))
  }
  x <- x %>%
    dplyr::rename("group" = dplyr::all_of(groupName)) %>%
    dplyr::select("group", "weight", dplyr::all_of(variables))
  lab <- unique(x$group)
  if (length(lab) != 2) {
    stop("Number of labels in group column different from 2.")
  }
  x <- x %>%
    dplyr::mutate(group = dplyr::if_else(.data$group == .env$lab[1], 1, 0))
  denominator <- x %>%
    dplyr::group_by(.data$group) %>%
    dplyr::tally(wt = .data$weight, name = "denominator")
  result <- NULL
  for (k in 1:length(variables)) {
    y <- x %>%
      dplyr::rename("label" = dplyr::all_of(variables[k])) %>%
      dplyr::group_by(.data$group,.data$label) %>%
      dplyr::tally(wt = .data$weight) %>%
      dplyr::right_join(denominator, by = "group") %>%
      dplyr::mutate(percentage = dplyr::if_else(
        is.na(.data$n), 0, .data$n/.data$denominator
      )) %>%
      dplyr::select("label", "group", "percentage") %>%
      tidyr::pivot_wider(names_from = "group", values_from = "percentage", values_fill = 0)
    TT <- y[["1"]]
    CC <- y[["0"]]
    result <- result %>% dplyr::union_all(dplyr::tibble(
      variable = variables[k],
      asmd = asmdFromPercentage(TT, CC)
    ))
  }
  result <- result %>% mutate(asmd_type = "categorical")
  return(result)
}

#' @noRd
asmdFromPercentage <- function(TT, CC) {
  TT <- TT[-1]
  CC <- CC[-1]
  n <- length(TT)
  vect <- TT - CC
  TT1 <- matrix(rep(TT, n), nrow = n, byrow = TRUE)
  TT2 <- matrix(rep(TT, n), nrow = n, byrow = FALSE)
  CC1 <- matrix(rep(CC, n), nrow = n, byrow = TRUE)
  CC2 <- matrix(rep(CC, n), nrow = n, byrow = FALSE)
  S <- (TT1*TT2 + CC1*CC2) / 2
  diag(S) <- (TT*(1-TT) + CC*(1-CC)) / 2
  asmd <- as.numeric(sqrt(vect %*% solve(S) %*% vect))
  return(asmd)
}
