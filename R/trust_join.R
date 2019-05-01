#' Perform a fuzzy join trusting the algorithm.
#'
#' Performs a fuzzy join that trusts the algorithm. Allows to provide custom matches.
#'
#' @export
trust_join <- function(x, y, by, max_dist = Inf,
                       method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                                  "cosine", "jaccard", "jw", "soundex"),
                       mode = "inner",
                       custom_match = NULL,
                       ignore_case = FALSE,
                       match_cols = FALSE,
                       ...) {
  var_by <- enquo(by)
  var_by_x <- sym(str_c(by, ".x"))
  var_by_y <- sym(str_c(by, ".y"))
  var_match <- sym(str_c(".", by, "_match"))

  v_order <- unique(x[[quo_name(var_by)]])

  res <- stringdist_join(x = x, y = y, by = by, max_dist = max_dist, method = method,
                         mode = mode, ignore_case = ignore_case,
                         distance_col = ".dist",
                         ... = ...)

  if (!is.null(custom_match)){

    df_custom_match <- data.frame(
      value_x = names(custom_match),
      value_y = custom_match
    )

    df_custom_match <- df_custom_match %>%
      mutate(.dist2 = -1)

    names(df_custom_match) <- c(quo_name(var_by_x), quo_name(var_by_y),
                                ".dist2")

    suppressMessages({suppressWarnings({res <- res %>% left_join(df_custom_match)})})

    res <- res %>%
      replace_na(list(.dist2 = Inf)) %>%
      rowwise() %>%
      mutate(.dist = min(.dist, .dist2)) %>%
      ungroup() %>%
      select(-.dist2)
  }

  res <- res %>%
    group_by(!!var_by_x) %>%
    arrange(.dist) %>%
    slice(1) %>%
    ungroup() %>%
    slice(match(v_order, .[[quo_name(var_by_x)]]))

  names(res)[1] <- quo_name(var_by)
  res[[quo_name(var_match)]] <- res[[quo_name(var_by_y)]]

  res[[quo_name(var_by_x)]] <- NULL; res[[quo_name(var_by_y)]] <- NULL

  res <- select(res, !!var_by, !!var_match, ".dist", everything())

  if (match_cols == F) {
    res[[quo_name(var_match)]] <- NULL
    res$.dist <- NULL
  }

  return(res)

}
