#' Perform a fuzzy join trusting the algorithm.
#'
#' Performs a fuzzy join that trusts the algorithm. Allows to provide custom matches.
#'
#' @importFrom dplyr "%>%"
#'
#' @export

trust_join <- function (x, y, by, max_dist = Inf, method = c("osa", "lv", "dl", 
                                                             "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"), 
                        mode = "inner", custom_match = NULL, ignore_case = FALSE, 
                        match_cols = FALSE, ...) {
  # generate symbols from argument values:
  
  var_by <- rlang::enquo(by)
  var_by_x <- rlang::sym(stringr::str_c(by, ".x"))
  var_by_y <- rlang::sym(stringr::str_c(by, ".y"))
  var_match <- rlang::sym(stringr::str_c(".", by, "_match"))
  
  v_order <- unique(x[[rlang::quo_name(var_by)]])
  
  res <- fuzzyjoin::stringdist_join(
    x = x, y = y, by = by, max_dist = max_dist, 
    method = method, mode = mode, ignore_case = ignore_case, 
    distance_col = ".dist", ... = ...)
  
  if (!is.null(custom_match)) {
    df_custom_match <- data.frame(value_x = names(custom_match), 
                                  value_y = custom_match)
    df_custom_match <- df_custom_match %>% dplyr::mutate(.dist2 = -1)
    names(df_custom_match) <- c(rlang::quo_name(var_by_x), 
                                rlang::quo_name(var_by_y), 
                                ".dist2")
    suppressMessages({
      suppressWarnings({
        res <- res %>% dplyr::left_join(df_custom_match)
      })
    })
    res <- res %>% 
      tidyr::replace_na(list(.dist2 = Inf)) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(.dist = min(.dist, .dist2)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-.dist2)
  }
  
  res <- res %>% 
    dplyr::group_by(!!var_by_x) %>% 
    dplyr::arrange(.dist) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::slice(match(v_order, .[[rlang::quo_name(var_by_x)]]))

  names(res)[which(names(res) == rlang::quo_name(var_by_x))] <- rlang::quo_name(var_by)

  res[[rlang::quo_name(var_match)]] <- res[[rlang::quo_name(var_by_y)]]
  
  res[[rlang::quo_name(var_by_y)]] <- NULL
  res <- dplyr::select(res, !!var_by, !!var_match, ".dist", dplyr::everything())
  
  if (match_cols == F) {
    res[[rlang::quo_name(var_match)]] <- NULL
    res$.dist <- NULL
  }
  
  # set column order appropiately:
  res <- res %>% dplyr::select(unique(c(colnames(x), colnames(y))))
  
  return(res)
}
