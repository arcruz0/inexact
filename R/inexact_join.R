#' Performs a fuzzy (inexact) join, trusting the algorithm by default.
#'
#' The function allows to provide custom matches.
#'
#' @importFrom data.table "%chin%"
#'
#' @export

inexact_join <- function(x, y, by, max_dist = Inf, 
                         method = c("osa", "lv", "dl", "hamming", "lcs", 
                                    "qgram", "cosine", "jaccard", "jw", 
                                    "soundex"), 
                         mode = "left", custom_match = NULL, ignore_case = FALSE, 
                         match_cols = FALSE, output = "auto",
                         ...) {
  # convert data frames to data.tables
  class_original_x <- class(x)
  dt_x <- data.table::as.data.table(x); dt_y <- data.table::as.data.table(y)
  
  # get unique values in both ID variables
  v_unique_ids_x <- unique(dt_x[[by]]); v_unique_ids_y <- unique(dt_y[[by]])
  
  # calculate string distances as a matrix
  st <- stringdist::stringdistmatrix(
    a = v_unique_ids_x, b = v_unique_ids_y, useNames = T, ...
  )
  
  f_min_matrix <- function(x){colnames(st)[which(x == min(x) & x <= max_dist)[1]]}
  
  dt_st <- data.table::data.table(
    orig = v_unique_ids_x, 
    rep  = as.vector(apply(st, 1, f_min_matrix)),
    .dist = apply(st, 1, min)
  )
  
  if (!is.null(custom_match)){
    dt_custom_match <- data.table::data.table(
      orig = names(custom_match),
      rep  = custom_match,
      .dist = -1
    )
    
    st_long <- data.table::melt(data.table::as.data.table(st, keep.rownames = T), 
                                id.vars = "rn")
    
    data.table::setnames(st_long, c("orig", "rep", ".dist"))
    
    st_long[, .dist := .dist * -1]
    
    dt_custom_match[.dist == -1] <- data.table::merge.data.table(
      x = dt_custom_match[.dist == -1, !c(".dist")], 
      y = st_long,
      by = c("orig", "rep"),
      all.x = T, sort = F
    )
    
    dt_st[orig %chin% dt_custom_match[, orig],] <- dt_custom_match
  }
  
  dt_st <- data.table::merge.data.table(
    dt_st, data.table:::unique.data.table(dt_y, by = by),
    by.x = "rep", by.y = by,
    all.x = T, sort = F
  )
  
  ret <- data.table::merge.data.table(dt_x, dt_st, 
                                      by.x = by, by.y = "orig",
                                      all.x = T, sort = F)
  
  data.table::setnames(ret, old = "rep", new = ".match")
  
  data.table::setcolorder(ret, c(names(x), setdiff(names(y), names(x))))
  
  ret[, .custom_match := data.table::fifelse(
    .dist < 0, T, F
  )]
  
  ret[, .dist := abs(.dist)]
  
  if (match_cols == F){
    ret <- ret[, !c(".match", ".dist", ".custom_match")]
  }
  
  if (output == "data.table" | "data.table" %in% class_original_x){
    return(ret[])
  } else if ((output == "tibble" | "tibble" %in% class_original_x) & requireNamespace("tibble", quietly = TRUE)){
    return(tibble::as_tibble(ret[]))
  } else if ((output == "tibble" | "tibble" %in% class_original_x) & !requireNamespace("tibble", quietly = TRUE)){
    warning("Package 'tibble' not installed. Returning a 'data.frame' instead.")
    return(as.data.frame(ret[]))
  } else {
    return(as.data.frame(ret[]))
  }
  
}
