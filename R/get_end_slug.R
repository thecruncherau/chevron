#' @importFrom utils tail
get_end_slug <- function(str, last_n = 1) {
  slugs <- strsplit(str, "/")[[1]]
  return(utils::tail(slugs, last_n))
}