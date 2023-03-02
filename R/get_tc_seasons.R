#' Get NRL fixture and result data from The Cruncher
#'
#' @param years A vector of integers that represent the year of the seasons wanted (2017- available)
#'
#' @return A `tibble` with each row aligning to a match
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' get_tc_seasons(2017:2023)
get_tc_seasons <- function(years) {
  data <- tibble::tibble(jsonlite::fromJSON("https://thecruncherau.vercel.app/league/m/data.json"))
  data[data$year %in% years,]
}