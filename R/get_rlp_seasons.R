#' Get fixture and result data from Rugby League Project
#'
#' @param rlp_competition_code A string representing the competition, as used on Rugby League Project.
#' @param years A vector of integers that represent the first year of the seasons wanted.
#'
#' @return A `tibble` with each row aligning to a match
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_rlp_seasons("nrl", 2019)
get_rlp_seasons <- function(rlp_competition_code, years) {
  if (!is.numeric(years)) stop("`years` must be of type `numeric`")

  base_url <- "https://www.rugbyleagueproject.org/seasons/"
  base_url_with_rlp_competition_code <- paste0(base_url, rlp_competition_code)

  lapply(years, \(year) {
    season_url <- paste0(base_url_with_rlp_competition_code, "-", year, "/results.html")
    html <- rvest::read_html(season_url)
    season <- html %>% rvest::html_element(".list")

    # convert to table, get necessary columns only
    season_table <- (season %>% rvest::html_table(header = T))[-1, -1]
    season_cols <- c("date", "time", "home_team", "home_score", "away_team", "away_score", "referee_name", "venue", "crowd")
    season_table <- season_table[, seq_along(season_cols)]
    names(season_table) <- season_cols

    # rearrange round numbers in table, make numeric columns numeric
    round_info_rows <- unlist(season_table[, 1]) == unlist(season_table[, 2])
    round_names <- unlist(season_table[, 1])[round_info_rows]
    season_table <- season_table[!round_info_rows,]
    season_table$home_score <- as.numeric(season_table$home_score)
    season_table$away_score <- as.numeric(season_table$away_score)
    season_table$crowd <- as.numeric(gsub(",", "", season_table$crowd))

    match_ids <- season %>%
      rvest::html_elements("a.rlplnk") %>%
      rvest::html_attr("href")|>
        vapply(get_end_slug, "") |>
        as.numeric()

    # remove bye rounds from round_names
    round_names <- round_names[!grepl("Bye: ", round_names)]

    # loop through round_names, ignoring bye rows
    last_entry_was_true <- F
    round_result <- NULL
    previous_number <- 0
    for(i in seq_along(round_info_rows)) {
      if (round_info_rows[i] && last_entry_was_true) next
      if (round_info_rows[i] && !last_entry_was_true) {
        previous_number <- previous_number + 1
        last_entry_was_true <- T
      } else {
        round_result <- c(round_result, previous_number)
        last_entry_was_true <- F
      }
    }


    season_table <- tibble::tibble(rlp_match_id = match_ids, rlp_round_name = round_names[unlist(round_result)], season_table)

  }) %>% dplyr::bind_rows()


}