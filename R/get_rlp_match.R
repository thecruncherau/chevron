#' Get data for a single match on Rugby League Project
#'
#' @param rlp_match_id An integer that represents a match ID found on Rugby League Project.
#'
#' @return A list containing match, team and player information.
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export
#'
#' @examples
#' get_rlp_match(39999)
get_rlp_match <- function(rlp_match_id) {
  if (!is.numeric(rlp_match_id)) stop("`rlp_match_id` must be of type `numeric`")

  base_url <- "https://www.rugbyleagueproject.org/matches/"

  # parse into easier formats for scraping
  # todo: need to more helpful messages if 404 / redirect
  html <- rvest::read_html(paste0(base_url, rlp_match_id))
  program <- html %>% rvest::html_element(".program")
  program_table <- program %>% rvest::html_table()
  info <- html %>% rvest::html_elements("dt, dd")
  info_text <- info %>% rvest::html_text2()

  result <- list()

  #####

  # add match_info

  # referee info
  if ("Referees" %in% info_text || "Referee" %in% info_text) {
    two_or_more_referees <- "Referees" %in% info_text
    referee_index <- match(ifelse(two_or_more_referees, "Referees", "Referee"), info_text) + 1
    referee <- unlist(strsplit(info_text[referee_index], ", "))
    referee_origin <- strsplit(referee, " (", fixed = T) |> vapply(\(text) text[2] |> substr(1, nchar(text[2]) - 1), "")
    referee_ids <- info[referee_index] %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href") |>
        vapply(get_end_slug, "") |>
        as.numeric()

    referee_df <- data.frame(rlp_referee_id = referee_ids, name = referee |>
      strsplit(" (", fixed = T) |>
      vapply(\(text) text[1], ""), type = "on-field", origin = referee_origin)
  } else { NA -> referee -> referee_df }


  match_info <- list(rlp_match_id = rlp_match_id,
                     rlp_round_name = html %>% rvest::html_element("h2") %>% rvest::html_text2(),
                     referee_info = tibble::tibble(referee_df),
                     status = info_text[match("Status", info_text) + 1],
                     date = info_text[match("Date", info_text) + 1], # todo: convert to date
                     crowd = gsub(",", "", info_text[match("Crowd", info_text) + 1]) |> as.numeric()
  )
  result <- c(result, match_info = list(match_info))

  #####

  # get hteam and ateam names todo: could use 'status == complete'?
  teams <- (rvest::html_elements(html, "h3")[1] %>%
    rvest::html_text2() |>
      strsplit(" vs. "))[[1]]

  if (length(teams) == 1) {
    teams <- strsplit(teams, " def. ")[[1]]
    if (length(teams) == 1) teams <- strsplit(teams, " lost to")[[1]]

    teams <- strsplit(teams, " ")
    teams <- c(paste(utils::head(teams[[1]], -1), collapse = " "), paste(utils::head(teams[[2]], -1), collapse = " "))
  }
  if (length(teams) != 2) stop("team names could not be scraped")

  ### get team, player and coach ids
  ids <- program %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href")
  scorers <- ids[seq_len(length(ids) - length(unique(ids)))] |>
    vapply(get_end_slug, "") |>
    as.numeric()
  ids <- rev(unique(rev(ids)))

  player_info_available <- T
  if (length(ids) != 0) {
    ids <- ids[c(seq(1, length(ids), 2), seq(2, length(ids), 2))]
  } else {
    player_info_available <- F
    warning("player data is not available for this match") # todo: should this warning be only for completed matches?
  }

  away_coach_present <- grepl("/coaches", ids[length(ids)])

  player_ids <- ids[!grepl("/coaches", ids)] |>
    vapply(get_end_slug, "") |>
    as.numeric()
  coach_ids <- ids[grepl("/coaches", ids)] |>
    vapply(get_end_slug, "") |>
    as.numeric()

  # if one or more coach is missing
  if (length(coach_ids) == 0) coach_ids <- c(NA, NA)
  if (length(coach_ids) == 1) coach_ids <- ifelse(away_coach_present, c(NA, coach_ids), c(coach_ids, NA))

  team_ids <- program %>%
    rvest::html_elements("span") %>%
    rvest::html_attr("href")
  team_ids <- team_ids[!is.na(team_ids)] |>
    utils::head(2) |>
    vapply(get_end_slug, "") |>
    as.numeric()
  ###

  # add team info: team_id, coach, ht_score, penalties, scrums, penalties IF AVAILABLE
  team_info <- tibble::tibble(
    rlp_team_id = team_ids,
    team_name = teams,
    rlp_coach_id = coach_ids,
    coach = program_table[match("COACH", unlist(program_table[, 3])), c(1, 5)] |>
      unlist() |>
      unname(),
    ht_score = program_table[match("Halftime Score", unlist(program_table[, 1])), c(2, 4)] |>
      unlist() |>
      unname() |>
      as.numeric(),
    ft_score = colnames(program_table)[c(2, 4)] |> as.numeric(),
    scrums = program_table[match("Scrums", unlist(program_table[, 1])), c(2, 4)] |>
      unlist() |>
      unname() |>
      as.numeric(),
    penalties = program_table[match("Penalties", unlist(program_table[, 1])), c(2, 4)] |>
      unlist() |>
      unname() |>
      as.numeric(),
    is_home = c(T, F),
    row.names = NULL
  )
  result <- c(result, team_info = list(team_info))

  #####

  if (player_info_available) {
    (match("Teams", unlist(program_table[, 3])) + 1):(nrow(program_table) - 2) -> player_names_range
    line_ups <- program_table[player_names_range,]
    # remove blank rows of table
    line_ups <- line_ups[line_ups[, 1] != "",]

    player_info <- data.frame(
      order = seq_along(player_ids),
      rlp_player_id = player_ids,
      name = unlist(line_ups[, c(1, 5)]),
      number = unlist(line_ups[, c(2, 4)]) |> as.numeric(),
      position = unlist(line_ups[, 3]),
      team_name = rep(teams, each = nrow(line_ups)), # assumes formatting is equal on both sides
      is_captain = grepl("(c)", unlist(line_ups[, c(1, 5)]), fixed = T),
      # tries = 0,
      # goals = 0,
      # goal_attempts = 0,
      # sin_bins = 0,
      # send_offs = 0,
      row.names = NULL
    )

    # get try-scoring statistics
    tries_index <- match("T", program_table[, 3] |> unlist())
    tries_range <- 1:min(match(NA, unlist(program_table[tries_index:nrow(program_table), 1])) - 1,
                         match(NA, unlist(program_table[tries_index:nrow(program_table), 5])) - 1,
                         max(
                           match("", unlist(program_table[tries_index:nrow(program_table), 1])) - 1,
                           match("", unlist(program_table[tries_index:nrow(program_table), 5])) - 1), na.rm = T) + tries_index - 1
    tries_df <- program_table[tries_range,]
    tries_count <- seq_len(nrow(tries_df)) |>
      vapply(\(row_number) {
        no_tries <- NULL
        if (tries_df[row_number, 1] != "") {
          no_tries <- c(no_tries, ifelse(tries_df[row_number, 2] == "", 1, as.numeric(tries_df[row_number, 2])))
        } else no_tries <- c(no_tries, NA)
        if (tries_df[row_number, 5] != "") {
          no_tries <- c(no_tries, ifelse(tries_df[row_number, 4] == "", 1, as.numeric(tries_df[row_number, 4])))
        } else no_tries <- c(no_tries, NA)

        return(no_tries)
      }, c(0, 0)) |>
      as.vector()

    # add try data
    player_info <- merge(player_info, data.frame(rlp_player_id = scorers[seq_along(tries_count[!is.na(tries_count)])], tries = tries_count[!is.na(tries_count)]),
                         by = "rlp_player_id", all.x = T
    )
    # remove try-scorers
    scorers <- scorers[-seq_along(tries_count[!is.na(tries_count)])]


    # get goal-scoring statistics
    goals_index <- match("G", program_table[, 3] |> unlist())
    goals_range <- 1:min(match(NA, unlist(program_table[goals_index:nrow(program_table), 1])) - 1,
                         match(NA, unlist(program_table[goals_index:nrow(program_table), 5])) - 1,
                         match("Teams", unlist(program_table[goals_index:nrow(program_table), 3])) - 1,
                         max(
                           match("", unlist(program_table[goals_index:nrow(program_table), 1])) - 1,
                           match("", unlist(program_table[goals_index:nrow(program_table), 5])) - 1), na.rm = T) + goals_index - 1
    goals_df <- program_table[goals_range,]


    goals_count <- seq_len(nrow(goals_df)) |>
      vapply(\(row_number) {
        no_goals <- NULL
        no_goals_attempts <- NULL

        if (goals_df[row_number, 1] != "") {
          goal_kicking_performance <- goals_df[row_number, 2] |>
            as.character() |>
            strsplit("/") |>
            unlist() |>
            as.numeric() # goals/attempts
          no_goals <- c(no_goals, goal_kicking_performance[1])
          no_goals_attempts <- c(no_goals_attempts, goal_kicking_performance[2])
        } else { no_goals <- c(no_goals, NA); no_goals_attempts <- c(no_goals_attempts, NA) }

        if (goals_df[row_number, 5] != "") {
          goal_kicking_performance <- goals_df[row_number, 4] |>
            as.character() |>
            strsplit("/") |>
            unlist() |>
            as.numeric() # goals/attempts
          no_goals <- c(no_goals, goal_kicking_performance[1])
          no_goals_attempts <- c(no_goals_attempts, goal_kicking_performance[2])
        } else { no_goals <- c(no_goals, NA); no_goals_attempts <- c(no_goals_attempts, NA) }

        return(c(no_goals, no_goals_attempts)[c(1, 3, 2, 4)])
      }, c(0, 0, 0, 0)) |>
      as.vector()

    # add goal data
    player_info <- merge(player_info, data.frame(rlp_player_id = scorers[seq_len(length(goals_count[!is.na(goals_count)]) / 2)], goals = (goals_count[!is.na(goals_count)])[seq(1, by = 2, length.out = length(goals_count[!is.na(goals_count)]) / 2)],
                                                 goal_attempts = (goals_count[!is.na(goals_count)])[seq(2, by = 2, length.out = length(goals_count[!is.na(goals_count)]) / 2)]),
                         by = "rlp_player_id", all.x = T
    )
    # remove goal scorers
    scorers <- scorers[-seq_len(length(goals_count[!is.na(goals_count)]) / 2)]

    # get sin bin statistics
    sin_bins_index <- match("BIN", program_table[, 3] |> unlist())
    if (!is.na(sin_bins_index)) {
      sin_bins_range <- 1:min(match(NA, unlist(program_table[sin_bins_index:nrow(program_table), 1])) - 1,
                              match(NA, unlist(program_table[sin_bins_index:nrow(program_table), 5])) - 1,
                              match("Teams", unlist(program_table[sin_bins_index:nrow(program_table), 3])) - 1,
                              max(
                                match("", unlist(program_table[sin_bins_index:nrow(program_table), 1])) - 1,
                                match("", unlist(program_table[sin_bins_index:nrow(program_table), 5])) - 1), na.rm = T) + sin_bins_index - 1
      sin_bins_df <- program_table[sin_bins_range,]
      sin_bins_count <- seq_len(nrow(sin_bins_df)) |>
        vapply(\(row_number) {
          no_sin_bins <- NULL
          if (sin_bins_df[row_number, 1] != "") {
            no_sin_bins <- c(no_sin_bins, ifelse(sin_bins_df[row_number, 2] == "", 1, as.numeric(sin_bins_df[row_number, 2])))
          } else no_sin_bins <- c(no_sin_bins, NA)
          if (sin_bins_df[row_number, 5] != "") {
            no_sin_bins <- c(no_sin_bins, ifelse(sin_bins_df[row_number, 4] == "", 1, as.numeric(sin_bins_df[row_number, 4])))
          } else no_sin_bins <- c(no_sin_bins, NA)

          return(no_sin_bins)
        }, c(0, 0)) |>
        as.vector()

      # add sin bin data
      player_info <- merge(player_info, data.frame(rlp_player_id = scorers[seq_along(sin_bins_count[!is.na(sin_bins_count)])], sin_bins = sin_bins_count[!is.na(sin_bins_count)]),
                           by = "rlp_player_id", all.x = T
      )
      # remove sin binned players
      scorers <- scorers[-seq_along(sin_bins_count[!is.na(sin_bins_count)])]
    } else player_info$sin_bins <- 0


    # get send off statistics
    send_offs_index <- match("BIN", program_table[, 3] |> unlist())
    if (!is.na(send_offs_index)) {
      send_offs_range <- 1:min(match(NA, unlist(program_table[send_offs_index:nrow(program_table), 1])) - 1,
                               match(NA, unlist(program_table[send_offs_index:nrow(program_table), 5])) - 1,
                               match("Teams", unlist(program_table[send_offs_index:nrow(program_table), 3])) - 1,
                               max(
                                 match("", unlist(program_table[send_offs_index:nrow(program_table), 1])) - 1,
                                 match("", unlist(program_table[send_offs_index:nrow(program_table), 5])) - 1), na.rm = T) + send_offs_index - 1
      send_offs_df <- program_table[send_offs_range,]
      send_offs_count <- seq_len(nrow(send_offs_df)) |>
        vapply(\(row_number) {
          no_send_offs <- NULL
          if (send_offs_df[row_number, 1] != "") {
            no_send_offs <- c(no_send_offs, ifelse(send_offs_df[row_number, 2] == "", 1, as.numeric(send_offs_df[row_number, 2])))
          } else no_send_offs <- c(no_send_offs, NA)
          if (sin_bins_df[row_number, 5] != "") {
            no_send_offs <- c(no_send_offs, ifelse(send_offs_df[row_number, 4] == "", 1, as.numeric(send_offs_df[row_number, 4])))
          } else no_send_offs <- c(no_send_offs, NA)

          return(no_send_offs)
        }, c(0, 0)) |>
        as.vector()

      # add send off data
      player_info <- merge(player_info, data.frame(rlp_player_id = scorers[seq_along(send_offs_count[!is.na(send_offs_count)])], send_offs = send_offs_count[!is.na(send_offs_count)]),
                           by = "rlp_player_id", all.x = T
      )

      # todo: remove sent off players if more statistics are added
    } else player_info$send_offs <- 0

    # remove NAs from statistics
    player_info$tries[is.na(player_info$tries)] <- 0
    player_info$goals[is.na(player_info$goals)] <- 0
    player_info$goal_attempts[is.na(player_info$goal_attempts)] <- 0
    player_info$sin_bins[is.na(player_info$sin_bins)] <- 0
    player_info$send_offs[is.na(player_info$send_offs)] <- 0

    # rearrange to original ordering
    player_info <- player_info[order(player_info$order), -2] %>% tibble::tibble(row.names = NULL)

    # player_info <- merge(player_info, data.frame(rlp_player_id = scorers[seq_len(length(tries_count[!is.na(tries_count)]))], tries_count[!is.na[tries_count]]))


    # remove ' (c) '
    player_info$name <- ifelse(grepl("(c)", player_info$name, fixed = T),
                               player_info$name |> substr(1, nchar(player_info$name) - 4),
                               player_info$name
    )
  } else player_info <- NA


  result <- c(result, player_info = list(player_info))


  return(result)
}