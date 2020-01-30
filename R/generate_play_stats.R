#' Generate Player Statistics
#'
#' Get statistics for all the individual players.
#'
#' Input a play df. Return a data frame of statistics characterizing the play
#' patterns of each individual player.
#'
#' @param play_df a data frame of valid plays
#' @export
generate_player_stats <- function(play_df){

    df <- play_df %>%
        group_by(name) %>%
        summarise(
            play_count = n(),
            distinct_games = length(unique(game)),
            chair_hours = sum(length) / 60,
            expected_wins = sum(expected_wins)
        )

    winner_df <- play_df %>%
        filter(win == 1) %>%
        group_by(name) %>%
        summarise(win_count = n())

    df <- left_join(df, winner_df, by = "name") %>%
        mutate(
            win_count = tidyr::replace_na(win_count, 0),
            win_pct = win_count / play_count * 100,
            expected_win_pct = expected_wins / play_count * 100,
            expectation_comparison = win_pct - expected_win_pct
        ) %>%
        filter(name != "Anonymous player") %>%
        arrange(desc(chair_hours))

    colnames(df)[1] <- "player"

    df

}


#' Generate Game Statistics
#'
#' Get statistics for all the unique games played.
#'
#' Input a play df. Return a data frame of statistics characterizing the play
#' patterns of each individual game.
#' @param play_df a data frame of valid plays
#' @export
generate_game_stats <- function(play_df) {

    df <- play_df %>%
        group_by(game) %>%
        summarise(
            chair_hours = sum(length) / 60,
            play_count = length(unique(id)),
            player_count = length(unique(name)),
            locations = length(unique(location))
        ) %>%
        arrange(desc(chair_hours))

    play_time <- play_df %>%
        group_by(id, length, game) %>%
        summarise(player_count = length(unique(name))) %>%
        group_by(game) %>%
        summarise(clock_hours = sum(length) / 60)

    df <- left_join(df, play_time, by = "game")

    df

}


#' Get Z x Z Index
#'
#' Get a gaming index, such as the H-index.
#'
#' Input a play df and the index you desire.
#' H-index: N where you have played N distinct games at least N times each.
#' HH-index: N where you have played N distinct games for at least N hours each.
#' K-index: N where you have played with N distinct people for at least N games each.
#' KH-index: N where you have played with N distinct people for at least N hours each.
#' Returns the value of the given index.
#'
#' @param play_df a data frame of valid plays
#' @param play_index an index of any of the index values: "H", "HH", "K", or "KH", not case sensitive
#' @export
get_gaming_index <- function(play_df, play_index) {

    # check if the index is valid
    if ((!grepl("^[HhKk][Hh]$|^[HhKk]$", play_index)) | (length(play_index) > 1)) stop("ValueError: Invalid play_index value")

    play_index <- tolower(play_index)

    if (play_index == "h") {

        play_df %>%
            generate_game_stats() %>%
            arrange(desc(play_count)) %>%
            pull(play_count) %>%
            find_index() %>%
            return()

    } else if (play_index == "hh") {

        play_df %>%
            generate_game_stats() %>%
            arrange(desc(clock_hours)) %>%
            pull(clock_hours) %>%
            find_index() %>%
            return()

    } else if (play_index == "k") {

        play_df %>%
            generate_player_stats() %>%
            arrange(desc(play_count)) %>%
            pull(play_count) %>%
            find_index() %>%
            return()

    } else if (play_index == "kh") {

        play_df %>%
            generate_player_stats() %>%
            arrange(desc(chair_hours)) %>%
            pull(chair_hours) %>%
            find_index() %>%
            return()

    }
}
