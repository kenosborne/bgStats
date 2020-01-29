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
        summarise(hours_played = sum(length) / 60)

    df <- left_join(df, play_time, by = "game")

    df

}
