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
            chair_hours = sum(length),
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
