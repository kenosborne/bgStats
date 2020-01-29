#' Add Winner Data
#'
#' Takes a play df and adds a column of winner data.
#'
#' Takes a dataframe of gameplay stats as input. Finds all the winners
#' for that game's id, and makes a list of them. Appends that list back
#' to the original data frame.
#'
#' @param play_df a data frame of gameplay statistics
#' @return the same play_df with an appended winners column
#' @export
add_winner_data <- function(play_df) {

    wins_df <- play_df %>% filter(win == 1)

    games_with_wins <- wins_df %>%
        arrange(id) %>%
        pull(id) %>%
        unique()

    winners_df <- data.frame(
        id = games_with_wins,
        winners = rep(NA, length(games_with_wins))
    )

    winners <- wins_df %>%
        group_split(id) %>%
        lapply(pull, name)

    winners_df$winners <- winners

    play_df <- left_join(play_df, winners_df, by = "id")

    play_df

}
