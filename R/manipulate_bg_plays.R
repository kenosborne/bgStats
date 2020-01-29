#' Read a BGG play.xml file into R
#'
#' This function takes an xml website and username as inputs. It looks at
#' all of the logged games from BGG as listed by the xml website, and loads those
#' into R as a list. It outputs a list that has the same structure as the xml file.
#'
#' @param user The username of a Board Game Geek account.
#' @param base_url The url of Board Game Geek's xml api.
#' @return A list of gameplay statistics in XML style.
#' @export
read_bgg_stats_xml <- function(
    user,
    base_url = "https://boardgamegeek.com/xmlapi2/plays?username="
    ) {

    if (!grepl("^[A-Za-z_0-9]{4}[A-Za-z_0-9]*$", user)) stop("Invalid Username")

    paste0(base_url, user) %>%
        readr::read_lines() %>%
        XML::xmlParse() %>%
        XML::xmlRoot() %>%
        XML::getNodeSet("play")
}


#' Consider the XML Only from Valid Games
#'
#' This function takes in and returns a Board Game Geek xml list parameter.
#' The output file will only have valid plays.
#'
#' @param xml_play A BGG play.xml file that is loaded into R.
#' @return Two  possible outcomes. 1. A BGG play.xml file that is stripped of invalid plays. 2. An error that says "no valid plays logged"
#' @export
clean_playXML <- function(xml_play, player_data = "players", game_data = "item"){

    has_both <- xml_play %>%
        lapply(
            function(x) {
                nms <- names(x)
                names(nms) <- NULL
                is.element(player_data, nms) && is.element(game_data, nms)
            }
        )

    if (length(has_both) == 0) stop("No Valid Plays Logged")

    which (has_both == TRUE) %>%
        xml_play[.]
}


#' Turn a Board Game Geek file into an R Data Frame
#'
#' This function takes a Board Game Geek xml node. It outputs a
#' data frame where all plays are sorted by play_id (listed in the data fram as `id``) and date.
#'
#' @param xml_node An individual BGG play, usually one element of an XNL list.
#' @param player_data The name of the xml sub-node where player data is kept. "players" by default.
#' @param game_data The name of the xml sub-node where game data is kept. "item" by default.
#' @return A data frame of gameplay statistics.
play_node_to_df <- function(xml_node, player_data = "players", game_data = "item") {

    df <- XML::xmlSApply(xml_node[[player_data]], XML::xmlAttrs) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE) %>% #all cols are char cols
        tibble::remove_rownames() %>%
        mutate_at(vars(-username, -name), funs(as.numeric)) %>% #change most cols to numeric cols
        mutate(join_me = 0)

    game_name <- XML::xmlGetAttr(xml_node[[game_data]], "name")

    play_data <- XML::xmlAttrs(xml_node) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        mutate_at(vars(-location, -date), funs(as.numeric)) %>%
        mutate(
            join_me = 0,
            game = game_name,
            player_count = nrow(df),
            expected_wins = 1/nrow(df)
        )

    df <- left_join(df, play_data, by = "join_me") %>%
        select(-join_me)

    df

}


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


#' Turn a Board Game Geek file into an R Data Frame
#'
#' Takes a Board Game Geek xml list parameter. It outputs a data frame where all
#' plays are sorted by play_id (listed in the data fram as `id``) and date.
#'
#' @param xml_play An individual BGG play, usually one element of an XNL list.
#' @return A data frame of gameplay statistics.
#' @export
play_xml_to_df <- function(xml_play, player_data = "players", game_data = "item") {

    lapply(xml_play, play_node_to_df) %>%
        rbind_list() %>%
        add_winner_data()

}

