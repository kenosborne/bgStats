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
read_xml_website <- function(
    user,
    base_url = "https://boardgamegeek.com/xmlapi2/plays?username="
    ) {

    paste0(base_url, user) %>%
        readr::read_lines() %>%
        XML::xmlParse() %>%
        XML::xmlRoot() %>%
        XML::getNodeSet("play")
}






#' Turn a Board Game Geek file into an R Data Frame
#'
#' This function takes a Board Game Geek xml list parameter It outputs a
#' data frame where all plays are sorted by play_id (listed in the data fram as `id``) and date.
#'
#' @param xml_play The BGG play.xml file that is loaded into R.
#' @param player_data The name of the xml sub-node where player data is kept. "players" by default.
#' @param game_data The name of the xml sub-node where game data is kept. "item" by default.
#' @return A data frame of gameplay statistics.
#' @export
playXML_to_df <- function(xml_play, player_data = "players", game_data = "item") {

    df <- XML::xmlSApply(xml_play[[player_data]], XML::xmlAttrs) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE) %>% #all cols are char cols
        tibble::remove_rownames() %>%
        mutate_at(vars(-username, -name), funs(as.numeric)) %>% #change most cols to numeric cols
        mutate(join_me = 0)

    game_name <- XML::xmlGetAttr(play[[game_data]], "name")

    play_data <- XML::xmlAttrs(play) %>%
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

