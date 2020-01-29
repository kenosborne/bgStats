#' Find Index for Gaming Statistics
#'
#' A helper function to count for N x N gaming indeces.
#'
#' Takes in a numeric vector for which we're looking to find a N x N index.
#' Returns the highest N where N >= N items deep in a descending sorted list.
#'
#' @param num_vect A numeric vector
find_index <- function(num_vect) {

    # count items that have value of counter or higher
    counter <- 1
    while (num_vect[counter] >= counter) {
        counter <- counter + 1
    }

    counter - 1
}
