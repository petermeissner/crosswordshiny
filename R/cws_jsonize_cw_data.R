#' Helper: transform cw data to JSON for Crossword.js
#'
#' @param cw A crossword object (see \code{crossword::Crossword$new()})
#'
#'
#' @return
#'
#' JSON string of clue, answer, positions ready to use with Crossword.js
#'
#' @export

cws_jsonize_cw_data <- function(cw){
  # extract data from cw
  cw_data <- cw$words

  # transform data
  cw_data %>%
    dplyr::rename(
      answer      = word,
      orientation = direction,
      startx      = row,
      starty      = col
    ) %>%
    dplyr::select(
      clue,
      answer,
      orientation,
      startx,
      starty
    ) %>%
    dplyr::mutate(
      orientation =
        ifelse(
          test = orientation == "right",
          yes  = "across",
          no   = "down"
        )
    )

  # return as JSON-string
  jsonlite::toJSON(cw_data, pretty = TRUE)
}
