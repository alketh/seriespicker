#' Extract Series meta information.
#'
#' @param series_id Character string of the IMDB series id.
#' @return Character giving the original series title.
#' @export
#' @examples
#' series_id <- seriespicker::ids[c(1, 4, 6)]
#' get_meta(series_id)
#'
#' get_meta(ids[1:10])
#' \dontrun{
#'   get_meta()
#' }

get_meta <- function(series_id = seriespicker::ids) {
  urls <- paste0("http://www.imdb.com/title/tt", series_id)

  # Read in raw html
  html_raw <- purrr::map(urls, xml2::read_html)

  # Extract english titles
  titles <- vector(mode = "character",length = length(series_id))
  for (i in seq_along(titles)) {
    ot <- rvest::html_nodes(html_raw[[i]], ".originalTitle") # use original title if available
    if (length(ot) == 0) ot <- rvest::html_nodes(html_raw[[i]], "title") # use english title otherwise
    titles[i] <- rvest::html_text(ot)
  }

  # Cleanup titles
  par_pos <- stringr::str_locate(titles, pattern = "\\(")
  titles <- stringr::str_sub(string = titles, end = par_pos[, 1] - 2)

  # Extract number of seasons
  seasons <- purrr::map(html_raw, ~rvest::html_nodes(., "#title-episode-widget"))
  seasons <- purrr::map(seasons, ~rvest::html_nodes(., "a"))
  seasons <- purrr::map(seasons, ~rvest::html_attr(., "href"))

  # Count season links.
  for (i in seq_along(seasons)) {
    seasons[[i]] <- seasons[[i]][grepl(pattern = "season", seasons[[i]])]
  }

  # Add to output tibble
  out <- tibble::tibble(ids = series_id, title = titles, nos = purrr::map_int(seasons, length))
  return(out)
}

