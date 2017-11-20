#' Extract number of seasons
#'
#' @param series_id Character string of the IMDB series id.
#' @return Integer giving the number of seasons per series.
#' @export
#' @examples
#' get_seasons(ids[1:10])

get_seasons <- function(series_id = seriespicker::ids) {
  urls <- paste0("http://www.imdb.com/title/tt", series_id)

  # Read in raw html
  html_raw <- purrr::map(urls, xml2::read_html)

  # Extract links within title-episode-widget.
  seasons <- purrr::map(html_raw, ~rvest::html_nodes(., "#title-episode-widget")) %>%
    purrr::map(., ~rvest::html_nodes(., "a")) %>%
    purrr::map(., ~rvest::html_attr(., "href"))

  # Count season links.
  for (i in seq_along(seasons)) {
    seasons[[i]] <- seasons[[i]][grepl(pattern = "season", seasons[[i]])]
  }
  purrr::map_int(seasons, length)
}

