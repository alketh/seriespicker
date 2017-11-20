#' Extract Series meta information.
#'
#' @param series_id Character string of the IMDB series id.
#' @return Character giving the original series title.
#' @export
#' @examples
#' get_meta(ids[1:10])

get_meta <- function(series_id = seriespicker::ids) {
  urls <- paste0("http://www.imdb.com/title/tt", series_id)

  # Read in raw html
  html_raw <- purrr::map(urls, xml2::read_html)

  # Extract Original title
  titles <- purrr::map(html_raw, ~rvest::html_nodes(., ".originalTitle")) %>%
    purrr::map_chr(., rvest::html_text)

  # Extract number of seasons
  seasons <- purrr::map(html_raw, ~rvest::html_nodes(., "#title-episode-widget")) %>%
    purrr::map(., ~rvest::html_nodes(., "a")) %>%
    purrr::map(., ~rvest::html_attr(., "href"))

  # Count season links.
  for (i in seq_along(seasons)) {
    seasons[[i]] <- seasons[[i]][grepl(pattern = "season", seasons[[i]])]
  }
  purrr::map_int(seasons, length)

    purrr::map(., ~rvest::html_nodes(., "a")) %>%
    purrr::map(., ~rvest::html_attr(., "href"))

  # Count season links.
  for (i in seq_along(seasons)) {
    seasons[[i]] <- seasons[[i]][grepl(pattern = "season", seasons[[i]])]
  }
  purrr::map_int(seasons, length)

  # Add to output tibble

}
