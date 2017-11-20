#' Get episode specific series rating.

id <- "0944947"
season <- 3

ratings <- function(id, season) {
  url <- paste0("http://www.imdb.com/title/tt", id, "/episodes?season=", season, "&ref_=tt_eps_sn_", season)

  html_raw <- xml2::read_html(url)

  # Create links to episodes
  episode_urls <- rvest::html_nodes(html_raw, "div")
  episode_urls <- rvest::html_attr(episode_urls, "data-const")
  episode_urls <- episode_urls[!is.na(episode_urls)]

  # Read in raw rating html per episode.
  rating_urls <- paste0("http://www.imdb.com/title/", episode_urls, "/ratings?ref_=tt_ov_rt")
  ratings_raw <- purrr::map(rating_urls, xml2::read_html)

  # Extract ratings and votes per age and gender ratings
  rarings <- purrr::map(ratings_raw, ~rvest::html_table(.)[2][[1]])

  rating <- ratings[[1]]
  clean_ratings <- function(rating) {

  }

  # Extract episode names
  names <- purrr::map(ratings_raw, ~rvest::html_nodes(., ".subnav_heading")) %>%
    purrr::map_chr(., rvest::html_text)
}
