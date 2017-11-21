#' Get episode specific series rating.
#'
#' @param id Character IMDB series id string. E.g. \code{0944947} for "Game of Thrones".
#' @param seasn Integer Season number.
#' @return tidy dataframe with columns "gender", "age", "rating", "votes", "id", "name"
#' @export
#'
#' @examples
#' id <- "0944947"
#' season <- 3
#' rating(id, season)
#'
#' df <- rating("0944947", 5)
#' df <- rating("0052520", 3)

rating <- function(id, season) {
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
  ratings <- purrr::map(ratings_raw, ~rvest::html_table(.)[2][[1]])

  # rating <- ratings[[1]]
  clean_ratings <- function(rating) {
    df <- tidyr::gather_(rating, key_col = "age", value_col = "rating", gather_cols = names(rating)[2:ncol(rating)])
    names(df)[1] <- "gender"
    out <- tidyr::separate(df, col = "rating", into = c("rating", "x", "y", "votes"), sep = "\n")
    out <- dplyr::select_(out, .dots = c("gender", "age", "rating", "votes"))
    return(out)
  }

  # Extract episode names
  names <- purrr::map(ratings_raw, ~rvest::html_nodes(., ".subnav_heading"))
  names <- purrr::map_chr(names, rvest::html_text)

  # Add episode name to rating table!
  ratings_clean <- purrr::map(ratings, clean_ratings)

  for (i in seq_along(ratings_clean)) {
    ratings_clean[[i]]$id <- i
    ratings_clean[[i]]$name <- names[i]
  }

  # Combine to single dataframe
  out <- dplyr::bind_rows(ratings_clean)
  out$rating <- as.numeric(out$rating)
  out$votes <- as.numeric(stringr::str_replace(trimws(out$votes), ",", ""))

  return(tibble::as.tibble(out))
}
