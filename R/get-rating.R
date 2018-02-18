#' Get episode specific series rating.
#'
#' @param id Character IMDB series id string. E.g. \code{0944947} for "Game of Thrones".
#' @param season Integer Season number.
#' @return tidy dataframe with columns "gender", "age", "rating", "votes", "id", "name"
#' @export
#'
#' @examples
#' id <- "0944947"
#' season <- 3
#' season <- 1
#'
#' id <- "5491994"
#' season <- 1
#'
#' id <- "1641384"
#' season <- 3
#'
#' rating(id, season)
#'
#' df <- rating("0944947", 5)
#' df <- rating("0052520", 3)

# Personal NOTE: >90% of computation time is spent with xml2::read_html.

get_rating <- function(id, season) {
  url <- paste0("http://www.imdb.com/title/tt", id, "/episodes?season=", season, "&ref_=tt_eps_sn_", season)

  html_raw <- xml2::read_html(url)

  # Create links to episodes
  episode_urls <- rvest::html_nodes(html_raw, "div")
  episode_urls <- rvest::html_attr(episode_urls, "data-const")
  episode_urls <- episode_urls[!is.na(episode_urls)]

  # Read in raw rating html per episode.
  rating_urls <- paste0("http://www.imdb.com/title/", episode_urls, "/ratings?ref_=tt_ov_rt")

  # catch issues with internet connection:
  # E.g.  Error in open.connection(x, "rb") : Recv failure: Connection was reset
  ratings_raw <- try(purrr::map(rating_urls, xml2::read_html))
  out_col <- 9
  out_names <- c("id", "season", "ep_nr", "ep_name", "gender", "age", "rating", "votes", "error")

  out <- data.frame(t(rep(NA, out_col)))
  names(out) <- out_names
  out$id <- id
  out$season <- season

  if (class(ratings_raw) == "try-error") {
    out$error <- "error in xml2::read_html call"
  } else {
    # Extract ratings and votes per age and gender ratings
    ratings <- purrr::map(ratings_raw, ~rvest::html_table(.)[2][[1]])

    # In case some episodes are not released yet define NAs to the whole season!
    if (any(sapply(ratings, is.null))) {
      out$error <- NA
    } else {
      # rating <- ratings[[1]]
      clean_ratings <- function(rating) {
        df <- tidyr::gather_(rating, key_col = "age", value_col = "rating", gather_cols = names(rating)[2:ncol(rating)])
        names(df)[1] <- "gender"

        # Some episodes have no rating
        df[df$rating == "-", 3] <- NA
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
        ratings_clean[[i]]$ep_nr <- i
        ratings_clean[[i]]$ep_name <- names[i]
      }

      # Combine to single dataframe
      out <- dplyr::bind_rows(ratings_clean)
      out$rating <- as.numeric(out$rating)
      out$votes <- as.numeric(stringr::str_replace(trimws(out$votes), ",", ""))
      out$id <- id
      out$season <- season
      out$error <- NA

      # Redorder solumns
      out <- dplyr::select_(out, .dots = out_names)
    }
  }

  return(tibble::as.tibble(out))
}
