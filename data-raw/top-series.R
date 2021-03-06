# Extract IMDB ids of top 250 series
top_series <- xml2::read_html("http://www.imdb.com/chart/toptv/")
top_series <- rvest::html_nodes(top_series, "a")
top_series <- rvest::html_attr(top_series, "href")
top_series <- top_series[grepl(pattern = "/title/", top_series)]

# For some reason Dr Strangelove is part of the series lists.
top_series <- top_series[grepl(pattern = "_tt_", top_series)]

top_series <- unique(top_series)

ids <- substr(top_series, start = 10, stop = 16)

# Get meta information for top 250 series
top250 <- get_meta(series_id = ids)

# Extract rating from top250 Series on IMDB and store as tidy dataframe.
ids <- purrr::flatten_chr(purrr::map2(top250$ids, top250$nos, ~rep(x = .x, times = .y)))
seasons <- unlist(purrr::map(top250$nos, ~seq(1, .)))

# lets add a progress bar here!
ratings <- vector(mode = "list", length = length(ids))
pb <- dplyr::progress_estimated(length(ratings))
for (i in seq_along(ratings)) {
  ratings[[i]] <- seriespicker::rating(id = ids[i], season = seasons[i])
  pb$tick()$print()
}
pb$stop()

# Let's combine everything to one dataframe and have a look at it.
rating <- dplyr::bind_rows(ratings)
rating <- dplyr::left_join(rating, top250)

# There was an issue with the internet connection with 1 entry:
which(grepl(pattern = "error", x = rating$error))

devtools::use_data(ids, top250, rating, overwrite = TRUE)

rm(list = ls())

