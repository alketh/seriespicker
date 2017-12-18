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

# lets do some debugging
ratings <- vector(mode = "list", length = length(ids))
for (i in seq_along(ids)) {
  ratings[[i]] <- seriespicker::rating(id = ids[i], season = seasons[i])
  print(i)
}

ratings <- purrr::map2(.x = ids, .y = seasons, seriespicker::rating)


devtools::use_data(ids, top250, ratings, overwrite = TRUE)

rm(list = ls())

