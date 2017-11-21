# Extract IMDB ids of top 250 series
top_series <- xml2::read_html("http://www.imdb.com/chart/toptv/")
top_series <- rvest::html_nodes(top_series, "a")
top_series <- rvest::html_attr(top_series, "href")
top_series <- top_series[grepl(pattern = "/title/", top_series)]
top_series <- unique(top_series)

ids <- substr(top_series, start = 10, stop = 16)

# Get meta information for top 250 series
top250 <- get_meta(series_id = ids)


devtools::use_data(ids, top250, overwrite = TRUE)

rm(list = ls())

