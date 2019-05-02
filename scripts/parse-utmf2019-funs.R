library(rvest)
library(tidyverse)
library(progress)

utmf2019_entrant <- readRDS("interim/utmf2019_entrant.RDS")
n_points <- 17

extract_time <- function(html){
  list_of_strings <- html_nodes(html, "e")
  points <- html_attr(list_of_strings, "idpt")
  good_idx <- !is.na(points)
  points <- points[good_idx]
  times <- html_attr(list_of_strings[good_idx], 'tps') %>% 
    lubridate::hms() %>% 
    as.numeric(units = "secs")
  data.frame(point = points, time = times, stringsAsFactors = FALSE)
}

extract_point <- function(html){
  list_of_text <- html_nodes(html, "pt")
}

query_livetrail <- function(bib){
  url = paste0("https://utmf.livetrail.run/coureur.php?rech=", bib)
  html <- read_html(url, encoding = "UTF-8")
  cum_time <- extract_time(html) %>% mutate(bib = bib) %>% spread(point, time)
}

query_all <- function(bibs){
  pb <- progress_bar$new(total = length(bibs))
  result <- list()
  for (i in seq_along(bibs)){
    result[[i]] <- query_livetrail(bibs[i])
    pb$tick()
  }
  bind_rows(result)
}
