pretty_time <- function(seconds){
  seconds <- as.numeric(seconds)
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  paste0(h, ":", ifelse(m < 10, 0, ""), m)
}

pretty_seq <- function(nv, int = 3600){
  nv <- as.numeric(nv)
  rg <- range(nv, na.rm = TRUE) %/% 3600
  seq((rg[1]-1) * 3600, (rg[2] + 1) * 3600, by = int)
}