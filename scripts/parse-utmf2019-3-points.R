library(rvest)
library(tidyverse)

load("interim/utmf2018.RData")

x <- read_html("https://utmf.livetrail.run/coureur.php?rech=49")
y <- html_nodes(x, "pt")
cum_dist <- html_attr(y, 'km')


utmf2019_course <- tibble(
  point = c(colnames(utmf2018_all)[8:13], colnames(utmf2018_all)[15:28]),
  cum_dist = as.numeric(cum_dist))

saveRDS(utmf2019_course, "interim/utmf2019_course.RDS")
