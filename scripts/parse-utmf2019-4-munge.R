library(tidyverse)

utmf2019_raw <- readRDS("interim/utmf2019.RDS")
utmf2019_entrant <- readRDS("interim/utmf2019_entrant.RDS")
utmf2019_course <- readRDS("interim/utmf2019_course.RDS")
points_en <- readLines("interim/utmf2019-points.txt")

load("interim/utmf2018-fixed2.RData")

utmf2019 <- utmf2019_raw %>% dplyr::select(bib, `0`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, everything()) %>% 
  set_names(c("bib", "Start", "W", colnames(utmf2018_all)[9:26])) %>% 
  left_join(utmf2019_entrant, by = "bib") %>% 
  dplyr::select(bib, name = name_en, name_ja, nationality, sex, everything())

utmf2019_long <- utmf2019 %>% gather(key = "point", value = "cum_time", Start:Finish) %>% 
  left_join(utmf2019_course, by = "point")

saveRDS(utmf2019, "interim/utmf2019_formatted.RDS")
saveRDS(utmf2019_long, "interim/utmf2019_long.RDS")
