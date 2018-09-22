library(tidyverse)
library(tabulizer)

points <- c("banff", "kanemata", "apa", "shizen", "akakura",
           "kurohime", "sasagamine", "ohashi", "togakushi", "izuna",
           "finish")
header <- c("rank", "bib", "name", "name_kana", points)


sfmt2018m <- extract_tables("sfmt2018/result100mile_m.pdf", encoding = 'UTF-8') %>% 
  lapply(as.tibble) %>% reduce(bind_rows) %>% 
  set_names(header) %>% mutate(sex = "m")

sfmt2018f <- extract_tables("sfmt2018/result100mile_w.pdf", encoding = 'UTF-8') %>% 
  lapply(as.tibble) %>% reduce(bind_rows) %>% 
  set_names(header) %>% mutate(sex = "f")

sfmt2018 <- sfmt2018m %>% bind_rows(sfmt2018f) %>% 
  mutate_at(vars(-(rank:name_kana), -sex), lubridate::hms)

save(sfmt2018, file = "interim/sfmt2018.RData")
