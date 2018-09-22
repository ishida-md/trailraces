library(tabulizer)
library(tidyverse)

load("interim/utmf2018.RData")

mf <- extract_tables("input/utmf2018_women_names.pdf", encoding = "UTF-8")
matf <- mf %>% reduce(rbind)
tbf <- as.tibble(matf[-1, ]) %>% mutate(sex = "female")

mm <- extract_tables("input/utmf2018_men_names.pdf", encoding = "UTF-8")
matm <- mm %>% reduce(rbind)
tbm <- as.tibble(matm[-1, ]) %>% mutate(sex = "male")

utmf2018_names <- tbf %>% bind_rows(tbm) %>% 
  set_names(c("bib", "name", "name_en", "country", "sex")) %>% select("bib", "name")

utmf2018_all <- utmf2018_all %>% select(-name) %>% left_join(utmf2018_names) %>% select(rank, bib, name, everything())

save(utmf2018_all, file = "interim/utmf2018-fixed.RData")