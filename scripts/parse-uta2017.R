library(tabulizer)
library(tidyverse)
library(lubridate)

cols <- c("rank", "bib", "name", "club", "cat", "rank_cat", "time", "diff_time", "country")

uta2017_raw <- tabulizer::extract_tables("input/uta_2017_classement_UTA100_scratch.pdf") 
uta2017 <- uta2017_raw[1:29] %>% map(function(x) x[-1, ]) %>% 
  map(as.tibble) %>% bind_rows() %>% set_names(cols)

tail <- uta2017_raw[[30]] %>% .[-(1:2), ] %>% as.tibble() %>% select(-4) %>% set_names(cols)

uta2017 <- uta2017 %>% bind_rows(tail) %>% filter(name != "") %>% mutate(rank = as.numeric(rank), time = as.numeric(hms(time), units = "sec"))

save(uta2017, file = "interim/uta2017.RData")