library(tidyverse)
library(tabulizer)

sfmt2015_raw <- extract_tables("input/result2015m.pdf", encoding = 'UTF-8')
sfmt2015_tb <- map(sfmt2015_raw, as.tibble)
sfmt2015 <- bind_rows(sfmt2015_tb) %>% 
  set_names(c("rank2015", "bib", "name_kanji", "name_kana", "madarao", "kurohime", "otomi", "ohashi", "togakushi", "finish")) %>% 
  mutate_all(parse_guess) %>% 
  mutate_at(vars(madarao:finish), as.numeric)

sfmt2017_raw <- extract_tables("input/result2017_m160.pdf", encoding = 'UTF-8')
sfmt2017_tb <- map(sfmt2017_raw, as.tibble)
sfmt2017 <- bind_rows(sfmt2017_tb) %>% 
  set_names(c("rank2017", "bib", "name_kanji", "name_kana", "banff", "akaike", "apa", "seishonen","akakura", "kurohime")) %>% 
  mutate_all(parse_guess) %>% 
  mutate_at(vars(banff:kurohime), as.numeric)

save(sfmt2015, sfmt2017, file = "interim/sfmt.RData")
