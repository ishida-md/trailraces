library(tidyverse)
library(lubridate)
library(tabulizer)

men_mt <- extract_tables("input/result2018_UTMF_Men.pdf", encoding = "UTF-8")
men_tb <- men_mt %>% map(as.tibble) %>% reduce(full_join) %>% mutate(sex = "Male")

women_mt <- extract_tables("input/result2018_UTMF_Women.pdf", encoding = "UTF-8")
women_tb <- women_mt %>% map(as.tibble) %>% reduce(full_join) %>% mutate(sex = "Female")

header <- men_mt[[1]][1, ]
header_pretty <- c("rank", "bib", "name", "name_en", "country", "age_categ", "age_categ_rank", header[8:28])

utmf2018_all <- men_tb %>% bind_rows(women_tb) %>% set_names(c(header_pretty, "sex")) %>% filter(rank != "Ranking") %>% mutate_at(vars(Start:Finish), hms) %>% mutate_at(vars(Start:Finish), as.numeric)

table(utmf2018_all$country) %>% sort(decreasing = TRUE)
save(utmf2018_all, file = "interim/utmf2018.RData")
