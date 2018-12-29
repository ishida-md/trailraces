library(tabulizer)

uta100_raw <- tabulizer::extract_tables("input/uta_2018_classement_UTA100_scratch.pdf") 

save(uta100_raw, file = "interim/uta100.RData")
