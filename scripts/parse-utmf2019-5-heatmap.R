library(tidyverse)
library(pheatmap)
library(gplots)

utmf2019_long <- readRDS("interim/utmf2019_long.RDS")
utmf2019_course <- readRDS("interim/utmf2019_course.RDS")
utmf2019_milers <- read_csv("input/utmf2019-milers.csv")

utmf2019_course_unique <- utmf2019_course %>% group_by(cum_dist) %>% 
  filter(row_number(point) == 1)

utmf2019_rank <- utmf2019_long %>% 
  group_by(sex, point) %>% 
  arrange(point, cum_time) %>% 
  mutate(rank_at = ifelse(!is.na(cum_time), row_number(), NA)) %>% 
  ungroup() %>% 
  dplyr::select(bib, sex, point, rank_at) %>% 
  spread(key = point, value = rank_at) %>% 
  dplyr::select(bib, sex, W, `A1 IN`, `A2 IN`, `A3 IN`, `A4 IN`, `A5 IN`, `A6 IN`, `A7 IN`,
         `A8 OUT`, `A9 IN`, Finish) %>% 
  arrange(sex, Finish, `A9 IN`, `A8 OUT`, `A7 IN`, `A6 IN`, `A5 IN`)

#-------------------------------------------------
#heatmap showing ranks over the points

finisher <- utmf2019_rank %>% filter(!is.na(`Finish`)) %>% 
  mutate(color = recode(sex, M = "blue", F = "red")) %>% 
  left_join(utmf2019_milers)

finisher_mt <- as.matrix((finisher %>% dplyr::select(W:Finish)))
finisher_long <- finisher %>% gather(W:Finish, key = "point", value = "rank_at") %>% 
  left_join(utmf2019_course, by = "point")

max_bound <- 200
medians <- apply(finisher_mt, 1, median, na.rm = TRUE)

for (i in 1:nrow(finisher_mt)){
  idx <- is.na(finisher_mt[i, ])
  finisher_mt[i, idx] <- medians[i]
  
  idx_max <- finisher_mt[i, ] > max_bound
  finisher_mt[i, idx_max] <- max_bound
}

milers_idx <- which(!is.na(finisher$name_pretty))

heatmap.2(finisher_mt, Colv = FALSE, 
          Rowv = FALSE, 
          dendrogram = "none",
          trace = 'none', 
          labRow = finisher$name_pretty, 
          col=bluered(75),
          cexRow = 1,
          rowsep = c(sum(finisher$sex == "F"), # number separating the sex
                     milers_idx, milers_idx - 1),
          sepwidth = c(0.1, 0.1),
          sepcolor = 'black', 
          RowSideColors = finisher$color,
          density.info = "none",
          keysize = 1.5,
          key.title = "Rank"
          )


ggplot(finisher_long, aes(cum_dist, rank_at, group = bib, color = rank_at)) +
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5) + 
  facet_wrap(~sex, scales = "free_y")

#----------------------------------------
# Plots the ranks of the runners who reached a point 
# within a specified rank


plot_top_n <- function(point, n = 100){
  point <- enquo(point)
  reduced <- utmf2019_rank %>% filter(!!point <= n) %>% 
    mutate(finished = !is.na(Finish)) %>% 
    gather(W:Finish, key = point, value = rank_at) %>% 
    left_join(utmf2019_course, by = "point")
  plt <- ggplot(reduced, aes(cum_dist, rank_at, color = finished, group = bib)) + 
    geom_point() + geom_line() + facet_wrap(~sex) + 
    scale_y_reverse(limits = c(400, 0)) +
    scale_x_continuous(breaks = utmf2019_course_unique$cum_dist, 
                       labels = utmf2019_course_unique$point) +
    theme(axis.text.x = element_text(angle = 45))
  return(list(plot = plt, df = reduced))
}

