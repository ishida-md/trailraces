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
  mutate(color = recode(sex, M = "lightblue", F = "pink")) %>% 
  arrange(sex, Finish, `A9 IN`, `A8 OUT`, `A7 IN`, `A6 IN`, `A5 IN`)

#-------------------------------------------------
#heatmap showing ranks over the points

finisher <- utmf2019_rank %>% filter(!is.na(`Finish`)) %>% 
  left_join(utmf2019_milers)

finisher_mt <- as.matrix((finisher %>% dplyr::select(W:Finish)))
finisher_long <- finisher %>% gather(W:Finish, key = "point", value = "rank_at") %>% 
  left_join(utmf2019_course, by = "point")

max_bound <- 200
medians <- apply(finisher_mt, 1, median, na.rm = TRUE)

clean_mt <- function(mt, max, filler){
  for (i in 1:nrow(mt)){
    idx <- is.na(mt[i, ])
    mt[i, idx] <- filler[i]
    idx_max <- mt[i, ] > max
    mt[i, idx_max] <- max
  }
  mt
}

finisher_mt <- clean_mt(finisher_mt, max_bound, medians)

milers_idx <- which(!is.na(finisher$name_pretty))


plot_heatmap <- function(mt, ...){
  heatmap.2(mt, Colv = FALSE, 
            Rowv = FALSE, 
            dendrogram = "none",
            trace = 'none', 
            col=bluered(75),
            cexRow = 1,
            sepwidth = c(0.01, 0.01),
            sepcolor = 'black', 
            density.info = "none",
            keysize = 1.5,
            key.title = "Rank",
            ...
  ) 
}

plot_heatmap(finisher_mt, labRow = finisher$name_pretty,
             rowsep = c(sum(finisher$sex == "F"), # number separating the sex
            milers_idx, milers_idx - 1), 
            RowSideColors = finisher$color)

plot_heatmap(finisher_mt, rowsep = sum(finisher$sex == "F"), labRow = FALSE, 
             RowSideColors = finisher$color)

ggplot(finisher_long, aes(cum_dist, rank_at, group = bib, color = sex)) +
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5) + 
  facet_wrap(~sex, scales = "free_y") +
  scale_x_continuous(breaks = utmf2019_course_unique$cum_dist, 
                     labels = utmf2019_course_unique$point) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1))

#----------------------------------------
# Plots the ranks of the runners who reached a point 
# within a specified rank


plot_top_n <- function(point, n = 100){
  point <- enquo(point)
  reduced <- utmf2019_rank %>% filter(!!point <= n) %>% 
    mutate(finished = !is.na(Finish)) %>% 
    arrange(sex, Finish, `A9 IN`, `A8 OUT`, `A7 IN`, `A6 IN`, `A5 IN`, `A4 IN`)
  
  reduced_long <- reduced %>%
    gather(W:Finish, key = point, value = rank_at) %>% 
    left_join(utmf2019_course, by = "point")
  
  reduced_mt <- as.matrix((reduced %>% dplyr::select(W:Finish)))
  reduced_mt <- clean_mt(reduced_mt, 200, filler = 100)
  heat <- plot_heatmap(reduced_mt, RowSideColors = reduced$color, labRow = FALSE)
  
  plt <- ggplot(reduced_long, aes(cum_dist, rank_at, color = finished, group = bib)) + 
    geom_point() + geom_line() + facet_wrap(~sex) + 
    scale_y_reverse(limits = c(400, 0)) +
    scale_x_continuous(breaks = utmf2019_course_unique$cum_dist, 
                       labels = utmf2019_course_unique$point) +
    theme(axis.text.x = element_text(angle = 45))
  return(list(plot = plt, df = reduced, mt = reduced_mt, heatmap = heat))
}

# Runners who passed A1 < 100

A1 <- plot_top_n(`A1 IN`, 100)
A2 <- plot_top_n(`A2 IN`, 100)
A3 <- plot_top_n(`A3 IN`, 100)
A7 <- plot_top_n(`A7 IN`, 100)
