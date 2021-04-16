library(tidyverse)
library(data.table)
library(Hmisc)
library(viridis)
library(patchwork)
library(scales)
library(broom)
library(widyr)

lu_files <- list.files(here::here("data-raw"), pattern = "landuse.csv", full.names = TRUE)
lu_munis <- list.files(here::here("data-raw"), pattern = "landuse.csv", full.names = FALSE) %>%
  str_sub(1, 3)

landuse <- map2(lu_files, lu_munis, function(x, y) {
    z <- fread(x)
    z[, city := y]
    
    return(z)
  }) %>% rbindlist()

access_files <- list.files(here::here("data/access"), pattern = ".csv", full.names = TRUE)

access_df <- map(access_files, fread) %>% rbindlist()
access_df[landuse, on = .(origin = id_hex), pop_total := i.pop_total]


# Summary -----------------------------------------------------------------


access_summary <- access_df %>%
  group_by(city, mode, metric, jobs, fun, threshold) %>%
  summarise(ac_mean = mean(accessibility),
            ac_median = median(accessibility),
            wt_mean = weighted.mean(accessibility, pop_total),
            wt_median = wtd.quantile(accessibility, weights = pop_total, probs = 0.5))



# Rank --------------------------------------------------------------------

access_rank <- access_summary %>%
  pivot_longer(ac_mean:wt_median, names_to = "stat", values_to = "value") %>%
  group_by(mode, metric, jobs, fun, threshold, stat) %>%
  arrange(desc(value)) %>%
  mutate(rank = row_number())

rank_df <- landuse[, .(population = sum(pop_total), jobs = sum(empregos_total)), by = city]
rank_df <- rank_df %>%
  arrange(desc(population)) %>%
  mutate(rank_pop = row_number()) %>%
  arrange(desc(jobs)) %>%
  mutate(rank_jobs = row_number()) %>% 
  arrange(desc(jobs/population)) %>%
  mutate(rank_ratio = row_number()) 

cities <- (rank_df %>% arrange(rank_pop))$city
colors <- inlmisc::GetColors(n = 20, reverse = TRUE)

p_rank <- rank_df %>% 
  mutate(city = factor(city, levels = cities)) %>%
  pivot_longer(starts_with("rank"), names_to = "criteria", values_to = "rank") %>%
  ggplot(aes(x=criteria, y=-rank, fill = city)) +
  geom_tile(alpha = 0.7) +
  geom_text(aes(label=city), fontface="bold", hjust=0.5, vjust=0.5) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), 
                     breaks = c(-1, -5, -10, -15, -20),
                     labels = c(1, 5, 10, 15, 20)) +
  facet_wrap(~criteria, scales = "free") +
  theme_light() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())

access_rank %>%
  mutate(city = factor(city, levels = cities)) %>%
  mutate(threshold = factor(threshold)) %>%
  filter(stat == "wt_median", threshold == 30) %>% 
  # filter(jobs=="rel") %>%
  ggplot(aes(x=jobs, y=-rank, fill = city)) +
  geom_tile(alpha = 0.7) +
  geom_text(aes(label=city), fontface="bold", hjust=0.5, vjust=0.5, 
            color="black") +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), 
                     breaks = c(-1, -5, -10, -15, -20),
                     labels = c(1, 5, 10, 15, 20)) +
  scale_fill_manual(values = colors) +
  facet_grid(stat~metric+fun, scales = "free", space = "free") +
  theme_light() +
  theme(legend.position = "none",
        axis.title = element_blank()) + p_rank + plot_layout(widths = c(0.8, 0.2))



# Correlations ---------------------------------------------------

access_rank %>%
  select(-rank) %>%
  pivot_wider(names_from = jobs, values_from = value) %>% 
  ungroup() %>% 
  select(-city) %>%
  group_by(mode, metric, fun, threshold, stat) %>%
  nest() %>%
  mutate(pearson = map(data, cor),
         spearman = map(data, cor, method = "spearman")) %>%
  mutate(pearson = map_dbl(pearson, min),
         spearman = map_dbl(spearman, min)) %>%
  View()

# Log Correlations ------------------------------------------------



access_rank %>%
  # filter(city=="poa") %>%
  select(-rank) %>%
  pivot_wider(names_from = jobs, values_from = value) %>% 
  ungroup() %>%
  ggplot(aes(x=abs, y=rel)) + 
  geom_point(aes(color=city)) +
  geom_smooth(method="lm") +
  scale_color_viridis_d(option = "cividis") +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = percent) +
  facet_wrap(~stat)

access_rank %>%
  ungroup() %>%
  # filter(city=="poa") %>%
  select(-value) %>%
  pivot_wider(names_from = jobs, values_from = rank) %>% 
  filter(metric=="bfca", threshold==15,fun=="gaussian") %>% 
  left_join(rank_df) %>%
  # mutate(abs = abs / (jobs),
  #        rel = rel / (jobs)) %>%
  ggplot(aes(x=-abs, y=-rel)) + 
  geom_point(aes(color=city)) +
  geom_smooth(method="lm") +
  coord_equal() +
  scale_color_viridis_d(option = "cividis") +
  # scale_x_log10(labels = comma) +
  # scale_y_log10(labels = percent) +
  facet_wrap(~stat)


# Correlation to indicators -----------------------------------------------
tidy_correlation <- function(data, method) {
  res <- cor(data, method = method)
  res[lower.tri(res, diag = TRUE)] <- NA
  res <- res %>%
    as_tibble(rownames = "var1") %>%
    pivot_longer(cols = -var1, names_to = "var2") %>% 
    filter(var1 %in% c("abs", "rel")) %>%
    drop_na() %>%
    mutate(method = method)
  
  return(res)
}

access_cor <- access_rank %>%
  ungroup() %>%
  select(-rank) %>%
  pivot_wider(names_from = jobs, values_from = value) %>% 
  left_join(rank_df) %>%
  mutate(ratio = jobs / population) %>%
  select(-starts_with("rank"), -city) %>%
  group_by(mode, metric, fun, threshold, stat) %>%
  nest() %>%
  mutate(pearson = map(data, tidy_correlation, method = "pearson"),
         spearman = map(data, tidy_correlation, method = "spearman"),
         correlations = map2(pearson, spearman, rbind)) %>%
  unnest(correlations) %>%
  select(-data, -pearson, -spearman) %>%
  pivot_wider(names_from = method, values_from = value)

t <- 30
access_cor %>%
  # filter(var1 == "abs", var2 == "ratio") %>%
  filter(threshold==t) %>%
  filter(stat == "ac_median") %>%
  mutate(method = paste(metric, fun)) %>%
  # mutate(method = factor(method, levels = c("cumulative binary", "gravity gaussian",
  #                                           "2sfca binary", "2sfca gaussian",
  #                                           "bfca binary", "bfca gaussian",
  #                                           "pfca binary", "pfca gaussian"))) %>%
  ggplot(aes(x=method, y=spearman, fill=stat)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  expand_limits(y=c(-1, 1)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~var1+var2, ncol=4) +
  theme(legend.position = "bottom",
        axis.title = element_blank()) +
  labs(title="Spearman's rank correlations between accessibility metrics and urban indicators",
       subtitle = sprintf("by walk, time threshold = %s", t))

ggsave(filename = here::here("plots", sprintf("access_cor_t%s.png", t)),
       width=297, height=210, units="mm", dpi=300)

access_cor %>%
  # filter(var1 == "abs", var2 == "ratio") %>%
  filter(threshold==t) %>%
  filter(stat == "ac_median") %>%
  mutate(method = paste(metric, fun)) %>% 
  mutate(method = factor(method, levels = c("cumulative binary", "gravity gaussian",
                                            "2sfca binary", "2sfca gaussian",
                                            "bfca binary", "bfca gaussian",
                                            "pfca binary", "pfca gaussian"))) %>%
  ggplot(aes(x=metric, y=spearman, fill=fun)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  expand_limits(y=c(-1, 1)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~var1+var2, ncol=4) +
  theme(legend.position = "bottom",
        axis.title = element_blank()) +
  labs(title="Spearman's rank correlations between accessibility metrics and urban indicators",
       subtitle = sprintf("by walk, time threshold = %s", t))

access_cor %>% 
  ggplot(aes(pearson, spearman)) +
  geom_point() +
  facet_grid(var1~var2)

