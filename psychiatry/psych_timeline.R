library(tidyverse)
source("./programs/fun_save_png.R")
source("./programs/fun_plot.R")

plot_h_errorbar <- function(df, xvar, ymin = conc_l, ymax = conc_u, color = NULL, label_suffix = NULL){
  dplyr_xvar <- enquo(xvar)
  dplyr_ymin <- enquo(ymin)
  dplyr_ymax <- enquo(ymax)
  dplyr_color <- enquo(color)
  ggplot(data = df, aes(x = !!dplyr_xvar, color = !!dplyr_color)) +
    geom_errorbar(aes(ymin = !!dplyr_ymin, ymax = !!dplyr_ymax, width = 0.25), size = 0.7, position = position_dodge(width=0.9)) +
    geom_text(aes(y = !!dplyr_ymin, label = paste0(!!dplyr_ymin, label_suffix)), hjust = 0.5, position = position_dodge(width=0.5)) +
    geom_text(aes(y = !!dplyr_ymax, label = paste0(!!dplyr_ymax, label_suffix)), hjust = -0.3, position = position_dodge(width=0.9)) +
    theme_cowplot() +
    theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8))
}

df_psych_raw <- read.csv("./psychiatry/psych_timeline.csv")

df_psych_time <- df_psych_raw %>% 
  dplyr::mutate(
    disorder_fct = factor(dis_cat, levels = unique(.$dis_cat), labels = unique(.$disorder)),
    diagnosis = fct_rev(factor(.$diagnosis, levels = unique(.$diagnosis)))
    ) 

  
p_psych_time <- df_psych_time %>% 
  plot_h_errorbar(xvar = diagnosis, ymin = t_beg, ymax = t_end, color = disorder_fct, label_suffix = "     ") +
  facet_wrap(~disorder_fct, ncol = 1, scales = "free") +
  ylab("Time (Days)") +
  scale_y_continuous(limits = c(-5, 730), breaks = seq(0,720,90)) +
  xlab("") +
  ggtitle("Psychiatric Diagnosis Timeline (in Days)") +
  theme(legend.position = "none") +
  coord_flip()
save_png("psychiatry/psych_timeline.png", p_psych_time, dim_w = 10, dim_h = 40)

