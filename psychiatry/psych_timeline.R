library(tidyverse)
source("./programs/fun_save_png.R")
source("./programs/plot_h_errorbar.R")
library(gsheet) # https://cran.r-project.org/web/packages/gsheet/gsheet.pdf

# load url of google sheet, add encoding
url <- "https://docs.google.com/spreadsheets/d/1lTrc-vq_fMnXjJaGGOL1pwtFUlwj4M0NWOVYM102MSU/gid="
construct_download_url(url, format = "csv")

# add suffix for each sheet, name each dataframe
url_sheet <- lapply(FUN = function(x) {paste0(url, x)}, X = c("0", "1919441802", "657875738", "1248990854"))
names(url_sheet) <- c("time", "age", "substance", "forensic")

# load data from Google sheet
df_data <- lapply(X = url_sheet, FUN = function(x) {read.csv(text = gsheet2text(x, format = 'csv'), stringsAsFactors = FALSE)})

# clean dataset of time
df_psych_time <- df_data$time %>% 
  dplyr::mutate(
    disorder_fct = factor(dis_cat, levels = unique(.$dis_cat), labels = unique(.$disorder)),
    diagnosis = fct_rev(factor(.$diagnosis, levels = unique(.$diagnosis)))
    ) 

# function for formatted plot
plot_h_errorbar_pretty <- function(df){
  df %>% 
    plot_h_errorbar(xvar = diagnosis, ymin = t_beg, ymin_txt = t_beg_txt, ymax = t_end, ymax_txt = t_end_txt, color = disorder_fct, label_suffix = "     ") +
    ylab("Time (Days)") +
    scale_y_continuous(limits = c(-15, 730), breaks = seq(0, 720, 90)) +
    xlab("") +
    theme(legend.position = "none") +
    coord_flip() 
}

# ggplot color hues
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# plot of all diagnoses
p_psych_time <- df_psych_time %>% 
  plot_h_errorbar_pretty() +
  facet_wrap(~disorder_fct, ncol = 1, scales = "free") +
  ggtitle("Psychiatric Diagnostic Timeline (in Days)")
save_png("./psychiatry/psych_timeline.png", p_psych_time, dim_w = 10, dim_h = 40)

# function creating plots of each class of diagnostics
map2(
  .x = unique(df_psych_time$dis_cat), 
  .y = unique(df_psych_time$disorder),
  .f = function(x, y){
    p_psych_time_unique <- df_psych_time %>% 
      dplyr::filter(dis_cat == x) %>% 
      plot_h_errorbar_pretty() +
      facet_wrap(~disorder_fct, ncol = 1, scales = "free") +
      scale_color_manual(values = gg_color_hue(max(df_psych_time$dis_cat))[[x]]) +
      ggtitle(paste0(y, " Diagnostic Timeline (Days)"))
    save_png(paste0("./psychiatry/psych_timeline_", x, "_", str_remove(y, pattern = "/"), ".png"), p_psych_time_unique, dim_w = 10, dim_h = 6)
  }
)
