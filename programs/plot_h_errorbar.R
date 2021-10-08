library(cowplot)

plot_h_errorbar <- function(df, xvar, ymin, ymin_txt, ymax, ymax_txt, color = NULL, label_suffix = NULL){
  dplyr_xvar <- enquo(xvar)
  dplyr_ymin <- enquo(ymin)
  dplyr_ymin_txt <- enquo(ymin_txt)
  dplyr_ymax <- enquo(ymax)
  dplyr_ymax_txt <- enquo(ymax_txt)
  dplyr_color <- enquo(color)
  ggplot(data = df, aes(x = !!dplyr_xvar, color = !!dplyr_color)) +
    geom_errorbar(aes(ymin = !!dplyr_ymin, ymax = !!dplyr_ymax, width = 0.25), size = 0.7, position = position_dodge(width=0.9)) +
    geom_text(aes(y = !!dplyr_ymin, label = paste0(!!dplyr_ymin_txt, label_suffix)), hjust = 0.7, position = position_dodge(width=0.5), size = 3) +
    geom_text(aes(y = !!dplyr_ymax, label = paste0(!!dplyr_ymax_txt, label_suffix)), hjust = -0.1, position = position_dodge(width=0.9), size = 3) +
    theme_cowplot() +
    theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8))
}
