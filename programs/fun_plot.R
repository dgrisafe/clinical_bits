library(cowplot)

plot_errorbar <- function(df, xvar, ymin = conc_l, ymax = conc_u, color = NULL, label_suffix = NULL){
  dplyr_xvar <- enquo(xvar)
  dplyr_ymin <- enquo(ymin)
  dplyr_ymax <- enquo(ymax)
  dplyr_color <- enquo(color)
  ggplot(data = df, aes(x = !!dplyr_xvar, color = !!dplyr_color)) +
    geom_errorbar(aes(ymin = !!dplyr_ymin, ymax = !!dplyr_ymax, width = 0.25), size = 0.8, position = position_dodge(width=0.9)) +
    geom_text(aes(y = !!dplyr_ymin, label = paste0(!!dplyr_ymin, label_suffix)), vjust = 1.4, position = position_dodge(width=0.9)) +
    geom_text(aes(y = !!dplyr_ymax, label = paste0(!!dplyr_ymax, label_suffix)), vjust = -0.5, position = position_dodge(width=0.9)) +
    theme_cowplot() +
    theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8))
}

# plot_errorbar(color = color_sex)