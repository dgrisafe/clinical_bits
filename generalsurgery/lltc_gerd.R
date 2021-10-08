library(tidyverse)
library(cowplot)
source("./programs/fun_save_png.R")
source("./programs/plot_h_errorbar.R")

# load data from CSV file
df_hawasli_raw <- read.csv(file = "generalsurgery/Hawasli2021.csv")

# clean dataset of time
df_hawasli <- df_hawasli_raw %>% 
  dplyr::arrange(age) %>% 
  dplyr::mutate(
    id = factor(row_number()),
    bmi_perc_loss = as.double(str_remove(bmi_perc_loss, "%$"))/100,
    bmi_loss_calc = (bmi_pre_sg - bmi_pre_lltc) / bmi_pre_sg,
    recur_reflux = factor(ifelse(ps_reflux_postop > 0L, 2, 1), levels = 1:2, labels = c("Resolved", "Recurred"))
    ) %>% glimpse

# BMI Change
p_bmi_loss <- df_hawasli %>% 
  ggplot(aes(x = age, shape = recur_reflux)) +
  theme_cowplot() +
  geom_point(aes(y = bmi_pre_sg)) +
  geom_segment(aes(xend = age, y = bmi_pre_sg, yend = bmi_pre_lltc), arrow = arrow(), color = "red", linetype = 3, size = 0.6) +
  geom_point(aes(y = bmi_pre_lltc), color = "red") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0,60,10)) +
  scale_x_continuous(limits = c(35, 70), breaks = seq(40, 70, 10)) +
  theme(legend.position = "bottom") +
  ggrepel::geom_label_repel(aes(y = bmi_pre_lltc, label = scales::percent(bmi_perc_loss, accuracy = 1))) +
  labs(caption = "Labels show excess BMI lost\nAverage reflux duration 39 ± 37 months", shape = "Reflux") +
  ggtitle("Women with Decreasing BMI from Sleeve Gastrectomy\nto Laparoscopic Ligamentum Teres Cardiopexy") +
  xlab("\nAge (Years)") +
  ylab("BMI (kg/m2)")
save_png("./generalsurgery/gs_bmiloss.png", p_bmi_loss, dim_w = 10, dim_h = 7)

# Personal Score of Refulx
df_hawasli_long <- df_hawasli %>% 
  pivot_longer(cols = c("ps_reflux_preop", "ps_reflux_postop"), names_prefix = "ps_reflux_", names_to = "operation", values_to = "ps_reflux") %>% 
  dplyr::mutate(
    dur_reflux_mon = ifelse(operation == "preop", 0, dur_refulx_mon)
    ) 
p_reflux <- df_hawasli_long %>% 
  ggplot(aes(x = dur_reflux_mon, y = ps_reflux, group = id, linetype = recur_reflux)) +
  theme_cowplot() +
  geom_line() +
  ggtitle("Womens' Self-Reported Reflux Before and After\nLaparoscopic Ligamentum Teres Cardiopexy") +
  xlab("\nDuration of Reflux Monitoring (Months)") +
  ylab("Personal Score of Reflux") +
  labs(linetype = "Reflux") +
  theme(legend.position = "bottom")
save_png("./generalsurgery/gs_refluxrecurrence.png", p_reflux, dim_w = 10, dim_h = 7)
