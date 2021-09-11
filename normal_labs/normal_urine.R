library(tidyverse)
source("./programs/fun_save_png.R")
source("./programs/format_sex.R")
source("./programs/fun_plot.R")


# creatine clearance
df_creatine_clear <- data.frame(
  conc_l = c(88, 97),
  conc_u = c(128, 137),
  sex = form_sex
)
p_creatine_clear <- df_creatine_clear %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Flow Rate (mL/min)") +
  ylim(c(80, 140)) +
  xlab("") +
  ggtitle("Urine Creatine Clearance")
save_png("normal_labs/UrineCreatineClearance.png", p_creatine_clear)


# 17-Hydroxycorticosteroids
df_17_hydroxycorticosteroids <- data.frame(
  conc_l = c(2.0, 3.0),
  conc_u = c(8.0, 10.0),
  sex = form_sex
)
p_17_hydroxycorticosteroids <- df_17_hydroxycorticosteroids %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Urine Concentration (mg / 24 hr)") +
  ylim(c(0, 10)) +
  xlab("") +
  ggtitle("Urine 17-Hydroxycorticosteroids")
save_png("normal_labs/Urine17Hydroxycorticosteroids.png", p_17_hydroxycorticosteroids)


# 17-Ketosteroids, Total
df_17_ketosteroids <- data.frame(
  conc_l = c(6, 8),
  conc_u = c(15, 20),
  sex = form_sex
)
p_17_ketosteroids <- df_17_ketosteroids %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Urine Concentration (mg / 24 hr)") +
  ylim(c(0, 21)) +
  xlab("") +
  ggtitle("Urine 17-Ketosteroids, Total")
save_png("normal_labs/Urine17Ketosteroids.png", p_17_ketosteroids)
