library(tidyverse)
source("./programs/fun_save_png.R")
source("./programs/format_sex.R")
source("./programs/fun_plot.R")


# blood hematocrit
df_hmcrt <- data.frame(
  conc_l = c(36, 41),
  conc_u = c(46, 53),
  sex = form_sex
)
p_hmcrt <- df_hmcrt %>% 
  plot_errorbar(xvar = sex, color = sex, label_suffix = "%") +
  scale_color_manual(values = color_sex) +
  ylab("Blood Concentration (%)") +
  ylim(c(33, 55)) +
  xlab("") +
  ggtitle("Blood Hematocrit")
save_png("normal_labs/BloodHematocrit.png", p_hmcrt)


# blood hemoglobin
df_hmgbn <- data.frame(
  conc_l = c(12.0, 13.5),
  conc_u = c(16.0, 17.5),
  sex = form_sex
)
p_hmgbn <- df_hmgbn %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Blood Concentration (g/dL)") +
  ylim(c(10, 20)) +
  xlab("") +
  ggtitle("Blood Hemoglobin")
save_png("normal_labs/BloodHemoglobin.png", p_hmgbn)


# blood erythrocytes
df_eryth <- data.frame(
  conc_l = c(3.5, 4.3),
  conc_u = c(5.5, 5.9),
  sex = form_sex
)
p_eryth <- df_eryth %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Blood Count (million cells / mm^3)") +
  ylim(c(3,6)) +
  xlab("") +
  ggtitle("Blood Erythrocytes")
save_png("normal_labs/BloodErythrocyte.png", p_eryth)


# blood Erythrocyte sedimentation rate
df_erythsed <- data.frame(
  conc_l = c(0, 0),
  conc_u = c(20, 15),
  sex = form_sex
)
p_erythsed <- df_erythsed %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Sedimentation Rate (mm/hr)") +
  ylim(c(0,20)) +
  xlab("") +
  ggtitle("Blood Erythrocyte Sedimentation Rate (ESR)")
save_png("normal_labs/BloodErythrocyteSedimentationRate.png", p_erythsed)


# blood leukocytes
df_leukocytes <- data.frame(
  conc_l = c(54, 3, 1, 0, 25, 3),
  conc_u = c(62, 5, 3, 0.75, 33, 7),
  cell_type = factor(x = 1:6, levels = 1:6, labels = c("Neutrophils, Segmented", "Neutrophils, Bands", "Eosinophils", "Basophils", "Lymphocytes", "Monocytes"))
)
p_leukocytes <- df_leukocytes %>% 
  plot_errorbar(xvar = cell_type, color = cell_type, label_suffix = "%") +
  theme(axis.text.x = element_text(size = 7)) +
  ylab("Leukocyte Population (%)") +
  ylim(c(0, 63)) +
  xlab("") +
  ggtitle("Blood Leukocytes")
save_png("normal_labs/BloodLeukocytes.png", p_leukocytes)


# bleeding time
df_bleed_time <- data.frame(
  conc_l = c(25, 11),
  conc_u = c(40, 15),
  rate = factor(1:2, 1:2, labels = c("Partial Thromboplastin Time (PTT), Activated", "Prothrombin Time (PT)"))
)
p_bleed_time <- df_bleed_time %>% 
  plot_errorbar(xvar = rate) +
  scale_color_manual(values = color_sex) +
  ylab("Bleeding Time (Seconds)") +
  ylim(c(10, 40)) +
  xlab("") +
  ggtitle("Bleeding Time")
save_png("normal_labs/BloodBleedingTime.png", p_bleed_time)


# blood plasma volume
df_blood_vol <- data.frame(
  conc_l = c(28, 25, 19, 20),
  conc_u = c(45, 43, 31, 36),
  sex = rep(form_sex, 2),
  blood = factor(c(1,1,2,2), levels = 1:2, labels = c("Plasma", "RBC"))
)
p_blood_vol <- df_blood_vol %>% 
  plot_errorbar(xvar = blood, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Blood Density (mL/kg)") +
  ylim(c(19, 50)) +
  xlab("") +
  ggtitle("Blood Volume")
save_png("normal_labs/BloodVolume.png", p_blood_vol)
