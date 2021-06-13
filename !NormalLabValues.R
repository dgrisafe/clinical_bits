library(tidyverse)
library(cowplot)

# sex format
form_sex <- factor(x = 1:2, levels = 1:2, labels = c("female", "male"))
color_sex <- list(female = "#ff7f99", male = "#66adff")

# save as png files in consistent format
save_png <- function(file_name, plot, dim_res = 300, dim_w = 7, dim_h = 5, dim_units = "in"){
  png(file_name, res = dim_res, width = dim_w, height = dim_h, units = dim_units)
  print(plot)
  dev.off()
}



# blood hematocrit
df_hmcrt <- data.frame(
  conc_l = c(36, 41),
  conc_u = c(46, 53),
  sex = form_sex
)
p_hmcrt <- df_hmcrt %>% 
  ggplot(aes(x = sex, color = sex), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=paste0(conc_l, "%")), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=paste0(conc_u, "%")), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Blood Concentration (%)") +
  ylim(c(33, 55)) +
  xlab("") +
  ggtitle("Hematocrit") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("Hematocrit.png", p_hmcrt)


# blood hemoglobin
df_hmgbn <- data.frame(
  conc_l = c(12.0, 13.5),
  conc_u = c(16.0, 17.5),
  sex = form_sex
)
p_hmgbn <- df_hmgbn %>% 
  ggplot(aes(x = sex, color = sex), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Blood Concentration (g/dL)") +
  ylim(c(10, 20)) +
  xlab("") +
  ggtitle("Hemoglobin") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("Hemoglobin.png", p_hmgbn)


# blood erythrocytes
df_eryth <- data.frame(
  conc_l = c(3.5, 4.3),
  conc_u = c(5.5, 5.9),
  sex = form_sex
)
p_eryth <- df_eryth %>% 
  ggplot(aes(x = sex, color = sex), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Blood Count (million cells / mm^3)") +
  ylim(c(3,6)) +
  xlab("") +
  ggtitle("Erythrocytes") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("Erythrocyte.png", p_eryth)


# blood Erythrocyte sedimentation rate
df_erythsed <- data.frame(
  conc_l = c(0, 0),
  conc_u = c(20, 15),
  sex = form_sex
)
p_erythsed <- df_erythsed %>% 
  ggplot(aes(x = sex, color = sex), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Sedimentation Rate (mm/hr)") +
  ylim(c(0,20)) +
  xlab("") +
  ggtitle("Erythrocyte Sedimentation Rate (ESR)") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("ErythrocyteSedimentationRate.png", p_erythsed)



# Serum Cortisol
df_cortisol <- data.frame(
  conc_l = c(5, 3, 0),
  conc_u = c(23, 15, 11.5),
  time = c(0800, 1600, 2000)
)
p_cortisol <- df_cortisol %>% 
  ggplot(aes(x = time), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Serum Cortisol (µg/dL)") +
  ylim(c(0,25)) +
  xlab("24-Hour Day") +
  scale_x_continuous(limits = c(0000,2400), breaks = c(0000,0800, 1600, 2000,2400)) +
  labs(caption = "Cortisol at 2000 ≤ 50% of 0800 Level") +
  ggtitle("Serum Cortisol") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("SerumCortisol.png", p_cortisol)


# Serum Bilirubin
df_bilirubin <- data.frame(
  conc_l = c(0.1, 0),
  conc_u = c(1.0, 0.3),
  bilirubin = factor(x = 1:2, levels = 1:2, labels = c("Total", "Direct (Conjugated)"))
)
p_bilirubin <- df_bilirubin %>% 
  ggplot(aes(x = bilirubin), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Serum Concentration (mg/dL)") +
  ylim(c(0,1)) +
  xlab("Bilirubin") +
  ggtitle("Serum Bilirubin") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("SerumBilirubin.png", p_bilirubin)


# Serum Iron
df_iron <- data.frame(
  conc_l = c(50, 65),
  conc_u = c(170, 175),
  sex = form_sex
)
p_iron <- df_iron %>% 
  ggplot(aes(x = sex, color = sex), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Serum Iron (µg/dL)") +
  ylim(c(0,180)) +
  xlab("") +
  ggtitle("Serum Iron") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("SerumIron.png", p_iron)


# serum ferritin
df_ferritin <- data.frame(
  conc_l = c(10, 20),
  conc_u = c(120, 250),
  sex = form_sex
)
p_ferritin <- df_ferritin %>% 
  ggplot(aes(x = sex, color = sex), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Serum Concentration (ng/mL)") +
  ylim(c(0,250)) +
  xlab("") +
  ggtitle("Serum Ferritin") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("SerumFerritin.png", p_ferritin)


# reproductive hormones
df_hor_repro <- data.frame(
  conc_l = c(4, 4, 10, 40, 6, 5, 75, 30),
  conc_u = c(25, 30, 90, 250, 23, 30, 150, 200),
  time = factor(x = rep(1:4, 2), levels = 1:4, labels = c("male", "female follicular", "female midcycle", "female postmenopause")),
  hormone = factor(x = rep(1:2, each = 4), levels = 1:2, labels = c("Follicle-Stimulating Hormone", "Luteinizing Hormone"))
)
p_hor_repro <- df_hor_repro %>% 
  ggplot(aes(x = time, color = hormone), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = c("#e9dc3d", "#3ab141")) +
  ylab("Serum Concentration (mIU/mL)") +
  xlab("") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("Reproductive_Hormones.png", p_hor_repro)


# prolactin hormone
df_prolactin <- data.frame(
  conc_l = c(0, 0),
  conc_u = c(25, 17),
  sex = form_sex
)
p_prolactin <- df_prolactin %>% 
  ggplot(aes(x = sex, color = sex), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = color_sex) +
  ylab("Serum Concentration (ng/mL)") +
  ylim(c(0,25)) +
  xlab("") +
  ggtitle("Serum Prolactin") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("Serum_Hormone_Prolactin.png", p_prolactin)


# Serum Thyroid Hormones
df_thyroid <- data.frame(
  conc_l = c(5, 0.9, 0.1),
  conc_u = c(12, 1.7, 0.2),
  hormone = factor(x = 1:3, levels = 1:3, labels = c("Thyroxine (T4)", "Free Thyroxine (T4)", "Triiodothyronine (T3)"))
)
p_thyroid <- df_thyroid %>% 
  ggplot(aes(x = hormone), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  ylab("Serum Concentration (µg/dL)") +
  ylim(c(0, 12)) +
  xlab("") +
  ggtitle("Thyroid Hormone") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 
save_png("ThyroidHormone.png", p_thyroid)
